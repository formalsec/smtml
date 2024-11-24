let pp_exit_status fmt = function
  | Ok () -> Fmt.pf fmt "Exited 0"
  | Error (`Exit_non_zero n) -> Fmt.pf fmt "Exited %d" n
  | Error (`Signal s) -> Fmt.pf fmt "Signal %s" (Core.Signal.to_string s)

let parse_stdout =
  let re = Dune_re.(compile @@ Perl.re {|Run\s(.*?)\sin\s([0-9.]+)|}) in
  fun stdout ->
    let matches = Dune_re.all re stdout in
    List.map
      (fun group ->
        let benchmark = Dune_re.Group.get group 1 in
        let rtime = Dune_re.Group.get group 2 in
        (benchmark, Float.of_string rtime) )
      matches

let parse_results (prover, (_status, stdout, _stderr, _rusage)) =
  let results = parse_stdout stdout in
  let results =
    List.map
      (fun (benchmark, rtime) ->
        (Tool.prover_to_string prover, benchmark, rtime) )
      results
  in
  (prover, results)

let summary results =
  List.iter
    (fun (prover, results) ->
      let total, rtime =
        List.fold_left
          (fun (total, time) (_, _, rtime) -> (succ total, time +. rtime))
          (0, 0.) results
      in
      Fmt.pr "Ran %d benchmarks on %a in %0.03f" total Tool.pp_prover prover
        rtime )
    results

let make_data_frames results =
  List.map
    (fun (prover, prover_results) ->
      let provers, benchmark, rtime =
        List.fold_left
          (fun (prover_acc, bench_acc, rtime_acc) (prover, benchmark, rtime) ->
            (prover :: prover_acc, benchmark :: bench_acc, rtime :: rtime_acc)
            )
          ([], [], []) prover_results
      in
      let df =
        Owl_dataframe.make
          [| "prover"; "benchmark"; "rtime" |]
          ~data:
            [| Owl_dataframe.pack_string_series @@ Array.of_list provers
             ; Owl_dataframe.pack_string_series @@ Array.of_list benchmark
             ; Owl_dataframe.pack_float_series @@ Array.of_list rtime
            |]
      in
      (prover, df) )
    results

let write_data_frame started_at results_dir (prover, df) =
  let csv_file = Fmt.str "%a-%s.csv" Tool.pp_prover prover started_at in
  let csv_path = Fpath.(results_dir / csv_file) in
  Owl_dataframe.to_csv ~sep:',' df (Fpath.to_string csv_path)

let main _hook provers from_file dirs =
  let open Result in
  let now = Core_unix.(localtime @@ gettimeofday ()) in
  let started_at = Core_unix.strftime now "%Y%m%dT%H%M%S" in
  assert (List.for_all Tool.is_available provers);
  List.iter
    (fun (p : Tool.prover) -> match p with Z3 -> () | Smtml p -> p.st <- true)
    provers;
  let dirs = List.map Fpath.to_string dirs in
  let results =
    List.map
      (fun p ->
        let ((status, stdout, stderr, _) as result) =
          Tool.fork_and_run ?from_file p dirs
        in
        Fmt.pr "@[<v 2>Run %a@;Exited: %a@;Stdout:@; %a@;Stderr:@; %a@]@." Tool.pp_prover p
          pp_exit_status status Fmt.string (String.escaped stdout) Fmt.string (String.escaped stderr);
        (p, result) )
      provers
  in
  let results = List.map parse_results results in
  summary results;
  let data_frames = make_data_frames results in
  let results_dir = Fmt.kstr Fpath.v "res-multi-query-%s" started_at in
  let* _ = Bos.OS.Dir.create ~path:true results_dir in
  List.iter (write_data_frame started_at results_dir) data_frames;
  (* Option.iter (Notify.notify_done msg) hook; *)
  Ok ()

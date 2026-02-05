let files_to_run d =
  match
    Bos.OS.Dir.fold_contents ~traverse:`Any
      (fun path acc -> if Fpath.has_ext ".smt2" path then path :: acc else acc)
      [] d
  with
  | Ok results -> results
  | Error (`Msg err) -> Fmt.failwith "%s" err

let parse_status =
  let re = Re.(compile @@ Perl.re {|^(sat|unsat|unknown)|}) in
  fun stdout ->
    match Re.exec_opt re stdout with
    | None -> `Unknown
    | Some group -> (
      match Re.Group.get group 1 with
      | "sat" -> `Sat
      | "unsat" -> `Unsat
      | "unknown" -> `Unknown
      | _ -> assert false )

let pp_status fmt = function
  | `Sat -> Fmt.string fmt "sat"
  | `Unsat -> Fmt.string fmt "unsat"
  | `Unknown -> Fmt.string fmt "unknown"

let summarize results =
  let results_list =
    List.map
      (fun (prover, prover_results) ->
        ( prover
        , List.fold_left
            (fun (total, sat, unsat, unknown, time)
                 (_, _, stdout, _, rtime, _rusage) ->
              let sat, unsat, unknown =
                match parse_status stdout with
                | `Sat -> (succ sat, unsat, unknown)
                | `Unsat -> (sat, succ unsat, unknown)
                | `Unknown -> (sat, unsat, succ unknown)
              in
              (succ total, sat, unsat, unknown, time +. rtime) )
            (0, 0, 0, 0, 0.) prover_results ) )
      results
  in
  (* let solver, total, sat, unsat, unknwon, rtime = *)
  let _ =
    List.fold_left
      (fun (solver, total, sat, unsat, unknown, rtime)
           (prover, (p_total, p_sat, p_unsat, p_unknown, p_rtime)) ->
        ( Tool.prover_to_string prover :: solver
        , p_total :: total
        , p_sat :: sat
        , p_unsat :: unsat
        , p_unknown :: unknown
        , p_rtime :: rtime ) )
      ([], [], [], [], [], []) results_list
  in
  (* Owl_dataframe.make *)
  (*   [| "solver"; "total"; "sat"; "unsat"; "unknown"; "rtime" |] *)
  (*   ~data: *)
  (*     [| Owl_dataframe.pack_string_series @@ Array.of_list solver *)
  (*      ; Owl_dataframe.pack_int_series @@ Array.of_list total *)
  (*      ; Owl_dataframe.pack_int_series @@ Array.of_list sat *)
  (*      ; Owl_dataframe.pack_int_series @@ Array.of_list unsat *)
  (*      ; Owl_dataframe.pack_int_series @@ Array.of_list unknwon *)
  (*      ; Owl_dataframe.pack_float_series @@ Array.of_list rtime *)
  (*     |] *)
  assert false

(* Maybe we can clean this up later *)
(* let make_data_frames results = *)
(*   List.map *)
(*     (fun (prover, prover_results) -> *)
(*       let provers, benchmark, res, stdout, stderr, rtime, utime, stime, maxrss = *)
(*         List.fold_left *)
(*           (fun ( prover_acc *)
(*                , bench_acc *)
(*                , res_acc *)
(*                , stdout_acc *)
(*                , stderr_acc *)
(*                , rtime_acc *)
(*                , utime_acc *)
(*                , stime_acc *)
(*                , maxrss_acc ) *)
(*                (_status, benchmark, stdout, stderr, rtime, rusage) -> *)
(*             ( Tool.prover_to_string prover :: prover_acc *)
(*             , Fmt.str "%a" Fpath.pp benchmark :: bench_acc *)
(*             , Fmt.str "%a" pp_status (parse_status stdout) :: res_acc *)
(*             , String.escaped stdout :: stdout_acc *)
(*             , String.escaped stderr :: stderr_acc *)
(*             , rtime :: rtime_acc *)
(*             , rusage.ExtUnix.Specific.ru_utime :: utime_acc *)
(*             , rusage.ExtUnix.Specific.ru_stime :: stime_acc *)
(*             , rusage.ExtUnix.Specific.ru_maxrss :: maxrss_acc ) ) *)
(*           ([], [], [], [], [], [], [], [], []) *)
(*           prover_results *)
(*       in *)
(*       let df = *)
(*         Owl_dataframe.make *)
(*           [| "prover" *)
(*            ; "benchmark" *)
(*            ; "res" *)
(*            ; "stdout" *)
(*            ; "stderr" *)
(*            ; "rtime" *)
(*            ; "utime" *)
(*            ; "stime" *)
(*            ; "maxrss" *)
(*           |] *)
(*           ~data: *)
(*             [| Owl_dataframe.pack_string_series @@ Array.of_list provers *)
(*              ; Owl_dataframe.pack_string_series @@ Array.of_list benchmark *)
(*              ; Owl_dataframe.pack_string_series @@ Array.of_list res *)
(*              ; Owl_dataframe.pack_string_series @@ Array.of_list stdout *)
(*              ; Owl_dataframe.pack_string_series @@ Array.of_list stderr *)
(*              ; Owl_dataframe.pack_float_series @@ Array.of_list rtime *)
(*              ; Owl_dataframe.pack_float_series @@ Array.of_list utime *)
(*              ; Owl_dataframe.pack_float_series @@ Array.of_list stime *)
(*              ; Owl_dataframe.pack_int_series *)
(*                @@ Array.of_list (List.map Int64.to_int maxrss) *)
(*             |] *)
(*       in *)
(*       (prover, df) ) *)
(*     results *)

let write_data_frame started_at results_dir (prover, _df) =
  let csv_file = Fmt.str "%a-%s.csv" Tool.pp_prover prover started_at in
  let _csv_path = Fpath.(results_dir / csv_file) in
  (* Owl_dataframe.to_csv ~sep:',' df (Fpath.to_string csv_path) *)
  assert false

let main ~hook:_ ~timeout ~provers ~dirs =
  let open Result in
  let start_time = Unix.gettimeofday () in
  let now = Unix.(localtime start_time) in
  let _started_at = ExtUnix.Specific.strftime "%Y%m%dT%H%M%S" now in
  assert (List.for_all Tool.is_available provers);
  let files = List.concat_map files_to_run dirs in
  let provers_str = List.map Tool.prover_to_string provers in
  let w_prover = List.fold_left max 0 (List.map String.length provers_str) in
  let num_files = List.length files in
  let num_provers = List.length provers in
  let run_provers ?timeout provers run_id benchmark =
    List.map
      (fun prover ->
        let start = Unix.gettimeofday () in
        let status, stdout, stderr, rusage =
          Tool.fork_and_run ?timeout prover [ Fpath.to_string benchmark ]
        in
        let rtime = Unix.gettimeofday () -. start in
        let prover = Fmt.str "%a" Tool.pp_prover prover in
        Fmt.pr
          "@[<v 2>%-*s: [%05d/%05d]@;\
           File: %a@;\
           Exited: %a@;\
           Result: %s@;\
           Time  : %0.03f@]@."
          w_prover prover run_id num_files Fpath.pp benchmark
          Util.pp_exit_status status (String.trim stdout) rtime;
        (status, benchmark, stdout, stderr, rtime, rusage) )
      provers
  in
  (* For every benchmark we run the selected provers *)
  let results =
    let run_id = ref 0 in
    let init = List.init num_provers (fun _i -> []) in
    List.fold_left
      (fun acc file ->
        incr run_id;
        let res = run_provers ?timeout provers !run_id file in
        List.map2 (fun res acc -> res :: acc) res acc )
      init files
  in
  let results = List.map2 (fun p results -> (p, results)) provers results in
  let summary =
    List.map
      (fun (prover, results) ->
        ( prover
        , List.fold_left
            (fun acc (status, _, _, _, _, _) ->
              match status with Unix.WEXITED 0 -> succ acc | _ -> acc )
            0 results ) )
      results
  in
  let time_elapsed = Unix.gettimeofday () -. start_time in
  List.iter
    (fun (prover, passed) ->
      let percent = float passed /. float num_files *. 100. in
      Fmt.pr "@[<v 2>%-*s:@;Passed: %05d/%05d (%03.2f%%)@." w_prover
        (Tool.prover_to_string prover)
        passed num_files percent )
    summary;
  Fmt.pr "Time elasped: %.2f@." time_elapsed;
  (* let data_frames = make_data_frames results in *)
  (* let summary = summarize results in *)
  (* let msg = *)
  (*   Fmt.str "@[Single-Query Results:@;%a@]@." Owl_pretty.pp_dataframe summary *)
  (* in *)
  (* let results_dir = Fmt.kstr Fpath.v "res-single-query-%s" started_at in *)
  (* let* _ = Bos.OS.Dir.create ~path:true results_dir in *)
  (* List.iter (write_data_frame started_at results_dir) data_frames; *)
  (* Option.iter (Notify.notify_done msg) hook; *)
  (* Ok (Fmt.pr "%s" msg) *)
  Ok ()

open Smtml

let get_solver debug solver_type solver_mode =
  let module Mappings : Mappings.S_with_fresh =
    (val Solver_type.to_mappings solver_type)
  in
  Mappings.set_debug debug;
  match solver_mode with
  | Solver_mode.Batch -> (module Solver.Batch (Mappings) : Solver.S)
  | Cached -> (module Solver.Cached (Mappings))
  | Incremental -> (module Solver.Incremental (Mappings))

(* FIXME: this function has a sad name *)
let parse_file filename =
  let open Smtml_prelude.Result in
  let+ lines = Bos.OS.File.read_lines filename in
  (* FIXME: this can be improved *)
  let files =
    List.fold_left
      (fun acc line ->
        let line = String.trim line in
        (* Assume '#' at the start of a line is a comment *)
        if String.starts_with ~prefix:"#" line then acc else Fpath.v line :: acc )
      [] lines
  in
  List.rev files

exception Timeout

let init_arcl_eval = function
  | 1 -> Rewrite.Symb_normalizer.symb_renaming := true
  | 2 -> Solver.slice_algorithm := true
  | 3 -> Solver.unsat_cache := true
  | 4 ->
    Solver.unsat_cache := true;
    Solver.unsat_plus := true
  | 5 ->
    Rewrite.Symb_normalizer.symb_renaming := true;
    Solver.slice_algorithm := true
  | 6 ->
    Rewrite.Symb_normalizer.symb_renaming := true;
    Solver.unsat_cache := true
  | 7 ->
    Rewrite.Symb_normalizer.symb_renaming := true;
    Solver.unsat_cache := true;
    Solver.unsat_plus := true
  | 8 ->
    Rewrite.Symb_normalizer.symb_renaming := true;
    Solver.slice_algorithm := true;
    Solver.unsat_cache := true
  | 9 ->
    Rewrite.Symb_normalizer.symb_renaming := true;
    Solver.slice_algorithm := true;
    Solver.unsat_cache := true;
    Solver.unsat_plus := true
  | _ -> ()

let run ~debug ~dry ~print_statistics ~no_strict_status ~no_simpls ~timeout
  ~solver_type ~solver_mode ~from_file ~arcl_eval ~filenames =
  init_arcl_eval arcl_eval;
  if debug then Logs.Src.set_level Log.src (Some Logs.Debug);
  Logs.set_reporter @@ Logs.format_reporter ();
  let module Solver = (val get_solver debug solver_type solver_mode) in
  let module Interpret = Interpret.Make (Solver) in
  let total_tests = ref 0 in
  let total_t = ref 0. in
  let exception_log = ref [] in
  let exception_count = ref 0 in
  let run_file state file =
    Log.debug (fun k -> k "File %a..." Fpath.pp file);
    incr total_tests;
    let start_t = Unix.gettimeofday () in
    Fun.protect ~finally:(fun () ->
      if print_statistics then (
        let exec_t = Unix.gettimeofday () -. start_t in
        total_t := !total_t +. exec_t;
        Log.err (fun m -> m "Run %a in %.06f" Fpath.pp file exec_t) ) )
    @@ fun () ->
    try
      let ast =
        try Ok (Compile.until_rewrite file ~no_simpls)
        with Parse.Syntax_error err -> Error (`Parsing_error (file, err))
      in
      match ast with
      | Ok _ when dry -> state
      | Ok ast ->
        let old_handler = ref Sys.Signal_default in
        let timeout_sec = float_of_int timeout /. 1000. in
        Fun.protect ~finally:(fun () ->
          ignore
            (Unix.setitimer Unix.ITIMER_REAL
               { it_value = 0.; it_interval = 0. } );
          ignore (Sys.signal Sys.sigalrm !old_handler) )
        @@ fun () ->
        old_handler :=
          Sys.signal Sys.sigalrm (Sys.Signal_handle (fun _ -> raise Timeout));
        if timeout_sec > 0. then
          ignore
            (Unix.setitimer Unix.ITIMER_REAL
               { it_value = timeout_sec; it_interval = 0. } );
        Some (Interpret.start ?state ast ~no_strict_status ~timeout)
      | Error (`Parsing_error ((fpath, err_msg) as err)) ->
        Log.err (fun k -> k "%a: %s" Fpath.pp fpath err_msg);
        incr exception_count;
        exception_log := err :: !exception_log;
        state
    with
    | Timeout ->
      let err_msg =
        Printf.sprintf "Timeout during interpretation after %d ms" timeout
      in
      Log.app (fun k -> k "File %a: %s" Fpath.pp file err_msg);
      incr exception_count;
      exception_log := (file, err_msg) :: !exception_log;
      state
    | ex ->
      let err_msg = Printexc.to_string ex in
      Log.err (fun k ->
        k "Unhandled exception processing file %a: %s" Fpath.pp file err_msg );
      incr exception_count;
      exception_log := (file, err_msg) :: !exception_log;
      state
  in

  let run_dir prev_state d =
    let result =
      Bos.OS.Dir.fold_contents ~traverse:`Any
        (fun path state ->
          if Fpath.has_ext ".smt2" path then run_file state path else state )
        prev_state d
    in
    match result with
    | Error (`Msg e) ->
      let err_msg = "Error traversing directory: " ^ e in
      Log.err (fun k -> k "%a: %s" Fpath.pp d err_msg);
      incr exception_count;
      exception_log := (d, err_msg) :: !exception_log;
      prev_state
    | Ok state -> state
  in
  let run_path prev_state path =
    match Fpath.to_string path with
    | "-" -> run_file prev_state path
    | _ -> (
      match Bos.OS.Path.exists path with
      | Ok false ->
        Log.warn (fun k -> k "%a: No such file or directory" Fpath.pp path);
        prev_state
      | Ok true -> (
        try
          if Sys.is_directory (Fpath.to_string path) then
            run_dir prev_state path
          else run_file prev_state path
        with ex ->
          let err_msg = Printexc.to_string ex in
          Log.err (fun k ->
            k "Error processing path %a: %s" Fpath.pp path err_msg );
          incr exception_count;
          exception_log := (path, err_msg) :: !exception_log;
          prev_state )
      | Error (`Msg err) ->
        Log.err (fun k -> k "%s" err);
        prev_state )
  in
  let state =
    match from_file with
    | None -> List.fold_left run_path None filenames
    | Some file -> (
      match parse_file file with
      | Error (`Msg err) ->
        Log.err (fun k -> k "%s" err);
        None
      | Ok files -> List.fold_left run_file None files )
  in
  if print_statistics then (
    let ( cache_hits
        , cache_misses
        , slice_count
        , slice_factor
        , slice_ts
        , unsat_caches
        , unsat_hits
        , unsat_misses
        , unsat_plus
        , unsat_cache_ts
        , unsat_plus_ts ) =
      match state with
      | None -> (0, 0, 0, 0, 0.0, 0, 0, 0, 0, 0.0, 0.0)
      | Some state' -> (
        let stats = Solver.get_statistics state'.solver in
        let cache_hits = Statistics.Map.find_opt "cache hits" stats in
        let cache_misses = Statistics.Map.find_opt "cache misses" stats in
        let slice_count = Statistics.Map.find_opt "slice count" stats in
        let slice_factor = Statistics.Map.find_opt "slice factor" stats in
        let slice_ts = Statistics.Map.find_opt "slice ts" stats in
        let unsat_caches = Statistics.Map.find_opt "unsat caches" stats in
        let unsat_hits = Statistics.Map.find_opt "unsat hits" stats in
        let unsat_misses = Statistics.Map.find_opt "unsat misses" stats in
        let unsat_plus = Statistics.Map.find_opt "unsat plus" stats in
        let unsat_cache_ts = Statistics.Map.find_opt "unsat cache ts" stats in
        let unsat_plus_ts = Statistics.Map.find_opt "unsat plus ts" stats in
        match
          ( cache_hits
          , cache_misses
          , slice_count
          , slice_factor
          , slice_ts
          , unsat_caches
          , unsat_hits
          , unsat_misses
          , unsat_plus
          , unsat_cache_ts
          , unsat_plus_ts )
        with
        | ( Some (`Int cache_hits')
          , Some (`Int cache_misses')
          , Some (`Int slice_count')
          , Some (`Int slice_factor')
          , Some (`Float slice_ts')
          , Some (`Int unsat_caches')
          , Some (`Int unsat_hits')
          , Some (`Int unsat_misses')
          , Some (`Int unsat_plus')
          , Some (`Float unsat_cache_ts')
          , Some (`Float unsat_plus_ts') ) ->
          ( cache_hits'
          , cache_misses'
          , slice_count'
          , slice_factor'
          , slice_ts'
          , unsat_caches'
          , unsat_hits'
          , unsat_misses'
          , unsat_plus'
          , unsat_cache_ts'
          , unsat_plus_ts' )
        | _ -> (0, 0, 0, 0, 0.0, 0, 0, 0, 0, 0.0, 0.0) )
    in
    Log.err (fun k -> k "");
    Log.err (fun k -> k "total time: %.06f" !total_t);
    Log.err (fun k -> k "cache hits: %d" cache_hits);
    Log.err (fun k -> k "cache misses: %d" cache_misses);
    Log.err (fun k -> k "slice total: %d" slice_count);
    Log.err (fun k -> k "slice factor: %d" slice_factor);
    Log.err (fun k -> k "slice time: %f" slice_ts);
    Log.err (fun k -> k "unsat caches: %d" unsat_caches);
    Log.err (fun k -> k "unsat hits: %d" unsat_hits);
    Log.err (fun k -> k "unsat misses: %d" unsat_misses);
    Log.err (fun k -> k "unsat plus: %d" unsat_plus);
    Log.err (fun k -> k "unsat cache time: %f" unsat_cache_ts);
    Log.err (fun k -> k "unsat plus time: %f" unsat_plus_ts) );
  let write_exception_log =
    let open Smtml_prelude.Result in
    function
    | [] -> Ok None
    | exns ->
      let total = !total_tests in
      let exceptions = !exception_count in
      let percentage =
        if total = 0 then 0.0 else float exceptions /. float total *. 100.0
      in
      let log_file = Fpath.v (Filename.temp_file "smtml" "exceptions.sexp") in
      let* () =
        Bos.OS.File.writef log_file
          "@[<v 1>((total-tests %d)@;\
           (exceptions %d)@;\
           (exception-percentage %.2f%%)@;\
           @[<v 1>(errors (@;\
           %a@;\
           ))@]@;\
           )@]"
          total exceptions percentage
          (Fmt.list ~sep:Fmt.cut (fun fmt (fpath, err_msg) ->
             Fmt.pf fmt "@[<hov 1>((file \"%a\")@;(error %S))@]" Fpath.pp fpath
               err_msg ) )
          exns
      in
      Ok (Some log_file)
  in
  match write_exception_log !exception_log with
  | Error (`Msg err) ->
    Log.warn (fun k -> k "Could not write excptions log: %s" err)
  | Ok None -> ()
  | Ok (Some _) ->
    Log.app (fun k ->
      k "Exception log successfully written to temporary file." )

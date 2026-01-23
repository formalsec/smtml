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
  let open Smtml_prelude.Result.Syntax in
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

let run (s : Settings.Run.t) =
  (* ~debug ~dry ~print_statistics ~no_strict_status ~solver_type *)
  (*   ~solver_mode ~from_file ~filenames = *)
  if s.debug then Logs.Src.set_level Log.src (Some Logs.Debug);
  Logs.set_reporter @@ Logs.format_reporter ();
  let module Solver = (val get_solver s.debug s.solver_type s.solver_mode) in
  let module Interpret = Interpret.Make (Solver) in
  let total_tests = ref 0 in
  let total_t = ref 0. in
  let exception_log = ref [] in
  let exception_count = ref 0 in

  let run_file state file =
    Log.debug (fun k -> k "File %a..." Fpath.pp file);
    incr total_tests;
    let start_t = Unix.gettimeofday () in
    let ast =
      try Ok (Compile.until_rewrite file) with
      | Parse.Syntax_error err -> Error (`Parsing_error (file, err))
      | Smtml.Eval.Eval_error err -> Error (`Eval_error err)
    in
    let state =
      match ast with
      | Ok _ when s.dry -> state
      | Ok ast ->
        Some (Interpret.start ?state ast ~no_strict_status:s.no_strict_status)
      | Error (`Parsing_error ((fpath, err_msg) as err)) ->
        Log.err (fun k -> k "%a: %s" Fpath.pp fpath err_msg);
        incr exception_count;
        exception_log := err :: !exception_log;
        state
      | Error (`Eval_error kind) ->
        Log.err (fun k ->
          k "@[<h>%a: Eval_error: %a@]" Fpath.pp file Smtml.Eval.pp_error_kind
            kind );
        incr exception_count;
        state
    in
    if s.print_statistics then (
      let exec_t = Unix.gettimeofday () -. start_t in
      total_t := !total_t +. exec_t;
      Log.app (fun m -> m "Run %a in %.06f" Fpath.pp file exec_t) );
    state
  in

  let run_dir prev_state d =
    Bos.OS.Dir.fold_contents ~traverse:`Any
      (fun path state ->
        if Fpath.has_ext ".smt2" path then run_file state path else state )
      prev_state d
    |> Result.get_ok
  in

  let run_path prev_state path =
    match Fpath.to_string path with
    | "-" -> run_file prev_state path
    | _ -> (
      match Bos.OS.Path.exists path with
      | Ok false ->
        Log.warn (fun k -> k "%a: No such file or directory" Fpath.pp path);
        prev_state
      | Ok true ->
        if Sys.is_directory (Fpath.to_string path) then run_dir prev_state path
        else run_file prev_state path
      | Error (`Msg err) ->
        Log.err (fun k -> k "%s" err);
        prev_state )
  in

  let _ =
    match s.from_file with
    | None -> List.fold_left run_path None s.filenames
    | Some file -> (
      match parse_file file with
      | Error (`Msg err) ->
        Log.err (fun k -> k "%s" err);
        None
      | Ok files -> List.fold_left run_file None files )
  in

  if s.print_statistics then Log.app (fun k -> k "total time: %.06f" !total_t);
  let write_exception_log =
    let open Smtml_prelude.Result.Syntax in
    function
    | [] -> Ok None
    | exns ->
      let total = !total_tests in
      let exceptions = !exception_count in
      assert (total > 0);
      let percentage = float exceptions /. float total *. 100.0 in
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
  | Ok (Some fpath) ->
    Log.app (fun k ->
      k "Exception log successfully written to: %a" Fpath.pp fpath )

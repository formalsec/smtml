(***************************************************************************)
(* This file is part of the third-party OCaml library `smtml`.             *)
(* Copyright (C) 2023-2024 formalsec                                       *)
(*                                                                         *)
(* This program is free software: you can redistribute it and/or modify    *)
(* it under the terms of the GNU General Public License as published by    *)
(* the Free Software Foundation, either version 3 of the License, or       *)
(* (at your option) any later version.                                     *)
(*                                                                         *)
(* This program is distributed in the hope that it will be useful,         *)
(* but WITHOUT ANY WARRANTY; without even the implied warranty of          *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the           *)
(* GNU General Public License for more details.                            *)
(*                                                                         *)
(* You should have received a copy of the GNU General Public License       *)
(* along with this program.  If not, see <https://www.gnu.org/licenses/>.  *)
(***************************************************************************)

open Smtml
open Solver_dispatcher

let get_solver debug solver prover_mode =
  let module Mappings = (val mappings_of_solver solver : Mappings.S_with_fresh)
  in
  Mappings.set_debug debug;
  match prover_mode with
  | Options.Batch -> (module Solver.Batch (Mappings) : Solver.S)
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

let run debug solver prover_mode dry print_statistics from_file files =
  if debug then Logs.Src.set_level Log.src (Some Logs.Debug);
  Logs.set_reporter @@ Logs.format_reporter ();
  let module Solver = (val get_solver debug solver prover_mode) in
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
        Log.app (fun m -> m "Run %a in %.06f" Fpath.pp file exec_t) ) )
    @@ fun () ->
    let ast =
      try Ok (Compile.until_rewrite file)
      with Parse.Syntax_error err -> Error (`Parsing_error (file, err))
    in
    match ast with
    | Ok _ when dry -> state
    | Ok ast -> Some (Interpret.start ?state ast)
    | Error (`Parsing_error err) ->
      Log.err (fun k -> k "Error while parsing %a" Fpath.pp file);
      incr exception_count;
      exception_log := err :: !exception_log;
      state
  in
  let run_dir prev_state d =
    let result =
      Bos.OS.Dir.fold_contents ~traverse:`Any
        (fun path state ->
          if Fpath.has_ext ".smt2" path then run_file state path else state )
        prev_state d
    in
    match result with Error (`Msg e) -> failwith e | Ok state -> state
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
    match from_file with
    | None -> List.fold_left run_path None files
    | Some file -> (
      match parse_file file with
      | Error (`Msg err) ->
        Log.err (fun k -> k "%s" err);
        None
      | Ok files -> List.fold_left run_file None files )
  in
  if print_statistics then Log.app (fun k -> k "total time: %.06f" !total_t);
  let write_exception_log = function
    | [] -> Ok ()
    | exns ->
      let total = !total_tests in
      let exceptions = !exception_count in
      assert (total > 0);
      let percentage = float exceptions /. float total *. 100.0 in
      let log_fpath = Fpath.v "exceptions.log" in
      Bos.OS.File.writef log_fpath
        "Total tests: %d@\n\
         Exceptions: %d@\n\
         Exception percentage: %.2f%%@\n\
         @\n\
         %a"
        total exceptions percentage
        (Fmt.list
           ~sep:(fun fmt () -> Fmt.pf fmt "@\n@\n")
           (fun fmt (path, err) ->
             Fmt.pf fmt "File: %a@\nError: %s" Fpath.pp path err ) )
        exns
  in
  match write_exception_log !exception_log with
  | Error (`Msg err) ->
    Log.warn (fun k -> k "Could not write excptions log: %s" err)
  | Ok () -> ()

(* TODO: Remove once dolmen is integrated *)
let to_smt2 debug solver filename =
  let module Mappings =
    (val mappings_of_solver solver : Mappings_intf.S_with_fresh)
  in
  Mappings.set_debug debug;
  let ast = Parse.from_file filename in
  let assertions =
    List.filter_map (function Ast.Assert e -> Some e | _ -> None) ast
  in
  let name = Fpath.to_string @@ Fpath.base filename in
  Format.printf "%a"
    (Mappings.Smtlib.pp ~name ?logic:None ?status:None)
    assertions

let cli =
  Cmdliner.Cmd.group
    (Cmdliner.Cmd.info "smtml" ~version:"%%VERSION%%")
    [ Options.cmd_run run; Options.cmd_to_smt2 to_smt2 ]

let () =
  match Cmdliner.Cmd.eval_value cli with
  | Error (`Exn | `Parse | `Term) -> exit 2
  | Ok (`Help | `Ok () | `Version) -> ()

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
  let module Mappings =
    (val mappings_of_solver solver : Mappings_intf.S_with_fresh)
  in
  Mappings.set_debug debug;
  match prover_mode with
  | Options.Batch -> (module Solver.Batch (Mappings) : Solver_intf.S)
  | Cached -> (module Solver.Cached (Mappings))
  | Incremental -> (module Solver.Incremental (Mappings))

let run debug solver prover_mode _print_statistics file =
  let module Solver = (val get_solver debug solver prover_mode) in
  let module Interpret = Interpret.Make (Solver) in
  let ast = Compile.until_rewrite file in
  let _ : Interpret.exec_state = Interpret.start ast in
  ()

let test debug solver prover_mode print_statistics dry files =
  let module Solver = (val get_solver debug solver prover_mode) in
  let module Interpret = Interpret.Make (Solver) in
  (* TODO: Add proper logs *)
  let debug fmt k = if debug then k (Fmt.epr fmt) in
  let exception_log = ref [] in
  let total_tests = ref 0 in
  let exception_count = ref 0 in
  let rec test_path state path =
    if Sys.is_directory (Fpath.to_string path) then test_dir state path
    else begin
      debug "File %a...@." (fun k -> k Fpath.pp path);
      try
        incr total_tests;
        let ast = Compile.until_rewrite path in
        if dry then begin
          state
        end
        else begin
          Some (Interpret.start ?state ast)
        end
      with exn ->
        incr exception_count;
        let exn_msg = Printexc.to_string exn in
        exception_log := (path, exn_msg) :: !exception_log;
        debug "Error processing file %a@." (fun k -> k Fpath.pp path);
        state
    end
  and test_dir state d =
    let result =
      Bos.OS.Dir.fold_contents
        (fun path state ->
          if Fpath.has_ext ".smt2" path then test_path state path else state )
        state d
    in
    match result with Error (`Msg e) -> failwith e | Ok state -> state
  and test_files files = List.fold_left test_path None files in
  let state = test_files files in
  let write_exception_log () =
    let oc = open_out "exceptions.log" in
    let total = !total_tests in
    let exceptions = !exception_count in
    let percentage =
      if total = 0 then 0.0
      else float_of_int exceptions /. float_of_int total *. 100.0
    in
    Printf.fprintf oc "Total tests: %d\n" total;
    Printf.fprintf oc "Exceptions: %d\n" exceptions;
    Printf.fprintf oc "Exception percentage: %.2f%%\n\n" percentage;
    List.iter
      (fun (path, exn_msg) ->
        Printf.fprintf oc "File: %s\nError: %s\n\n" (Fpath.to_string path)
          exn_msg )
      (List.rev !exception_log);
    close_out oc
  in

  if dry then begin
    write_exception_log ()
  end;
  if print_statistics then begin
    let state = Option.get state in
    let stats : Gc.stat = Gc.stat () in
    Format.eprintf
      "@[<v 2>(statistics @\n\
       (major-words %f)@\n\
       (solver-time %f)@\n\
       (solver-calls %d)@\n\
       @[<v 2>(solver-misc @\n\
       %a@])@])@\n"
      stats.major_words !Solver.solver_time !Solver.solver_count
      Solver.pp_statistics state.solver
  end

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
    [ Options.cmd_run run; Options.cmd_test test; Options.cmd_to_smt2 to_smt2 ]

let () =
  match Cmdliner.Cmd.eval_value cli with
  | Error (`Exn | `Parse | `Term) -> exit 2
  | Ok (`Help | `Ok () | `Version) -> ()

(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2026 formalsec *)
(* Written by the Smtml programmers *)

open Smtml

let run (settings : Settings.To_smt2.t) =
  let module Mappings : Mappings_intf.S_with_fresh =
    (val Solver_type.to_mappings settings.solver_type)
  in
  Mappings.set_debug settings.debug;
  let ast =
    match Parse.from_file settings.filename with
    | Ok script -> script
    | Error (`Msg err) -> Fmt.failwith "%s" err
  in
  let assertions =
    List.filter_map (function Ast.Assert e -> Some e | _ -> None) ast
  in
  let name = Fpath.to_string @@ Fpath.base settings.filename in
  Fmt.pr "%a" (Mappings.Smtlib.pp ~name ?logic:None ?status:None) assertions

let run_to_smtml ~filename =
  let ast =
    match Parse.from_file filename with
    | Ok script -> script
    | Error (`Msg err) -> Fmt.failwith "%s" err
  in
  let assertions =
    List.filter_map (function Ast.Assert e -> Some e | _ -> None) ast
  in
  Fmt.pr "%a" Expr.pp_smtml assertions

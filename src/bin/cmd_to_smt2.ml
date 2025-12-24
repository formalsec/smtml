open Smtml

let run (settings : Settings.To_smt2.t) =
  let module Mappings : Mappings_intf.S_with_fresh =
    (val Solver_type.to_mappings settings.solver_type)
  in
  Mappings.set_debug settings.debug;
  let ast = Parse.from_file settings.filename in
  let assertions =
    List.filter_map (function Ast.Assert e -> Some e | _ -> None) ast
  in
  let name = Fpath.to_string @@ Fpath.base settings.filename in
  Fmt.pr "%a" (Mappings.Smtlib.pp ~name ?logic:None ?status:None) assertions

let run_to_smtml ~filename =
  let ast = Parse.from_file filename in
  let assertions =
    List.filter_map (function Ast.Assert e -> Some e | _ -> None) ast
  in
  Fmt.pr "%a" Expr.pp_smtml assertions

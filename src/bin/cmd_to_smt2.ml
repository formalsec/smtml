open Smtml

let run ~debug ~solver_type ~filename =
  let module Mappings : Mappings_intf.S_with_fresh =
    (val Solver_type.to_mappings solver_type)
  in
  Mappings.set_debug debug;
  let ast = Parse.from_file filename in
  let assertions =
    List.filter_map (function Ast.Assert e -> Some e | _ -> None) ast
  in
  let name = Fpath.to_string @@ Fpath.base filename in
  Fmt.pr "%a" (Mappings.Smtlib.pp ~name ?logic:None ?status:None) assertions

let run_to_smtml ~filename =
  Num.set_default_printer `Full;
  Bitvector.set_default_printer `WithType;
  let ast = Parse.from_file filename in
  let assertions =
    List.filter_map (function Ast.Assert e -> Some e | _ -> None) ast
  in
  Fmt.pr "%a" Expr.pp_smtml assertions

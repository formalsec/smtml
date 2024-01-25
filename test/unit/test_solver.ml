open Encoding
open Ty
open Expr
module Solver = Solver.Z3_batch

let v i = Val (Int i) @: Ty_int
let eq i1 i2 = Relop (Eq, i1, i2) @: Ty_int
let ( >= ) i1 i2 = Relop (Ge, i1, i2) @: Ty_int
let ( * ) i1 i2 = Binop (Mul, i1, i2) @: Ty_int

let () =
  let solver = Solver.create ~logic:LIA () in
  let symbol_x = Symbol.("x" @: Ty_int) in
  let x = mk_symbol symbol_x in
  assert (Solver.check solver []);
  assert (Solver.check solver [ x >= v 0 ]);
  assert (
    let v = Solver.get_value solver x in
    v.node.e = Val (Int 0) && v.node.ty = Ty_int );
  assert (Solver.check solver [ eq x (v 3) ]);
  assert (
    let v = Solver.get_value solver (x * x) in
    v.node.e = Val (Int 9) && v.node.ty = Ty_int );
  assert (Solver.check solver []);
  let model = Solver.model ~symbols:[ symbol_x ] solver in
  let val_x = Option.bind model (fun m -> Model.evaluate m symbol_x) in
  assert (Option.is_some val_x);
  assert (Solver.check solver [ eq x (v 5) ]);
  let model = Solver.model solver in
  let val_x = Option.bind model (fun m -> Model.evaluate m symbol_x) in
  assert (Some (Value.Int 5) = val_x)

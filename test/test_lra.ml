open Encoding
open Ty
open Expr
module Solver = Solver.Z3_batch

let symbol_x = Symbol.("x" @: Ty_real)
let x = mk_symbol symbol_x
let v f = Val (Real f) @: Ty_real
let eq f1 f2 = Relop (Eq, f1, f2) @: Ty_bool

let () =
  let solver = Solver.create ~logic:LRA () in
  assert (Solver.check solver []);
  assert (Solver.check solver [ eq x (v 5.0) ]);
  let model = Solver.model solver in
  let val_x = Option.bind model (fun m -> Model.evaluate m symbol_x) in
  assert (Some (Value.Real 5.0) = val_x)

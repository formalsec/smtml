open Encoding
open Ty
open Expr
module Cached = Solver.Cached (Z3_mappings)
module Solver = Solver.Z3_batch

let v i = value (Int i)

let eq i1 i2 = relop Ty_bool Eq i1 i2

let ( >= ) i1 i2 = relop Ty_int Ge i1 i2

let ( * ) i1 i2 = binop Ty_int Mul i1 i2

(* Tests cached *)
let () =
  let solver = Cached.create ~logic:LIA () in
  let x = mk_symbol Symbol.("x" @: Ty_int) in
  let c = x >= v 0 in
  assert (!Cached.solver_count = 0);
  assert (`Sat = Cached.check solver [ c ]);
  assert (`Sat = Cached.check solver [ c ]);
  assert (`Sat = Cached.check solver [ c ]);
  assert (!Cached.solver_count = 1)

let () =
  let solver = Solver.create ~logic:LIA () in
  let symbol_x = Symbol.("x" @: Ty_int) in
  let x = mk_symbol symbol_x in
  assert (`Sat = Solver.check solver []);
  assert (`Sat = Solver.check solver [ x >= v 0 ]);
  assert (
    let v = Solver.get_value solver x in
    view v = Val (Int 0) );
  assert (`Sat = Solver.check solver [ eq x (v 3) ]);
  assert (
    let v = Solver.get_value solver (x * x) in
    view v = Val (Int 9) );
  assert (`Sat = Solver.check solver []);
  let model = Solver.model ~symbols:[ symbol_x ] solver in
  let val_x = Option.bind model (fun m -> Model.evaluate m symbol_x) in
  assert (Option.is_some val_x);
  assert (`Sat = Solver.check solver [ eq x (v 5) ]);
  let model = Solver.model solver in
  let val_x = Option.bind model (fun m -> Model.evaluate m symbol_x) in
  assert (Some (Value.Int 5) = val_x)

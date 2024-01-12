open Encoding
open Ty
open Expr
module Solver = Solver.Z3_batch
module Optimizer = Optimizer.Z3

let symbol_x = Symbol.("x" @: Ty_int)
let x = mk_symbol symbol_x
let v i = Val (Int i) @: Ty_int
let eq i1 i2 = Relop (Eq, i1, i2) @: Ty_bool
let ( < ) i1 i2 = Relop (Lt, i1, i2) @: Ty_int
let ( >= ) i1 i2 = Relop (Ge, i1, i2) @: Ty_int
let ( * ) i1 i2 = Binop (Mul, i1, i2) @: Ty_int

let () =
  let opt = Optimizer.create () in
  Optimizer.add opt [ x >= v 0; x < v 5 ];
  Optimizer.push opt;
  assert (Some (Value.Int 0) = Optimizer.minimize opt x);
  Optimizer.pop opt;
  assert (Some (Value.Int 4) = Optimizer.maximize opt x)

let () =
  let solver = Solver.create ~logic:LIA () in
  assert (Solver.check solver []);
  assert (Solver.check solver [ x >= v 0 ]);
  assert ({ e = Val (Int 0); ty = Ty_int } = Solver.get_value solver x);
  assert (Solver.check solver [ eq x (v 3) ]);
  assert ({ e = Val (Int 9); ty = Ty_int } = Solver.get_value solver (x * x));
  assert (Solver.check solver []);
  let model = Solver.model ~symbols:[ symbol_x ] solver in
  let val_x = Option.bind model (fun m -> Model.evaluate m symbol_x) in
  assert (Option.is_some val_x);
  assert (Solver.check solver [ eq x (v 5) ]);
  let model = Solver.model solver in
  let val_x = Option.bind model (fun m -> Model.evaluate m symbol_x) in
  assert (Some (Value.Int 5) = val_x)

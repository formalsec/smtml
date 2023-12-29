open Encoding
open Ty
open Expr
module Batch = Solver.Batch (Z3_mappings)

let symb_x = Symbol.("x" @: Ty_int)
let symb_y = Symbol.("y" @: Ty_bool)
let x = Expr.mk_symbol symb_x
let y = Expr.mk_symbol symb_y

let%test "check []" =
  let solver = Batch.create ~logic:QF_LIA () in
  Batch.check solver []

let%test "check [ x > 0 ]" =
  let solver = Batch.create ~logic:QF_LIA () in
  Batch.check solver [ Relop (Gt, x, Val (Int 0) @: Ty_int) @: Ty_int ]

let%test "get_value x" =
  let solver = Batch.create ~logic:QF_LIA () in
  Batch.add solver [ Relop (Eq, x, Val (Int 0) @: Ty_int) @: Ty_int ];
  assert (Batch.check solver []);
  let v = Batch.get_value solver x in
  Val (Int 0) = v.e

let%test "get_value (x * x)" =
  let solver = Batch.create ~logic:QF_LIA () in
  Batch.add solver [ Relop (Eq, x, Val (Int 3) @: Ty_int) @: Ty_int ];
  assert (Batch.check solver []);
  let v = Batch.get_value solver (Binop (Mul, x, x) @: Ty_int) in
  Val (Int 9) = v.e

let%test "eval []" =
  let solver = Batch.create ~logic:QF_LIA () in
  assert (Batch.check solver []);
  let m = Batch.model ~symbols:[ symb_x ] solver |> Option.get in
  Option.is_some @@ Model.evaluate m symb_x

let%test "eval [ x = 5 ]" =
  let solver = Batch.create ~logic:QF_LIA () in
  Batch.add solver [ Relop (Eq, x, Val (Int 5) @: Ty_int) @: Ty_int ];
  assert (Batch.check solver []);
  let m = Batch.model ~symbols:[ symb_x ] solver in
  Some (Value.Int 5) = Model.evaluate (Option.get m) symb_x

let%test "eval [ y = false ]" =
  let solver = Batch.create ~logic:QF_LIA () in
  Batch.add solver [ Relop (Eq, y, Val False @: Ty_bool) @: Ty_bool ];
  assert (Batch.check solver []);
  let model = Option.get (Batch.model solver) in
  Some Value.False = Model.evaluate model symb_y

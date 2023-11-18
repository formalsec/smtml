open Encoding
open Ty
open Expr
module Batch = Batch.Make (Z3_mappings)

let solver = Batch.create ()
let symb_x = Symbol.("x" @: Ty_int)
let symb_y = Symbol.("y" @: Ty_bool)
let x = Expr.mk_symbol symb_x
let y = Expr.mk_symbol symb_y
let%test "check []" = Batch.check solver []

let%test "check [ x > 0 ]" =
  Batch.check solver [ Relop (Gt, x, Val (Int 0) @: Ty_int) @: Ty_int ]

let%test "get_value x" =
  let pc = [ Relop (Eq, x, Val (Int 0) @: Ty_int) @: Ty_int ] in
  assert (Batch.check solver pc);
  let v = Batch.get_value solver x in
  Val (Int 0) = v.e

let%test "get_value (x * x)" =
  let pc = [ Relop (Eq, x, Val (Int 3) @: Ty_int) @: Ty_int ] in
  assert (Batch.check solver pc);
  let v = Batch.get_value solver (Binop (Mul, x, x) @: Ty_int) in
  Val (Int 9) = v.e

let%test "eval []" =
  assert (Batch.check solver []);
  let m = Batch.model ~symbols:[ symb_x ] solver |> Option.get in
  Option.is_some @@ Model.evaluate m symb_x

let%test "eval [ x = 5 ]" =
  assert (Batch.check solver [ Relop (Eq, x, Val (Int 5) @: Ty_int) @: Ty_int ]);
  let m = Batch.model ~symbols:[ symb_x ] solver in
  Some (Value.Int 5) = Model.evaluate (Option.get m) symb_x

let%test "eval [ y = false ]" =
  let pc = [ Relop (Eq, y, Val (Bool false) @: Ty_bool) @: Ty_bool ] in
  assert (Batch.check solver pc);
  let model = Option.get (Batch.model solver) in
  Some (Value.Bool false) = Model.evaluate model symb_y

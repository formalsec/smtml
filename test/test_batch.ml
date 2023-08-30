open Core
open Encoding
module Batch = Batch.Make (Z3_mappings)

let solver = Batch.create ()
let symb_x = Symbol.mk_symbol `IntType "x"
let symb_y = Symbol.mk_symbol `BoolType "y"
let x = Expression.mk_symbol symb_x
let y = Expression.mk_symbol symb_y

(* check *)
let%test "check-unconstrained" = Batch.check solver []

let%test "check-constrained" =
  Batch.check solver [ Integer.mk_gt x (Integer.mk_val 0) ]

(* eval *)
let%test "eval-unconstrained" =
  assert (Batch.check solver []);
  let m = Batch.model solver in
  Option.is_some @@ Model.evaluate (Option.value_exn m) symb_x

let%test "eval-constrained_int" =
  assert (Batch.check solver [ Integer.mk_eq x (Integer.mk_val 5) ]);
  let m = Batch.model ~symbols:[ symb_x ] solver in
  Poly.(Some (Value.Int 5) = Model.evaluate (Option.value_exn m) symb_x)

let%test "value_binds" =
  let pc = [ Boolean.mk_eq y (Boolean.mk_val false) ] in
  assert (Batch.check solver pc);
  let model = Option.value_exn (Batch.model solver) in
  Poly.(Some (Value.Bool false) = Model.evaluate model symb_y)

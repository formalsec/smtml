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
  let m = Batch.model ~symbols:[ symb_x ] solver |> Option.get in
  Option.is_some @@ Model.evaluate m symb_x

let%test "eval-constrained_int" =
  assert (Batch.check solver [ Integer.mk_eq x (Integer.mk_val 5) ]);
  let m = Batch.model ~symbols:[ symb_x ] solver in
  Some (Value.Int 5) = Model.evaluate (Option.get m) symb_x

let%test "value_binds" =
  let pc = [ Boolean.mk_eq y (Boolean.mk_val false) ] in
  assert (Batch.check solver pc);
  let model = Option.get (Batch.model solver) in
  Some (Value.Bool false) = Model.evaluate model symb_y

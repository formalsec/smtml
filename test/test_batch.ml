open Core
open Encoding

let solver = Batch.create ()
let x = Expression.mk_symbol_s `IntType "x"
let y = Expression.mk_symbol_s `BoolType "y"

(* check_sat *)
let%test "check_sat-unconstrained" = Batch.check_sat solver [ y ]

let%test "check_sat-constrained" =
  Batch.check_sat solver [ Integer.mk_gt x (Integer.mk_val 0) ]

(* eval *)
let%test "eval-unsat" =
  Option.is_none (Batch.eval solver x [ Boolean.mk_val false ])

let%test "eval-unconstrained" = Option.is_some (Batch.eval solver x [])

let%test "eval-constrained_int" =
  Poly.(
    Some (Value.Int 5)
    = Batch.eval solver x [ Integer.mk_eq x (Integer.mk_val 5) ])

let%test "eval-constrained_bool" =
  let pc = [ Boolean.mk_eq y (Boolean.mk_val true) ] in
  Poly.(Some (Value.Bool true) = Batch.eval solver y pc)

let%test "value_binds" =
  let symb_y = Symbol.mk_symbol `BoolType "y" in
  let pc = [ Boolean.mk_eq y (Boolean.mk_val false) ] in
  assert (Batch.check_sat solver pc);
  let model = Option.value_exn (Batch.find_model solver pc) in
  Poly.(Some (Value.Bool false) = Model.evaluate model symb_y)

open Encoding
module Batch = Batch.Make (Z3_mappings)

let solver = Batch.create ()
let symb_x = Symbol.mk_symbol `BoolType "x"
let x = Expression.mk_symbol symb_x

let%test "test_not" =
  let pc = [ Boolean.mk_not (Boolean.mk_eq x (Boolean.mk_val true)) ] in
  assert (Batch.check solver pc);
  let m = Batch.model solver in
  Some (Value.Bool false) = Model.evaluate (Option.get m) symb_x

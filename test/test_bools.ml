open Encoding

let solver = Batch.create ()
let int_symb = Expression.mk_symbol `IntType "x"
let bool_symb = Expression.mk_symbol `BoolType "y"

let%test "test_not" =
  let pc = [ Boolean.mk_not (Boolean.mk_eq bool_symb (Boolean.mk_val true)) ] in
  Some (Expression.Bool false) = Batch.eval solver bool_symb pc

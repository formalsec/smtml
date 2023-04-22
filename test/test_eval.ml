open Encoding

let solver = Batch.create ()
let int_symb = Expression.mk_symbol `IntType "x"
let bool_symb = Expression.mk_symbol `BoolType "y"

(* Satisfiability *)
let%test "test_unsat" =
  Option.is_none (Batch.eval solver int_symb [ Boolean.mk_val false ])

let%test "test_unconstrained" = Option.is_some (Batch.eval solver int_symb [])

let%test "test_constrained_int" =
  Some (Expression.Int 5)
  = Batch.eval solver int_symb [ Integer.mk_eq int_symb (Integer.mk_val 5) ]

let%test "test_constrained_bool" =
  let pc = [ Boolean.mk_eq bool_symb (Boolean.mk_val true) ] in
  Some (Expression.Bool true) = Batch.eval solver bool_symb pc

open Encoding

let solver = Batch.create ()
let x = Expression.mk_symbol `IntType "x"
let y = Expression.mk_symbol `BoolType "y"

(* Satisfiability *)
let%test "test_unsat" =
  Option.is_none (Batch.eval solver x [ Boolean.mk_val false ])

let%test "test_unconstrained" = Option.is_some (Batch.eval solver x [])

let%test "test_constrained_int" =
  Some (Expression.Int 5)
  = Batch.eval solver x [ Integer.mk_eq x (Integer.mk_val 5) ]

let%test "test_constrained_bool" =
  let pc = [ Boolean.mk_eq y (Boolean.mk_val true) ] in
  Some (Expression.Bool true) = Batch.eval solver y pc

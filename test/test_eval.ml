open Encoding
open Types
open Expression

let solver = Batch.create ()
let int_symb = Symbolic (`IntType, "x")
let bool_symb = Symbolic (`BoolType, "y")

(* Satisfiability *)
let%test "test_unsat" =
  Option.is_none (Batch.eval solver int_symb [ Val (Bool false) ])

let%test "test_unconstrained" = Option.is_some (Batch.eval solver int_symb [])

let%test "test_constrained_int" =
  Some (Int 5)
  = Batch.eval solver int_symb [ Relop (Int I.Eq, int_symb, Val (Int 5)) ]

let%test "test_constrained_bool" =
  let pc = [ Relop (Bool B.Eq, bool_symb, Val (Bool true)) ] in
  Some (Bool true) = Batch.eval solver bool_symb pc

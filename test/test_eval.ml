open Base
open Encoding
open Types
open Expression

let solver = Batch.create ()
let int_symb = Symbolic (`IntType, "x")

(* Satisfiability *)
let%test "eval_unsat" =
  let unsat = Relop (Int I.Eq, Val (Int 0), Val (Int 1)) in
  Option.is_none (Batch.eval solver int_symb [ unsat ])

let%test "eval_unconstrained" = Option.is_some (Batch.eval solver int_symb [])

let%test "eval_constrained" =
  Caml.( = ) (Option.some (Int 5))
    (Batch.eval solver int_symb [ Relop (Int I.Eq, int_symb, Val (Int 5)) ])

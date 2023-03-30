open Encoding
open Types
open Expression

let solver = Batch.create ()
let int_symb = Symbolic (`IntType, "x")
let bool_symb = Symbolic (`BoolType, "y")

(* Satisfiability *)

let%test "test_not" =
  let pc =
    [ Unop (Bool B.Not, Relop (Bool B.Eq, bool_symb, Val (Bool true))) ]
  in
  Some (Bool false) = Batch.eval solver bool_symb pc

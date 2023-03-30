open Encoding
open Types
open Expression

let opt = Optimizer.create ()
let int_symb = Symbolic (`IntType, "x")

(* Satisfiability *)
let%test "opt_min" =
  let pc =
    [
      Relop (Int I.Ge, int_symb, Val (Int 0));
      Relop (Int I.Lt, int_symb, Val (Int 5));
    ]
  in
  Some (Int 0) = Optimizer.minimize opt int_symb pc

let%test "opt_max" =
  let pc =
    [
      Relop (Int I.Ge, int_symb, Val (Int 0));
      Relop (Int I.Lt, int_symb, Val (Int 5));
    ]
  in
  Some (Int 4) = Optimizer.maximize opt int_symb pc

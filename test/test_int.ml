open Encoding
open Ty
open Expr
module Batch = Solver.Batch (Z3_mappings)

let solver = Batch.create ()
let one = Val (Int 1) @: Ty_int
let zero = Val (Int 0) @: Ty_int
let minus_one = Val (Int (-1)) @: Ty_int
let x = mk_symbol Symbol.("x" @: Ty_int)

(* Satisfiability *)
let%test _ = Batch.check solver [ Relop (Gt, x, zero) @: Ty_int ]
let%test _ = Batch.check solver [ Relop (Gt, one, minus_one) @: Ty_int ]

let%test _ =
  Batch.check solver [ Relop (Eq, Binop (Pow, x, one) @: Ty_int, x) @: Ty_int ]

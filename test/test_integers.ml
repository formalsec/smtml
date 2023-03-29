open Base
open Encoding
open Types
open Expression

let solver = Batch.create ()
let encode e = try ignore (Common.encode_expr e) with exn -> raise exn
let int_pos = Val (Int Int.one)
let int_neg = Val (Int Int.minus_one)
let int_zero = Val (Int Int.zero)
let int_symb = Symbolic (`IntType, "x")
(* Encoding *)
let%test_unit _ = encode int_pos
let%test_unit _ = encode int_neg
let%test_unit _ = encode int_zero
let%test_unit _ = encode int_symb
(* Satisfiability *)
let%test _ = Batch.check solver [ Relop (Int I.Gt, int_symb, int_zero) ]
let%test _ = Batch.check solver [ Relop (Int I.Gt, int_pos, int_neg) ]

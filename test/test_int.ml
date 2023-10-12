open Encoding
module Batch = Batch.Make (Z3_mappings)

let solver = Batch.create ()
let one = Integer.mk_val 1
let zero = Integer.mk_val 0
let minus_one = Integer.mk_val (-1)

let x = Expression.mk_symbol_s `IntType "x"

(* Satisfiability *)
let%test _ = Batch.check solver [ Integer.mk_gt x zero ]

let%test _ = Batch.check solver [ Integer.mk_gt one minus_one ]

let%test _ = Batch.check solver [ Integer.mk_eq (Integer.mk_pow x one) x ]

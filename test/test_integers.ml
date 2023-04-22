open Encoding

let solver = Batch.create ()
let encode e = try ignore (Z3_mappings.encode_expr e) with exn -> raise exn
let int_pos = Integer.mk_val Int.one
let int_neg = Integer.mk_val Int.minus_one
let int_zero = Integer.mk_val Int.zero
let int_symb = Expression.mk_symbol `IntType "x"

(* Encoding *)
let%test_unit _ = encode int_pos
let%test_unit _ = encode int_neg
let%test_unit _ = encode int_zero
let%test_unit _ = encode int_symb
(* Satisfiability *)
let%test _ = Batch.check_sat solver [ Integer.mk_gt int_symb int_zero ]
let%test _ = Batch.check_sat solver [ Integer.mk_gt int_pos int_neg ]

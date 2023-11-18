open Encoding
open Ty
open Expr
module Batch = Batch.Make (Z3_mappings)

let solver = Batch.create ()
let abc = Val (Str "abc") @: Ty_str
let symb_x = Symbol.("x" @: Ty_str)
let x = Expr.mk_symbol symb_x
let zero = Val (Int 0) @: Ty_int
let two = Val (Int 2) @: Ty_int

(* Satisfiability *)
let%test "test_concrete_len" =
  Batch.check solver
    [ Relop (Ge, Unop (Len, x) @: Ty_str, Unop (Len, abc) @: Ty_str) @: Ty_int ]

let%test "test_constrained_len" =
  not
    (Batch.check solver
       [ Relop (Eq, Unop (Len, x) @: Ty_str, Val (Int 4) @: Ty_int) @: Ty_int
       ; Relop (Eq, Unop (Len, x) @: Ty_str, Unop (Len, abc) @: Ty_str)
         @: Ty_int
       ] )

let%test "test_concrete_substr" =
  let pc =
    [ Relop
        (Eq, Triop (Substr, abc, zero, two) @: Ty_str, Val (Str "ab") @: Ty_str)
      @: Ty_str
    ]
  in
  Batch.check solver pc

let%test "test_symb_substr" =
  let pc =
    [ Relop (Eq, x, abc) @: Ty_str
    ; Relop
        (Eq, Unop (Len, Triop (Substr, x, zero, two) @: Ty_str) @: Ty_str, two)
      @: Ty_int
    ]
  in
  assert (Batch.check solver pc);
  let m = Batch.model solver in
  Some (Value.Str "abc") = Model.evaluate (Option.get m) symb_x

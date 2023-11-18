open Encoding
open Ty
open Expr
module Batch = Batch.Make (Z3_mappings)

let solver = Batch.create ()
let x = Expr.mk_symbol Symbol.("x" @: Ty_fp S32)
let nan = Val (Num (F32 (Int32.bits_of_float Float.nan))) @: Ty_fp S32

let%test "deterministic_nan" =
  let pc =
    [ Unop (Not, Unop (Is_nan, x) @: Ty_fp S32) @: Ty_bool
    ; Unop (Is_nan, x) @: Ty_fp S32
    ]
  in
  false = Batch.check solver pc

let%test "nondeterministic_nan" =
  let pc = [ Relop (Ne, x, nan) @: Ty_fp S32; Unop (Is_nan, x) @: Ty_fp S32 ] in
  true = Batch.check solver pc

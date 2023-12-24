open Encoding
open Ty
open Expr
module Batch = Solver.Batch (Z3_mappings)

let solver = Batch.create ()

let%test _ =
  let x = mk_symbol Symbol.("x" @: Ty_fp S32) in
  let const = Val (Num (F32 (Int32.bits_of_float 50.0))) @: Ty_fp S32 in
  let es = [ Relop (Eq, x, const) @: Ty_fp S32 ] in
  assert (Batch.check solver es);
  Batch.get_value solver x = const

let%test _ =
  let x = mk_symbol Symbol.("x" @: Ty_fp S64) in
  let const = Val (Num (F64 (Int64.bits_of_float 50.0))) @: Ty_fp S64 in
  let es = [ Relop (Eq, x, const) @: Ty_fp S64 ] in
  assert (Batch.check solver es);
  Batch.get_value solver x = const

let cvtop = Eval_numeric.eval_cvtop

let%test _ =
  cvtop (Ty_bitv S32) TruncSF32 (F32 (Int32.bits_of_float 8.5)) = I32 8l

let%test _ =
  cvtop (Ty_bitv S32) TruncSF64 (F64 (Int64.bits_of_float 8.5)) = I32 8l

let%test _ =
  cvtop (Ty_bitv S64) TruncSF32 (F32 (Int32.bits_of_float 8.5)) = I64 8L

let%test _ =
  cvtop (Ty_bitv S64) TruncSF64 (F64 (Int64.bits_of_float 8.5)) = I64 8L

let%test _ =
  cvtop (Ty_fp S32) ConvertSI32 (I32 8l) = F32 (Int32.bits_of_float 8.0)

let%test _ =
  cvtop (Ty_fp S32) ConvertSI64 (I64 8L) = F32 (Int32.bits_of_float 8.0)

let%test _ =
  cvtop (Ty_fp S64) ConvertSI32 (I32 8l) = F64 (Int64.bits_of_float 8.0)

let%test _ =
  cvtop (Ty_fp S64) ConvertSI64 (I64 8L) = F64 (Int64.bits_of_float 8.0)

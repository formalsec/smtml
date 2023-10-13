open Encoding
open Expression

module Batch = Batch.Make (Z3_mappings)

let solver = Batch.create ()

let%test _ =
  let x = mk_symbol_s `F32Type "x" in
  let const = FloatingPoint.mk_val 50.0 `F32Type in
  let es = [ FloatingPoint.(mk_eq x const `F32Type)  ] in
  assert (Batch.check solver es);
  Batch.get_value solver x = const

let%test _ =
  let x = mk_symbol_s `F64Type "x" in
  let const = FloatingPoint.mk_val 50.0 `F64Type in
  let es = [ FloatingPoint.(mk_eq x (mk_val 50.0 `F64Type) `F64Type)  ] in
  assert (Batch.check solver es);
  Batch.get_value solver x = const

let cvtop = Eval_numeric.eval_cvtop

let%test _ = cvtop (I32 TruncSF32) (F32 (Int32.bits_of_float 8.5)) = I32 8l

let%test _ = cvtop (I32 TruncSF64) (F64 (Int64.bits_of_float 8.5)) = I32 8l

let%test _ = cvtop (I64 TruncSF32) (F32 (Int32.bits_of_float 8.5)) = I64 8L

let%test _ = cvtop (I64 TruncSF64) (F64 (Int64.bits_of_float 8.5)) = I64 8L

let%test _ = cvtop (F32 ConvertSI32) (I32 8l) = F32 (Int32.bits_of_float 8.0)

let%test _ = cvtop (F32 ConvertSI64) (I64 8L) = F32 (Int32.bits_of_float 8.0)

let%test _ = cvtop (F64 ConvertSI32) (I32 8l) = F64 (Int64.bits_of_float 8.0)

let%test _ = cvtop (F64 ConvertSI64) (I64 8L) = F64 (Int64.bits_of_float 8.0)

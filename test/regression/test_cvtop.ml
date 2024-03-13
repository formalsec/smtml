open Encoding
open Ty

let f32 f = Value.Num (F32 (Int32.bits_of_float f))

let f64 f = Value.Num (F64 (Int64.bits_of_float f))

let () =
  let cvtop = Eval_numeric.eval_cvtop in
  assert (cvtop (Ty_bitv 32) TruncSF32 (f32 8.5) = Num (I32 8l));
  assert (cvtop (Ty_bitv 32) TruncSF64 (f64 8.5) = Num (I32 8l));
  assert (cvtop (Ty_bitv 64) TruncSF32 (f32 8.5) = Num (I64 8L));
  assert (cvtop (Ty_bitv 64) TruncSF64 (f64 8.5) = Num (I64 8L));
  assert (cvtop (Ty_fp 32) ConvertSI32 (Num (I32 8l)) = f32 8.0);
  assert (cvtop (Ty_fp 32) ConvertSI64 (Num (I64 8L)) = f32 8.0);
  assert (cvtop (Ty_fp 64) ConvertSI32 (Num (I32 8l)) = f64 8.0);
  assert (cvtop (Ty_fp 64) ConvertSI64 (Num (I64 8L)) = f64 8.0)

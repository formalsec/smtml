open Encoding
open Ty
open Encoding.Expr

let f32 f = Num.F32 (Int32.bits_of_float f)
let f64 f = Num.F64 (Int64.bits_of_float f)

let () =
  let ptr = Ptr (8390670l, Expr.Bitv.I32.v 2l) @: Ty_bitv S32 in
  let rem = Binop (Rem, ptr, Expr.Bitv.I32.v 1l) @: Ty_bitv S32 in
  let result = Expr.simplify rem in
  assert(result.e = Val (Num (I32 0l)))

let () =
  let cvtop = Eval_numeric.eval_cvtop in
  assert (cvtop (Ty_bitv S32) TruncSF32 (f32 8.5) = I32 8l);
  assert (cvtop (Ty_bitv S32) TruncSF64 (f64 8.5) = I32 8l);
  assert (cvtop (Ty_bitv S64) TruncSF32 (f32 8.5) = I64 8L);
  assert (cvtop (Ty_bitv S64) TruncSF64 (f64 8.5) = I64 8L);
  assert (cvtop (Ty_fp S32) ConvertSI32 (I32 8l) = f32 8.0);
  assert (cvtop (Ty_fp S32) ConvertSI64 (I64 8L) = f32 8.0);
  assert (cvtop (Ty_fp S64) ConvertSI32 (I32 8l) = f64 8.0);
  assert (cvtop (Ty_fp S64) ConvertSI64 (I64 8L) = f64 8.0)

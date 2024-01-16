open Encoding
open Ty

let f32 f = Num.F32 (Int32.bits_of_float f)
let f64 f = Num.F64 (Int64.bits_of_float f)

let () =
  let i32_unop = Eval_numeric.eval_unop (Ty_bitv S32) in
  let i32_binop = Eval_numeric.eval_binop (Ty_bitv S32) in
  let i32_relop = Eval_numeric.eval_relop (Ty_bitv S32) in
  let i32_cvtop = Eval_numeric.eval_cvtop (Ty_bitv S32) in
  assert (i32_unop Neg (I32 1l) = I32 (-1l));
  assert (i32_unop Not (I32 (-1l)) = I32 0l);
  assert (i32_binop Add (I32 0l) (I32 1l) = I32 1l);
  assert (i32_binop Sub (I32 1l) (I32 0l) = I32 1l);
  let x = i32_binop Mul (I32 2l) (I32 2l) in
  assert (i32_binop Div x (I32 2l) = I32 2l);
  assert (i32_binop Rem (I32 10l) (I32 7l) = I32 3l);
  assert (i32_binop And (I32 1l) (I32 0l) = I32 0l);
  assert (i32_binop Or (I32 0l) (I32 1l) = I32 1l);
  assert (i32_binop Xor (I32 1l) (I32 1l) = I32 0l);
  assert (i32_binop Shl (I32 1l) (I32 2l) = I32 4l);
  assert (i32_binop ShrA (I32 4l) (I32 2l) = I32 1l);
  assert (i32_relop Eq (I32 0l) (I32 0l));
  assert (i32_relop Ne (I32 0l) (I32 1l));
  assert (i32_relop Lt (I32 0l) (I32 1l));
  assert (i32_relop LtU (I32 0l) (I32 (-1l)));
  assert (i32_relop Le (I32 0l) (I32 1l));
  assert (i32_relop LeU (I32 0l) (I32 (-1l)));
  assert (i32_relop Gt (I32 1l) (I32 0l));
  assert (i32_relop GtU (I32 (-1l)) (I32 0l));
  assert (i32_relop Ge (I32 1l) (I32 0l));
  assert (i32_relop GeU (I32 (-1l)) (I32 0l));
  assert (i32_cvtop TruncSF32 (f32 8.5) = I32 8l);
  assert (i32_cvtop TruncSF64 (f64 8.5) = I32 8l)

let () =
  let i64_unop = Eval_numeric.eval_unop (Ty_bitv S64) in
  let i64_binop = Eval_numeric.eval_binop (Ty_bitv S64) in
  let i64_relop = Eval_numeric.eval_relop (Ty_bitv S64) in
  let i64_cvtop = Eval_numeric.eval_cvtop (Ty_bitv S64) in
  assert (i64_unop Neg (I64 1L) = I64 (-1L));
  assert (i64_unop Not (I64 (-1L)) = I64 0L);
  assert (i64_binop Add (I64 0L) (I64 1L) = I64 1L);
  assert (i64_binop Sub (I64 1L) (I64 0L) = I64 1L);
  let x = i64_binop Mul (I64 2L) (I64 2L) in
  assert (i64_binop Div x (I64 2L) = I64 2L);
  assert (i64_binop Rem (I64 10L) (I64 7L) = I64 3L);
  assert (i64_binop And (I64 1L) (I64 0L) = I64 0L);
  assert (i64_binop Or (I64 0L) (I64 1L) = I64 1L);
  assert (i64_binop Xor (I64 1L) (I64 1L) = I64 0L);
  assert (i64_binop Shl (I64 1L) (I64 2L) = I64 4L);
  assert (i64_binop ShrA (I64 4L) (I64 2L) = I64 1L);
  assert (i64_relop Eq (I64 0L) (I64 0L));
  assert (i64_relop Ne (I64 0L) (I64 1L));
  assert (i64_relop Lt (I64 0L) (I64 1L));
  assert (i64_relop LtU (I64 0L) (I64 (-1L)));
  assert (i64_relop Le (I64 0L) (I64 1L));
  assert (i64_relop LeU (I64 0L) (I64 (-1L)));
  assert (i64_relop Gt (I64 1L) (I64 0L));
  assert (i64_relop GtU (I64 (-1L)) (I64 0L));
  assert (i64_relop Ge (I64 1L) (I64 0L));
  assert (i64_relop GeU (I64 (-1L)) (I64 0L));
  assert (i64_cvtop TruncSF32 (f32 8.5) = I64 8L);
  assert (i64_cvtop TruncSF64 (f64 8.5) = I64 8L)

let () =
  let f32_cvtop = Eval_numeric.eval_cvtop (Ty_fp S32) in
  assert (f32_cvtop ConvertSI32 (I32 8l) = f32 8.0);
  assert (f32_cvtop ConvertSI64 (I64 8L) = f32 8.0)

let () =
  let f64_cvtop = Eval_numeric.eval_cvtop (Ty_fp S64) in
  assert (f64_cvtop ConvertSI32 (I32 8l) = f64 8.0);
  assert (f64_cvtop ConvertSI64 (I64 8L) = f64 8.0)

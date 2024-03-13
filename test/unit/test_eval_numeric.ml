open Encoding
open Ty

let () =
  let i32 i = Value.Num (I32 i) in
  let i32_unop = Eval_numeric.eval_unop (Ty_bitv 32) in
  let i32_binop = Eval_numeric.eval_binop (Ty_bitv 32) in
  let i32_relop = Eval_numeric.eval_relop (Ty_bitv 32) in
  assert (i32_unop Neg (i32 1l) = i32 (-1l));
  assert (i32_unop Not (i32 (-1l)) = i32 0l);
  assert (i32_binop Add (i32 0l) (i32 1l) = i32 1l);
  assert (i32_binop Sub (i32 1l) (i32 0l) = i32 1l);
  let x = i32_binop Mul (i32 2l) (i32 2l) in
  assert (i32_binop Div x (i32 2l) = i32 2l);
  assert (i32_binop Rem (i32 10l) (i32 7l) = i32 3l);
  assert (i32_binop And (i32 1l) (i32 0l) = i32 0l);
  assert (i32_binop Or (i32 0l) (i32 1l) = i32 1l);
  assert (i32_binop Xor (i32 1l) (i32 1l) = i32 0l);
  assert (i32_binop Shl (i32 1l) (i32 2l) = i32 4l);
  assert (i32_binop ShrA (i32 4l) (i32 2l) = i32 1l);
  assert (i32_binop Rotl (i32 Int32.min_int) (i32 2l) = i32 2l);
  assert (i32_binop Rotr (i32 2l) (i32 2l) = i32 Int32.min_int);
  assert (i32_relop Eq (i32 0l) (i32 0l));
  assert (i32_relop Ne (i32 0l) (i32 1l));
  assert (i32_relop Lt (i32 0l) (i32 1l));
  assert (i32_relop LtU (i32 0l) (i32 (-1l)));
  assert (i32_relop Le (i32 0l) (i32 1l));
  assert (i32_relop LeU (i32 0l) (i32 (-1l)));
  assert (i32_relop Gt (i32 1l) (i32 0l));
  assert (i32_relop GtU (i32 (-1l)) (i32 0l));
  assert (i32_relop Ge (i32 1l) (i32 0l));
  assert (i32_relop GeU (i32 (-1l)) (i32 0l))

let () =
  let i64 i = Value.Num (I64 i) in
  let i64_unop = Eval_numeric.eval_unop (Ty_bitv 64) in
  let i64_binop = Eval_numeric.eval_binop (Ty_bitv 64) in
  let i64_relop = Eval_numeric.eval_relop (Ty_bitv 64) in
  assert (i64_unop Neg (i64 1L) = i64 (-1L));
  assert (i64_unop Not (i64 (-1L)) = i64 0L);
  assert (i64_binop Add (i64 0L) (i64 1L) = i64 1L);
  assert (i64_binop Sub (i64 1L) (i64 0L) = i64 1L);
  let x = i64_binop Mul (i64 2L) (i64 2L) in
  assert (i64_binop Div x (i64 2L) = i64 2L);
  assert (i64_binop Rem (i64 10L) (i64 7L) = i64 3L);
  assert (i64_binop And (i64 1L) (i64 0L) = i64 0L);
  assert (i64_binop Or (i64 0L) (i64 1L) = i64 1L);
  assert (i64_binop Xor (i64 1L) (i64 1L) = i64 0L);
  assert (i64_binop Shl (i64 1L) (i64 2L) = i64 4L);
  assert (i64_binop ShrA (i64 4L) (i64 2L) = i64 1L);
  assert (i64_binop Rotl (i64 Int64.min_int) (i64 2L) = i64 2L);
  assert (i64_binop Rotr (i64 2L) (i64 2L) = i64 Int64.min_int);
  assert (i64_relop Eq (i64 0L) (i64 0L));
  assert (i64_relop Ne (i64 0L) (i64 1L));
  assert (i64_relop Lt (i64 0L) (i64 1L));
  assert (i64_relop LtU (i64 0L) (i64 (-1L)));
  assert (i64_relop Le (i64 0L) (i64 1L));
  assert (i64_relop LeU (i64 0L) (i64 (-1L)));
  assert (i64_relop Gt (i64 1L) (i64 0L));
  assert (i64_relop GtU (i64 (-1L)) (i64 0L));
  assert (i64_relop Ge (i64 1L) (i64 0L));
  assert (i64_relop GeU (i64 (-1L)) (i64 0L))

let () =
  let f32_unop = Eval_numeric.eval_unop (Ty_fp 32) in
  let f64_unop = Eval_numeric.eval_unop (Ty_fp 64) in
  assert (f32_unop Trunc (Num (F32 (Int32.bits_of_float 0.75))) = Num (F32 0l));
  assert (f64_unop Trunc (Num (F64 (Int64.bits_of_float 0.75))) = Num (F64 0L))

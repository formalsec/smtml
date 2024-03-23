open Encoding
open Ty
open Expr
open Value

(* int *)
let () =
  let v i = value (Int i) in
  assert (relop (Ty_int) Eq (v 0) (v 0) = value True);
  assert (relop (Ty_int) Ne (v 0) (v 0) = value False);
  assert (relop (Ty_int) Lt (v 0) (v 1) = value True);
  assert (relop (Ty_int) Le (v 0) (v 1) = value True);
  assert (relop (Ty_int) Gt (v 0) (v 1) = value False);
  assert (relop (Ty_int) Ge (v 0) (v 1) = value False)

(* real *)
let () =
  let v i = value (Real i) in
  assert (relop (Ty_real) Eq (v 0.0) (v 0.0) = value True);
  assert (relop (Ty_real) Ne (v 0.0) (v 0.0) = value False);
  assert (relop (Ty_real) Lt (v 0.0) (v 1.0) = value True);
  assert (relop (Ty_real) Le (v 0.0) (v 1.0) = value True);
  assert (relop (Ty_real) Gt (v 0.0) (v 1.0) = value False);
  assert (relop (Ty_real) Ge (v 0.0) (v 1.0) = value False)

(* i32 *)
let () =
  let v i = value (Num (I32 i)) in
  assert (relop (Ty_bitv 32) Eq (v 0l) (v 0l) = value True);
  assert (relop (Ty_bitv 32) Ne (v 0l) (v 0l) = value False);
  assert (relop (Ty_bitv 32) Lt (v 0l) (v 1l) = value True);
  assert (relop (Ty_bitv 32) LtU (v (-1l)) (v 0l) = value False);
  assert (relop (Ty_bitv 32) Le (v 0l) (v 1l) = value True);
  assert (relop (Ty_bitv 32) LeU (v (-1l)) (v 0l) = value False);
  assert (relop (Ty_bitv 32) Gt (v 1l) (v 0l) = value True);
  assert (relop (Ty_bitv 32) GtU (v 0l) (v (-1l)) = value False);
  assert (relop (Ty_bitv 32) Ge (v 1l) (v 0l) = value True);
  assert (relop (Ty_bitv 32) GeU (v 0l) (v (-1l)) = value False)

(* i64 *)
let () =
  let v i = value (Num (I64 i)) in
  assert (relop (Ty_bitv 64) Eq (v 0L) (v 0L) = value True);
  assert (relop (Ty_bitv 64) Ne (v 0L) (v 0L) = value False);
  assert (relop (Ty_bitv 64) Lt (v 0L) (v 1L) = value True);
  assert (relop (Ty_bitv 64) LtU (v (-1L)) (v 0L) = value False);
  assert (relop (Ty_bitv 64) Le (v 0L) (v 1L) = value True);
  assert (relop (Ty_bitv 64) LeU (v (-1L)) (v 0L) = value False);
  assert (relop (Ty_bitv 64) Gt (v 1L) (v 0L) = value True);
  assert (relop (Ty_bitv 64) GtU (v 0L) (v (-1L)) = value False);
  assert (relop (Ty_bitv 64) Ge (v 1L) (v 0L) = value True);
  assert (relop (Ty_bitv 64) GeU (v 0L) (v (-1L)) = value False)

(* f32 *)
let () =
  let v f = value (Num (F32 (Int32.bits_of_float f))) in
  assert (relop (Ty_fp 32) Eq (v 0.0) (v 0.0) = value True);
  assert (relop (Ty_fp 32) Ne (v 0.0) (v 0.0) = value False);
  assert (relop (Ty_fp 32) Lt (v 0.0) (v 1.0) = value True);
  assert (relop (Ty_fp 32) Le (v 0.0) (v 1.0) = value True);
  assert (relop (Ty_fp 32) Gt (v 0.0) (v 1.0) = value False);
  assert (relop (Ty_fp 32) Ge (v 0.0) (v 1.0) = value False)

(* f64 *)
let () =
  let v f = value (Num (F64 (Int64.bits_of_float f))) in
  assert (relop (Ty_fp 64) Eq (v 0.0) (v 0.0) = value True);
  assert (relop (Ty_fp 64) Ne (v 0.0) (v 0.0) = value False);
  assert (relop (Ty_fp 64) Lt (v 0.0) (v 1.0) = value True);
  assert (relop (Ty_fp 64) Le (v 0.0) (v 1.0) = value True);
  assert (relop (Ty_fp 64) Gt (v 0.0) (v 1.0) = value False);
  assert (relop (Ty_fp 64) Ge (v 0.0) (v 1.0) = value False)

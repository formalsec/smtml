open Smtml
open Ty
open Expr
open Value

let int i = value (Int i)

let real f = value (Real f)

let i32 i = value (Num (I32 i))

let i64 i = value (Num (I64 i))

let f32 f = value (Num (F32 (Int32.bits_of_float f)))

let f64 f = value (Num (F64 (Int64.bits_of_float f)))

(* bool *)
let () =
  assert (relop Ty_bool Eq (int 0) (int 0) = value True);
  assert (relop Ty_bool Ne (int 0) (int 0) = value False);
  assert (relop Ty_bool Eq (real 0.0) (real 0.0) = value True);
  assert (relop Ty_bool Ne (real 0.0) (real 0.0) = value False);
  assert (relop Ty_bool Eq (i32 0l) (i32 0l) = value True);
  assert (relop Ty_bool Ne (i32 0l) (i32 0l) = value False);
  assert (relop Ty_bool Eq (i64 0L) (i64 0L) = value True);
  assert (relop Ty_bool Ne (i64 0L) (i64 0L) = value False);
  assert (relop Ty_bool Eq (f32 0.0) (f32 0.0) = value True);
  assert (relop Ty_bool Ne (f32 0.0) (f32 0.0) = value False);
  assert (relop Ty_bool Eq (f64 0.0) (f64 0.0) = value True);
  assert (relop Ty_bool Ne (f64 0.0) (f64 0.0) = value False)

(* int *)
let () =
  assert (relop Ty_int Lt (int 0) (int 1) = value True);
  assert (relop Ty_int Le (int 0) (int 1) = value True);
  assert (relop Ty_int Gt (int 0) (int 1) = value False);
  assert (relop Ty_int Ge (int 0) (int 1) = value False)

(* real *)
let () =
  assert (relop Ty_real Lt (real 0.0) (real 1.0) = value True);
  assert (relop Ty_real Le (real 0.0) (real 1.0) = value True);
  assert (relop Ty_real Gt (real 0.0) (real 1.0) = value False);
  assert (relop Ty_real Ge (real 0.0) (real 1.0) = value False)

(* i32 *)
let () =
  assert (relop (Ty_bitv 32) Lt (i32 0l) (i32 1l) = value True);
  assert (relop (Ty_bitv 32) LtU (i32 (-1l)) (i32 0l) = value False);
  assert (relop (Ty_bitv 32) Le (i32 0l) (i32 1l) = value True);
  assert (relop (Ty_bitv 32) LeU (i32 (-1l)) (i32 0l) = value False);
  assert (relop (Ty_bitv 32) Gt (i32 1l) (i32 0l) = value True);
  assert (relop (Ty_bitv 32) GtU (i32 0l) (i32 (-1l)) = value False);
  assert (relop (Ty_bitv 32) Ge (i32 1l) (i32 0l) = value True);
  assert (relop (Ty_bitv 32) GeU (i32 0l) (i32 (-1l)) = value False)

(* i64 *)
let () =
  assert (relop (Ty_bitv 64) Lt (i64 0L) (i64 1L) = value True);
  assert (relop (Ty_bitv 64) LtU (i64 (-1L)) (i64 0L) = value False);
  assert (relop (Ty_bitv 64) Le (i64 0L) (i64 1L) = value True);
  assert (relop (Ty_bitv 64) LeU (i64 (-1L)) (i64 0L) = value False);
  assert (relop (Ty_bitv 64) Gt (i64 1L) (i64 0L) = value True);
  assert (relop (Ty_bitv 64) GtU (i64 0L) (i64 (-1L)) = value False);
  assert (relop (Ty_bitv 64) Ge (i64 1L) (i64 0L) = value True);
  assert (relop (Ty_bitv 64) GeU (i64 0L) (i64 (-1L)) = value False)

(* f32 *)
let () =
  assert (relop (Ty_fp 32) Lt (f32 0.0) (f32 1.0) = value True);
  assert (relop (Ty_fp 32) Le (f32 0.0) (f32 1.0) = value True);
  assert (relop (Ty_fp 32) Gt (f32 0.0) (f32 1.0) = value False);
  assert (relop (Ty_fp 32) Ge (f32 0.0) (f32 1.0) = value False)

(* f64 *)
let () =
  assert (relop (Ty_fp 64) Lt (f64 0.0) (f64 1.0) = value True);
  assert (relop (Ty_fp 64) Le (f64 0.0) (f64 1.0) = value True);
  assert (relop (Ty_fp 64) Gt (f64 0.0) (f64 1.0) = value False);
  assert (relop (Ty_fp 64) Ge (f64 0.0) (f64 1.0) = value False)

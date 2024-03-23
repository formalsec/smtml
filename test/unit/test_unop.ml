open Encoding
open Ty
open Expr
open Value

(* int *)
let () =
  let v i = value (Int i) in
  assert (unop Ty_int Neg (v 1) = v ~-1)

(* real *)
let () =
  let v f = value (Real f) in
  assert (unop Ty_real Neg (v 1.0) = v (-1.0));
  assert (unop Ty_real Abs (v 1.0) = v 1.0);
  assert (unop Ty_real Sqrt (v 4.0) = v 2.0);
  assert (unop Ty_real Nearest (v 0.504) = v 1.0);
  assert (unop Ty_real Ceil (v 0.3) = v 1.0);
  assert (unop Ty_real Floor (v 0.7) = v 0.0);
  assert (unop Ty_real Trunc (v 1.504) = v 1.0);
  assert (unop Ty_real Is_nan (v Float.nan) = value True)

(* i32 *)
let () =
  let v i = value (Num (I32 i)) in
  assert (unop (Ty_bitv 32) Neg (v 1l) = v (-1l));
  assert (unop (Ty_bitv 32) Not (v (-1l)) = v 0l)

(* i64 *)
let () =
  let v i = value (Num (I64 i)) in
  assert (unop (Ty_bitv 64) Neg (v 1L) = v (-1L));
  assert (unop (Ty_bitv 64) Not (v (-1L)) = v 0L)

(* f32 *)
let () =
  let v f = value (Num (F32 (Int32.bits_of_float f))) in
  assert (unop (Ty_fp 32) Trunc (v 0.75) = v 0.0)

(* f64 *)
let () =
  let v f = value (Num (F64 (Int64.bits_of_float f))) in
  assert (unop (Ty_fp 64) Trunc (v 0.75) = v 0.0)

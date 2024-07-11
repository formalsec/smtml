open Smtml
open Ty
open Expr
open Value

let ( = ) = Expr.equal

let true_ = value True

let false_ = value False

let int i = value (Int i)

let real f = value (Real f)

let str s = value (Str s)

let int32 i = value (Num (I32 i))

let int64 i = value (Num (I64 i))

let float32 f = value (Num (F32 (Int32.bits_of_float f)))

let float64 f = value (Num (F64 (Int64.bits_of_float f)))

let app f = value (App (f, []))

(* bool *)
let () =
  assert (relop Ty_bool Eq (int 0) (int 0) = true_);
  assert (relop Ty_bool Ne (int 0) (int 0) = false_);
  assert (relop Ty_bool Eq (real 0.0) (real 0.0) = true_);
  assert (relop Ty_bool Ne (real 0.0) (real 0.0) = false_);
  assert (relop Ty_bool Eq (int32 0l) (int32 0l) = true_);
  assert (relop Ty_bool Ne (int32 0l) (int32 0l) = false_);
  assert (relop Ty_bool Eq (int64 0L) (int64 0L) = true_);
  assert (relop Ty_bool Ne (int64 0L) (int64 0L) = false_);
  assert (relop Ty_bool Eq (float32 0.0) (float32 0.0) = true_);
  assert (relop Ty_bool Ne (float32 0.0) (float32 0.0) = false_);
  assert (relop Ty_bool Eq (float64 0.0) (float64 0.0) = true_);
  assert (relop Ty_bool Ne (float64 0.0) (float64 0.0) = false_)

(* int *)
let () =
  assert (relop Ty_int Lt (int 0) (int 1) = true_);
  assert (relop Ty_int Le (int 0) (int 1) = true_);
  assert (relop Ty_int Gt (int 0) (int 1) = false_);
  assert (relop Ty_int Ge (int 0) (int 1) = false_)

(* real *)
let () =
  let x = symbol @@ Symbol.make Ty_real "x" in
  assert (relop Ty_bool Ne (real Float.nan) x = true_);
  assert (relop Ty_bool Eq x (real Float.nan) = false_);
  assert (relop Ty_real Lt (real 0.0) (real 1.0) = true_);
  assert (relop Ty_real Le (real 0.0) (real 1.0) = true_);
  assert (relop Ty_real Gt (real 0.0) (real 1.0) = false_);
  assert (relop Ty_real Ge (real 0.0) (real 1.0) = false_)

(* str *)
let () =
  assert (relop Ty_str Lt (str "a") (str "b") = true_);
  assert (relop Ty_str Le (str "a") (str "b") = true_);
  assert (relop Ty_str Gt (str "a") (str "b") = false_);
  assert (relop Ty_str Ge (str "a") (str "b") = false_);
  assert (relop Ty_str Eq (str "a") (str "a") = true_);
  assert (relop Ty_str Ne (str "a") (str "a") = false_);
  assert (relop Ty_str Eq (str "a") (str "b") = false_);
  assert (relop Ty_str Ne (str "a") (str "b") = true_)

(* i32 *)
let () =
  assert (relop (Ty_bitv 32) Lt (int32 0l) (int32 1l) = true_);
  assert (relop (Ty_bitv 32) LtU (int32 (-1l)) (int32 0l) = false_);
  assert (relop (Ty_bitv 32) Le (int32 0l) (int32 1l) = true_);
  assert (relop (Ty_bitv 32) LeU (int32 (-1l)) (int32 0l) = false_);
  assert (relop (Ty_bitv 32) Gt (int32 1l) (int32 0l) = true_);
  assert (relop (Ty_bitv 32) GtU (int32 0l) (int32 (-1l)) = false_);
  assert (relop (Ty_bitv 32) Ge (int32 1l) (int32 0l) = true_);
  assert (relop (Ty_bitv 32) GeU (int32 0l) (int32 (-1l)) = false_)

(* i64 *)
let () =
  assert (relop (Ty_bitv 64) Lt (int64 0L) (int64 1L) = true_);
  assert (relop (Ty_bitv 64) LtU (int64 (-1L)) (int64 0L) = false_);
  assert (relop (Ty_bitv 64) Le (int64 0L) (int64 1L) = true_);
  assert (relop (Ty_bitv 64) LeU (int64 (-1L)) (int64 0L) = false_);
  assert (relop (Ty_bitv 64) Gt (int64 1L) (int64 0L) = true_);
  assert (relop (Ty_bitv 64) GtU (int64 0L) (int64 (-1L)) = false_);
  assert (relop (Ty_bitv 64) Ge (int64 1L) (int64 0L) = true_);
  assert (relop (Ty_bitv 64) GeU (int64 0L) (int64 (-1L)) = false_)

(* f32 *)
let () =
  assert (relop (Ty_fp 32) Lt (float32 0.0) (float32 1.0) = true_);
  assert (relop (Ty_fp 32) Le (float32 0.0) (float32 1.0) = true_);
  assert (relop (Ty_fp 32) Gt (float32 0.0) (float32 1.0) = false_);
  assert (relop (Ty_fp 32) Ge (float32 0.0) (float32 1.0) = false_)

(* f64 *)
let () =
  assert (relop (Ty_fp 64) Lt (float64 0.0) (float64 1.0) = true_);
  assert (relop (Ty_fp 64) Le (float64 0.0) (float64 1.0) = true_);
  assert (relop (Ty_fp 64) Gt (float64 0.0) (float64 1.0) = false_);
  assert (relop (Ty_fp 64) Ge (float64 0.0) (float64 1.0) = false_)

(* app *)
let () =
  assert (
    relop Ty_app Eq (app (`Op "undefined")) (app (`Op "undefined")) = true_ );
  assert (
    relop Ty_app Ne (app (`Op "undefined")) (app (`Op "undefined")) = false_ );
  assert (relop Ty_app Eq (app (`Op "undefined")) (int 1) = false_);
  assert (relop Ty_app Ne (int 1) (app (`Op "undefined")) = true_)

(* ptr *)
let () =
  let ty = Ty_bitv 32 in
  let p0 = ptr 0l (int32 0l) in
  let p1 = ptr 4l (int32 0l) in
  assert (relop Ty_bool Eq p0 p0 = true_);
  assert (relop Ty_bool Eq p0 p1 = false_);
  assert (relop Ty_bool Ne p0 p0 = false_);
  assert (relop Ty_bool Ne p0 p1 = true_);
  assert (relop ty LtU p0 p0 = false_);
  assert (relop ty LeU p0 p1 = true_);
  assert (relop ty GeU p0 p0 = true_);
  assert (relop ty GtU p0 p1 = false_);
  assert (relop ty Le p0 (int32 4l) = true_);
  assert (relop ty Lt (int32 4l) p0 = false_);
  assert (relop ty Gt p1 (int32 4l) = false_);
  assert (relop ty Ge (int32 4l) p1 = true_)

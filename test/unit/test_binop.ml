open Smtml
open Ty
open Expr
open Value

let ( = ) = Expr.equal

let true_ = value True

let false_ = value False

let int x = value (Int x)

let real x = value (Real x)

let string x = value (Str x)

let int32 x = value (Num (I32 x))

let int64 x = value (Num (I64 x))

let float32 x = value (Num (F32 (Int32.bits_of_float x)))

let float64 x = value (Num (F64 (Int64.bits_of_float x)))

let list x = value (List x)

(* int *)
let () =
  assert (binop Ty_int Add (int 0) (int 42) = int 42);
  assert (binop Ty_int Sub (int 0) (int 1) = int (-1));
  assert (binop Ty_int Mul (int 2) (int 21) = int 42);
  assert (binop Ty_int Div (int 84) (int 2) = int 42);
  assert (binop Ty_int Rem (int 0) (int 1) = int 0);
  assert (binop Ty_int Pow (int 2) (int 2) = int 4);
  assert (binop Ty_int Min (int 2) (int 4) = int 2);
  assert (binop Ty_int Max (int 2) (int 4) = int 4);
  assert (binop Ty_int And (int 1) (int 0) = int 0);
  assert (binop Ty_int Or (int 0) (int 1) = int 1);
  assert (binop Ty_int Xor (int 1) (int 1) = int 0);
  assert (binop Ty_int Shl (int 1) (int 2) = int 4);
  assert (binop Ty_int ShrA (int 4) (int 2) = int 1);
  assert (binop Ty_int ShrA (int (-4)) (int 2) = int (-1));
  assert (binop Ty_int ShrL (int (-4)) (int 2) <> int (-1))

(* real *)
let () =
  assert (binop Ty_real Add (real 0.0) (real 42.0) = real 42.0);
  assert (binop Ty_real Sub (real 0.0) (real 1.0) = real (-1.0));
  assert (binop Ty_real Mul (real 2.0) (real 21.0) = real 42.0);
  assert (binop Ty_real Div (real 84.0) (real 2.0) = real 42.0);
  assert (binop Ty_real Rem (real 0.0) (real 1.0) = real 0.0);
  assert (binop Ty_real Min (real 2.0) (real 4.0) = real 2.0);
  assert (binop Ty_real Max (real 2.0) (real 4.0) = real 4.0)

(* str *)
let () =
  assert (binop Ty_str At (string "abc") (int 0) = string "a");
  assert (binop Ty_str String_prefix (string "ab") (string "abcd") = true_);
  assert (binop Ty_str String_suffix (string "ab") (string "abcd") = false_);
  assert (binop Ty_str String_contains (string "abcd") (string "bc") = true_)

(* list *)
let () =
  let clist = list [ Int 0; Int 1; Int 2 ] in
  assert (binop Ty_list At clist (int 0) = int 0);
  assert (binop Ty_list List_cons (int 0) (list [ Int 1; Int 2 ]) = clist);
  assert (
    binop Ty_list List_append (list [ Int 0; Int 1 ]) (list [ Int 2 ]) = clist );
  let slist2 = make (List [ int 0; int 1 ]) in
  let slist3 = make (List [ int 0; int 1; int 2 ]) in
  assert (binop Ty_list At slist3 (int 0) = int 0);
  assert (binop Ty_list List_append slist2 (list [ Int 2 ]) = slist3);
  assert (
    binop Ty_list List_cons (int 0) (make (List [ int 1; int 2 ])) = slist3 )

(* i32 *)
let () =
  assert (
    let ptr = ptr 8390670l (int32 2l) in
    binop (Ty_bitv 32) Rem ptr (int32 1l) = int32 0l );
  assert (binop (Ty_bitv 32) Add (int32 0l) (int32 1l) = int32 1l);
  assert (binop (Ty_bitv 32) Sub (int32 1l) (int32 0l) = int32 1l);
  assert (
    let x = binop (Ty_bitv 32) Mul (int32 2l) (int32 2l) in
    binop (Ty_bitv 32) Div x (int32 2l) = int32 2l );
  assert (binop (Ty_bitv 32) Rem (int32 10l) (int32 7l) = int32 3l);
  assert (binop (Ty_bitv 32) And (int32 1l) (int32 0l) = int32 0l);
  assert (binop (Ty_bitv 32) Or (int32 0l) (int32 1l) = int32 1l);
  assert (binop (Ty_bitv 32) Xor (int32 1l) (int32 1l) = int32 0l);
  assert (binop (Ty_bitv 32) Shl (int32 1l) (int32 2l) = int32 4l);
  assert (binop (Ty_bitv 32) ShrA (int32 4l) (int32 2l) = int32 1l);
  assert (binop (Ty_bitv 32) Rotl (int32 Int32.min_int) (int32 2l) = int32 2l);
  assert (binop (Ty_bitv 32) Rotr (int32 2l) (int32 2l) = int32 Int32.min_int)

(* i64 *)
let () =
  assert (binop (Ty_bitv 64) Add (int64 0L) (int64 1L) = int64 1L);
  assert (binop (Ty_bitv 64) Sub (int64 1L) (int64 0L) = int64 1L);
  assert (
    let x = binop (Ty_bitv 64) Mul (int64 2L) (int64 2L) in
    binop (Ty_bitv 64) Div x (int64 2L) = int64 2L );
  assert (binop (Ty_bitv 64) Rem (int64 10L) (int64 7L) = int64 3L);
  assert (binop (Ty_bitv 64) And (int64 1L) (int64 0L) = int64 0L);
  assert (binop (Ty_bitv 64) Or (int64 0L) (int64 1L) = int64 1L);
  assert (binop (Ty_bitv 64) Xor (int64 1L) (int64 1L) = int64 0L);
  assert (binop (Ty_bitv 64) Shl (int64 1L) (int64 2L) = int64 4L);
  assert (binop (Ty_bitv 64) ShrA (int64 4L) (int64 2L) = int64 1L);
  assert (binop (Ty_bitv 64) Rotl (int64 Int64.min_int) (int64 2L) = int64 2L);
  assert (binop (Ty_bitv 64) Rotr (int64 2L) (int64 2L) = int64 Int64.min_int)

(* f32 *)
let () =
  let ty = Ty_fp 32 in
  assert (binop ty Copysign (float32 (-4.2)) (float32 2.0) = float32 4.2);
  assert (binop ty Copysign (float32 4.2) (float32 (-2.0)) = float32 (-4.2));
  assert (binop ty Copysign (float32 4.2) (float32 2.0) = float32 4.2);
  assert (binop ty Copysign (float32 (-4.2)) (float32 (-2.0)) = float32 (-4.2))

(* f64 *)
let () =
  let ty = Ty_fp 64 in
  assert (binop ty Copysign (float64 (-4.2)) (float64 2.0) = float64 4.2);
  assert (binop ty Copysign (float64 4.2) (float64 (-2.0)) = float64 (-4.2));
  assert (binop ty Copysign (float64 4.2) (float64 2.0) = float64 4.2);
  assert (binop ty Copysign (float64 (-4.2)) (float64 (-2.0)) = float64 (-4.2))

(* ptr *)
let () =
  let p0 = ptr 0l (int32 0l) in
  let p1 = binop (Ty_bitv 32) Add p0 (int32 4l) in
  assert (p1 = ptr 0l (int32 4l));
  assert (binop (Ty_bitv 32) Sub p1 p0 = int32 4l);
  assert (binop (Ty_bitv 32) Sub p1 (int32 4l) = p0);
  assert (binop (Ty_bitv 32) Add (int32 4l) p0 = p1)

(* Simplification *)
let () =
  let x = symbol @@ Symbol.make (Ty_bitv 32) "x" in
  let zero = int32 0l in
  let binop32 = binop (Ty_bitv 32) in
  assert (binop32 And x zero = zero && binop32 And zero x = zero);
  assert (binop32 Or zero x = x && binop32 Or x zero = x);
  assert (
    binop32 Sub (binop32 Sub x (int32 1l)) (int32 1l) = binop32 Sub x (int32 2l) );
  assert (
    binop32 Mul (binop32 Mul x (int32 2l)) (int32 2l) = binop32 Mul x (int32 4l) );
  assert (
    binop32 Mul (int32 2l) (binop32 Mul x (int32 2l)) = binop32 Mul (int32 4l) x )

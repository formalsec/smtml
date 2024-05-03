open Smtml
open Ty
open Expr
open Value

(* int *)
let () =
  let v i = value (Int i) in
  assert (unop Ty_int Neg (v 1) = v ~-1);
  assert (unop Ty_int Abs (v ~-1) = v 1);
  assert (unop Ty_int Neg (unop Ty_int Neg (v 1)) = v 1)

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

(* string*)
let () =
  let v s = value (Str s) in
  assert (unop Ty_str Length (v "abc") = value (Int 3));
  assert (unop Ty_str Trim (v " abc\t\n") = value (Str "abc"))

(* bool *)
let () =
  let true_ = value True in
  let false_ = value False in
  assert (unop Ty_bool Not true_ = false_);
  assert (unop Ty_bool Not (unop Ty_bool Not true_) = true_)

(* list *)
let () =
  let v l = value (List l) in
  assert (unop Ty_list Head (v [ Int 1; Int 2; Int 3 ]) = value (Int 1));
  assert (
    unop Ty_list Tail (v [ Int 1; Int 2; Int 3 ]) = value (List [ Int 2; Int 3 ]) );
  assert (unop Ty_list Length (v [ Int 1; Int 2; Int 3 ]) = value (Int 3));
  assert (
    unop Ty_list Reverse (v [ Int 1; Int 2; Int 3 ])
    = value (List [ Int 3; Int 2; Int 1 ]) );
  assert (
    unop Ty_list Reverse (unop Ty_list Reverse (v [ Int 1; Int 2; Int 3 ]))
    = v [ Int 1; Int 2; Int 3 ] )

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

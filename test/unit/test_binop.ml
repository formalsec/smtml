open Smtml
open Ty
open Expr
open Value

(* int *)
let () =
  let v i = value (Int i) in
  assert (binop Ty_int Add (v 0) (v 42) = v 42);
  assert (binop Ty_int Sub (v 0) (v 1) = v (-1));
  assert (binop Ty_int Mul (v 2) (v 21) = v 42);
  assert (binop Ty_int Div (v 84) (v 2) = v 42);
  assert (binop Ty_int Rem (v 0) (v 1) = v 0);
  assert (binop Ty_int Pow (v 2) (v 2) = v 4);
  assert (binop Ty_int Min (v 2) (v 4) = v 2);
  assert (binop Ty_int Max (v 2) (v 4) = v 4)

(* real *)
let () =
  let v i = value (Real i) in
  assert (binop Ty_real Add (v 0.0) (v 42.0) = v 42.0);
  assert (binop Ty_real Sub (v 0.0) (v 1.0) = v (-1.0));
  assert (binop Ty_real Mul (v 2.0) (v 21.0) = v 42.0);
  assert (binop Ty_real Div (v 84.0) (v 2.0) = v 42.0);
  assert (binop Ty_real Rem (v 0.0) (v 1.0) = v 0.0);
  assert (binop Ty_real Min (v 2.0) (v 4.0) = v 2.0);
  assert (binop Ty_real Max (v 2.0) (v 4.0) = v 4.0)

(* str *)
let () =
  let i i = value (Int i) in
  let v s = value (Str s) in
  assert (binop Ty_str Seq_at (v "abc") (i 0) = v "a");
  assert (binop Ty_str Seq_concat (v "ab") (v "cd") = v "abcd");
  assert (binop Ty_str Seq_prefix (v "ab") (v "abcd") = value True);
  assert (binop Ty_str Seq_suffix (v "ab") (v "abcd") = value False);
  assert (binop Ty_str Seq_contains (v "abcd") (v "bc") = value True)

(* i32 *)
let () =
  let v i = value (Num (I32 i)) in
  assert (
    let ptr = make @@ Ptr (8390670l, v 2l) in
    binop (Ty_bitv 32) Rem ptr (v 1l) = v 0l );
  assert (binop (Ty_bitv 32) Add (v 0l) (v 1l) = v 1l);
  assert (binop (Ty_bitv 32) Sub (v 1l) (v 0l) = v 1l);
  assert (
    let x = binop (Ty_bitv 32) Mul (v 2l) (v 2l) in
    binop (Ty_bitv 32) Div x (v 2l) = v 2l );
  assert (binop (Ty_bitv 32) Rem (v 10l) (v 7l) = v 3l);
  assert (binop (Ty_bitv 32) And (v 1l) (v 0l) = v 0l);
  assert (binop (Ty_bitv 32) Or (v 0l) (v 1l) = v 1l);
  assert (binop (Ty_bitv 32) Xor (v 1l) (v 1l) = v 0l);
  assert (binop (Ty_bitv 32) Shl (v 1l) (v 2l) = v 4l);
  assert (binop (Ty_bitv 32) ShrA (v 4l) (v 2l) = v 1l);
  assert (binop (Ty_bitv 32) Rotl (v Int32.min_int) (v 2l) = v 2l);
  assert (binop (Ty_bitv 32) Rotr (v 2l) (v 2l) = v Int32.min_int)

(* i64 *)
let () =
  let v i = value (Num (I64 i)) in
  assert (binop (Ty_bitv 64) Add (v 0L) (v 1L) = v 1L);
  assert (binop (Ty_bitv 64) Sub (v 1L) (v 0L) = v 1L);
  assert (
    let x = binop (Ty_bitv 64) Mul (v 2L) (v 2L) in
    binop (Ty_bitv 64) Div x (v 2L) = v 2L );
  assert (binop (Ty_bitv 64) Rem (v 10L) (v 7L) = v 3L);
  assert (binop (Ty_bitv 64) And (v 1L) (v 0L) = v 0L);
  assert (binop (Ty_bitv 64) Or (v 0L) (v 1L) = v 1L);
  assert (binop (Ty_bitv 64) Xor (v 1L) (v 1L) = v 0L);
  assert (binop (Ty_bitv 64) Shl (v 1L) (v 2L) = v 4L);
  assert (binop (Ty_bitv 64) ShrA (v 4L) (v 2L) = v 1L);
  assert (binop (Ty_bitv 64) Rotl (v Int64.min_int) (v 2L) = v 2L);
  assert (binop (Ty_bitv 64) Rotr (v 2L) (v 2L) = v Int64.min_int)

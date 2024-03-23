open Encoding
open Ty
open Expr
open Value

let int i = value (Int i)

let str s = value (Str s)

let i32 i = value (Num (I32 i))

let i64 i = value (Num (I64 i))

let f32 f = value (Num (F32 (Int32.bits_of_float f)))

let f64 f = value (Num (F64 (Int64.bits_of_float f)))

(* str *)
let () =
  assert (cvtop Ty_str String_to_code (str "a") = int 97);
  assert (cvtop Ty_str String_from_code (int 97) = str "a");
  assert (cvtop Ty_str String_to_int (str "42") = int 42);
  assert (cvtop Ty_str String_from_int (int 42) = str "42")

(* i32 *)
let () =
  assert (cvtop (Ty_bitv 32) TruncSF32 (f32 8.5) = i32 8l);
  assert (cvtop (Ty_bitv 32) TruncSF64 (f64 8.5) = i32 8l)

(* i64 *)
let () =
  assert (cvtop (Ty_bitv 64) TruncSF32 (f32 8.5) = i64 8L);
  assert (cvtop (Ty_bitv 64) TruncSF64 (f64 8.5) = i64 8L)

(* f32 *)
let () =
  assert (cvtop (Ty_fp 32) ConvertSI32 (i32 8l) = f32 8.0);
  assert (cvtop (Ty_fp 32) ConvertSI64 (i64 8L) = f32 8.0)

(* f64 *)
let () =
  assert (cvtop (Ty_fp 64) ConvertSI32 (i32 8l) = f64 8.0);
  assert (cvtop (Ty_fp 64) ConvertSI64 (i64 8L) = f64 8.0)

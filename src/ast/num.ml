(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2024 formalsec *)
(* Written by the Smtml programmers *)

type t =
  | I8 of int
  | I32 of int32
  | I64 of int64
  | F32 of int32
  | F64 of int64

type printer =
  [ `Pretty
  | `Hexadecimal
  ]

let compare n1 n2 =
  match (n1, n2) with
  | I8 i1, I8 i2 -> Int.compare i1 i2
  | I32 i1, I32 i2 -> Int32.compare i1 i2
  | I64 i1, I64 i2 -> Int64.compare i1 i2
  | F32 i1, F32 i2 ->
    Float.compare (Int32.float_of_bits i1) (Int32.float_of_bits i2)
  | F64 i1, F64 i2 ->
    Float.compare (Int64.float_of_bits i1) (Int64.float_of_bits i2)
  | I8 _, _ -> -1
  | I32 _, I8 _ -> 1
  | I32 _, _ -> -1
  | I64 _, (I8 _ | I32 _) -> 1
  | I64 _, _ -> -1
  | F32 _, (I8 _ | I32 _ | I64 _) -> 1
  | F32 _, F64 _ -> -1
  | F64 _, _ -> 1

let equal (n1 : t) (n2 : t) : bool = compare n1 n2 = 0

let num_of_bool (b : bool) : t = I32 (if b then 1l else 0l)

let pp_num fmt (n : t) =
  match n with
  | I8 i -> Fmt.pf fmt "(i8 %d)" i
  | I32 i -> Fmt.pf fmt "(i32 %ld)" i
  | I64 i -> Fmt.pf fmt "(i64 %Ld)" i
  | F32 f -> Fmt.pf fmt "(f32 %F)" (Int32.float_of_bits f)
  | F64 f -> Fmt.pf fmt "(f64 %F)" (Int64.float_of_bits f)

let pp_hex fmt (n : t) =
  match n with
  | I8 i -> Fmt.pf fmt "0x%02x" (i land 0xff)
  | I32 i -> Fmt.pf fmt "0x%08lx" i
  | I64 i -> Fmt.pf fmt "0x%016Lx" i
  | F32 f -> Fmt.pf fmt "(fp 0x%08lx)" f
  | F64 f -> Fmt.pf fmt "(fp 0x%016Lx)" f

let printer = ref pp_num

let set_default_printer = function
  | `Pretty -> printer := pp_num
  | `Hexadecimal -> printer := pp_hex

let pp fmt v = !printer fmt v

let to_string (n : t) : string = Fmt.str "%a" pp n

let to_json (n : t) : Yojson.Basic.t = `String (to_string n)

let type_of (n : t) =
  match n with
  | I8 _ -> Ty.(Ty_bitv 8)
  | I32 _ -> Ty.(Ty_bitv 32)
  | I64 _ -> Ty.(Ty_bitv 64)
  | F32 _ -> Ty.(Ty_fp 32)
  | F64 _ -> Ty.(Ty_fp 64)

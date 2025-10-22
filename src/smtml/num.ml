(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2024 formalsec *)
(* Written by the Smtml programmers *)

type t =
  | F32 of int32
  | F64 of int64

type printer =
  [ `Pretty
  | `Full
  | `Hexadecimal
  ]

let type_of (n : t) =
  match n with F32 _ -> Ty.(Ty_fp 32) | F64 _ -> Ty.(Ty_fp 64)

let compare n1 n2 =
  match (n1, n2) with
  | F32 i1, F32 i2 ->
    Float.compare (Int32.float_of_bits i1) (Int32.float_of_bits i2)
  | F64 i1, F64 i2 ->
    Float.compare (Int64.float_of_bits i1) (Int64.float_of_bits i2)
  | F32 _, F64 _ -> -1
  | F64 _, _ -> 1

let equal (n1 : t) (n2 : t) : bool = compare n1 n2 = 0

let pp_num fmt = function
  | F32 f -> Fmt.pf fmt "(f32 %F)" (Int32.float_of_bits f)
  | F64 f -> Fmt.pf fmt "(f64 %F)" (Int64.float_of_bits f)

let pp_num_full fmt = function
  | F32 f -> Fmt.pf fmt "(f32 %f)" (Int32.float_of_bits f)
  | F64 f -> Fmt.pf fmt "(f64 %f)" (Int64.float_of_bits f)

let pp_hex fmt = function
  | F32 f -> Fmt.pf fmt "(fp 0x%08lx)" f
  | F64 f -> Fmt.pf fmt "(fp 0x%016Lx)" f

let pp_no_type fmt = function
  | F32 f -> Fmt.pf fmt "%F" (Int32.float_of_bits f)
  | F64 f -> Fmt.pf fmt "%F" (Int64.float_of_bits f)

let printer = ref pp_no_type

let set_default_printer = function
  | `Pretty -> printer := pp_num
  | `Full -> printer := pp_num_full
  | `Hexadecimal -> printer := pp_hex

let pp fmt v = !printer fmt v

let to_string (n : t) : string = Fmt.str "%a" pp n

let of_string (cast : Ty.t) value =
  match cast with
  | Ty_fp 32 -> (
    match float_of_string_opt value with
    | None -> Fmt.error_msg "invalid value %s, expected float" value
    | Some n -> Ok (F32 (Int32.bits_of_float n)) )
  | Ty_fp 64 -> (
    match float_of_string_opt value with
    | None -> Fmt.error_msg "invalid value %s, expected float" value
    | Some n -> Ok (F64 (Int64.bits_of_float n)) )
  | _ -> Fmt.error_msg "invalid value, expected num"

let to_json (n : t) : Yojson.Basic.t = `String (Fmt.str "%a" pp_no_type n)

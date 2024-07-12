(***************************************************************************)
(* This file is part of the third-party OCaml library `smtml`.             *)
(* Copyright (C) 2023-2024 formalsec                                       *)
(*                                                                         *)
(* This program is free software: you can redistribute it and/or modify    *)
(* it under the terms of the GNU General Public License as published by    *)
(* the Free Software Foundation, either version 3 of the License, or       *)
(* (at your option) any later version.                                     *)
(*                                                                         *)
(* This program is distributed in the hope that it will be useful,         *)
(* but WITHOUT ANY WARRANTY; without even the implied warranty of          *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the           *)
(* GNU General Public License for more details.                            *)
(*                                                                         *)
(* You should have received a copy of the GNU General Public License       *)
(* along with this program.  If not, see <https://www.gnu.org/licenses/>.  *)
(***************************************************************************)

let pp_string = Format.pp_print_string

let fprintf = Format.fprintf

type _ cast =
  | C8 : int cast
  | C32 : int32 cast
  | C64 : int64 cast

type t =
  | Ty_int
  | Ty_real
  | Ty_bool
  | Ty_str
  | Ty_bitv of int
  | Ty_fp of int
  | Ty_list
  | Ty_app
  | Ty_unit

type 'a op =
  | Neg : [ `Unary ] op
  | Not : [ `Unary ] op
  | Clz : [ `Unary ] op
  | Ctz : [ `Unary ] op
  | Abs : [ `Unary ] op
  | Sqrt : [ `Unary ] op
  | Is_nan : [ `Unary ] op
  | Ceil : [ `Unary ] op
  | Floor : [ `Unary ] op
  | Trunc : [ `Unary ] op
  | Nearest : [ `Unary ] op
  | Length : [ `Unary ] op
  | List_head : [ `Unary ] op
  | List_tail : [ `Unary ] op
  | List_reverse : [ `Unary ] op
  | Add : [ `Binary ] op
  | Sub : [ `Binary ] op
  | Mul : [ `Binary ] op
  | Div : [ `Binary ] op
  | DivU : [ `Binary ] op
  | Rem : [ `Binary ] op
  | RemU : [ `Binary ] op
  | Shl : [ `Binary ] op
  | ShrA : [ `Binary ] op
  | ShrL : [ `Binary ] op
  | And : [ `Binary ] op
  | Or : [ `Binary ] op
  | Xor : [ `Binary ] op
  | Pow : [ `Binary ] op
  | Min : [ `Binary ] op
  | Max : [ `Binary ] op
  | Rotl : [ `Binary ] op
  | Rotr : [ `Binary ] op
  | At : [ `Binary ] op
  | List_append_last : [ `Binary ] op
  | List_append : [ `Binary ] op
  | Eq : [ `Binary ] op
  | Ne : [ `Binary ] op
  | Lt : [ `Binary ] op
  | LtU : [ `Binary ] op
  | Gt : [ `Binary ] op
  | GtU : [ `Binary ] op
  | Le : [ `Binary ] op
  | LeU : [ `Binary ] op
  | Ge : [ `Binary ] op
  | GeU : [ `Binary ] op
  | Ite : [ `Ternary ] op
  | List_set : [ `Ternary ] op
  | ToString : [ `Unary ] op
  | OfString : [ `Unary ] op
  | ToBool : [ `Unary ] op
  | OfBool : [ `Unary ] op
  | Reinterpret_int : [ `Unary ] op
  | Reinterpret_float : [ `Unary ] op
  | DemoteF64 : [ `Unary ] op
  | PromoteF32 : [ `Unary ] op
  | ConvertSI32 : [ `Unary ] op
  | ConvertUI32 : [ `Unary ] op
  | ConvertSI64 : [ `Unary ] op
  | ConvertUI64 : [ `Unary ] op
  | TruncSF32 : [ `Unary ] op
  | TruncUF32 : [ `Unary ] op
  | TruncSF64 : [ `Unary ] op
  | TruncUF64 : [ `Unary ] op
  | WrapI64 : [ `Unary ] op
  | Sign_extend : int -> [ `Unary ] op
  | Zero_extend : int -> [ `Unary ] op
  | Logand : [ `Nary ] op
  | Logor : [ `Nary ] op
  | Concat : [ `Nary ] op
  | String_trim : [ `Unary ] op
  (* Binop *)
  | String_prefix : [ `Binary ] op
  | String_suffix : [ `Binary ] op
  | String_contains : [ `Binary ] op
  | String_last_index : [ `Binary ] op
  (* Triop *)
  | String_extract : [ `Ternary ] op
  | String_replace : [ `Ternary ] op
  | String_index : [ `Ternary ] op
  (* Cvtop *)
  | String_to_code : [ `Unary ] op
  | String_from_code : [ `Unary ] op
  | String_to_int : [ `Unary ] op
  | String_from_int : [ `Unary ] op
  | String_to_float : [ `Unary ] op

type logic =
  | AUFLIA
  | AUFLIRA
  | AUFNIRA
  | LIA
  | LRA
  | QF_ABV
  | QF_AUFBV
  | QF_AUFLIA
  | QF_AX
  | QF_BV
  | QF_BVFP
  | QF_IDL
  | QF_LIA
  | QF_LRA
  | QF_NIA
  | QF_NRA
  | QF_RDL
  | QF_S
  | QF_UF
  | QF_UFBV
  | QF_UFIDL
  | QF_UFLIA
  | QF_UFLRA
  | QF_UFNRA
  | UFLRA
  | UFNIA

let pp fmt = function
  | Ty_int -> pp_string fmt "int"
  | Ty_real -> pp_string fmt "real"
  | Ty_bool -> pp_string fmt "bool"
  | Ty_str -> pp_string fmt "str"
  | Ty_bitv n -> fprintf fmt "i%d" n
  | Ty_fp n -> fprintf fmt "f%d" n
  | Ty_list -> pp_string fmt "list"
  | Ty_app -> pp_string fmt "app"
  | Ty_unit -> pp_string fmt "unit"

let pp_op (type a) fmt (op : a op) =
  match op with
  | Neg -> pp_string fmt "neg"
  | Not -> pp_string fmt "not"
  | Clz -> pp_string fmt "clz"
  | Ctz -> pp_string fmt "ctz"
  | Abs -> pp_string fmt "abs"
  | Sqrt -> pp_string fmt "sqrt"
  | Is_nan -> pp_string fmt "is_nan"
  | Ceil -> pp_string fmt "ceil"
  | Floor -> pp_string fmt "floor"
  | Trunc -> pp_string fmt "trunc"
  | Nearest -> pp_string fmt "nearest"
  | List_head -> pp_string fmt "head"
  | List_tail -> pp_string fmt "tail"
  | List_reverse -> pp_string fmt "reverse"
  | Length -> pp_string fmt "length"
  | Add -> pp_string fmt "add"
  | Sub -> pp_string fmt "sub"
  | Mul -> pp_string fmt "mul"
  | Div -> pp_string fmt "div"
  | DivU -> pp_string fmt "div_u"
  | Rem -> pp_string fmt "rem"
  | RemU -> pp_string fmt "rem_u"
  | Shl -> pp_string fmt "shl"
  | ShrA -> pp_string fmt "shr"
  | ShrL -> pp_string fmt "shr_u"
  | And -> pp_string fmt "and"
  | Or -> pp_string fmt "or"
  | Xor -> pp_string fmt "xor"
  | Pow -> pp_string fmt "pow"
  | Min -> pp_string fmt "min"
  | Max -> pp_string fmt "max"
  | Rotl -> pp_string fmt "rotl"
  | Rotr -> pp_string fmt "rotr"
  | At -> pp_string fmt "at"
  | List_append_last -> pp_string fmt "append_last"
  | List_append -> pp_string fmt "append"
  | Ite -> pp_string fmt "ite"
  | List_set -> pp_string fmt "set"
  | Eq -> pp_string fmt "eq"
  | Ne -> pp_string fmt "ne"
  | Lt -> pp_string fmt "lt"
  | LtU -> pp_string fmt "lt_u"
  | Gt -> pp_string fmt "gt"
  | GtU -> pp_string fmt "gt_u"
  | Le -> pp_string fmt "le"
  | LeU -> pp_string fmt "le_u"
  | Ge -> pp_string fmt "ge"
  | GeU -> pp_string fmt "ge_u"
  | ToString -> pp_string fmt "to_string"
  | OfString -> pp_string fmt "of_string"
  | ToBool -> pp_string fmt "to_bool"
  | OfBool -> pp_string fmt "of_bool"
  | Reinterpret_int -> pp_string fmt "reinterpret_int"
  | Reinterpret_float -> pp_string fmt "reinterpret_float"
  | DemoteF64 -> pp_string fmt "demote_f64"
  | PromoteF32 -> pp_string fmt "promote_f32"
  | ConvertSI32 -> pp_string fmt "convert_i32_s"
  | ConvertUI32 -> pp_string fmt "convert_i32_u"
  | ConvertSI64 -> pp_string fmt "convert_i64_s"
  | ConvertUI64 -> pp_string fmt "convert_i64_u"
  | TruncSF32 -> pp_string fmt "trunc_f32_s"
  | TruncUF32 -> pp_string fmt "trunc_f32_u"
  | TruncSF64 -> pp_string fmt "trunc_f64_s"
  | TruncUF64 -> pp_string fmt "trunc_f64_u"
  | WrapI64 -> pp_string fmt "wrap_i64"
  | Sign_extend sz -> fprintf fmt "extend_i%d_s" sz
  | Zero_extend sz -> fprintf fmt "extend_i%d_u" sz
  | Logand -> pp_string fmt "and"
  | Logor -> pp_string fmt "or"
  | Concat -> pp_string fmt "++"
  | String_trim -> pp_string fmt "trim"
  | String_prefix -> pp_string fmt "prefixof"
  | String_suffix -> pp_string fmt "suffixof"
  | String_contains -> pp_string fmt "contains"
  | String_last_index -> pp_string fmt "last_indexof"
  | String_extract -> pp_string fmt "substr"
  | String_replace -> pp_string fmt "replace"
  | String_index -> pp_string fmt "indexof"
  | String_to_code -> pp_string fmt "to_code"
  | String_from_code -> pp_string fmt "from_code"
  | String_to_int -> pp_string fmt "to_int"
  | String_from_int -> pp_string fmt "from_int"
  | String_to_float -> pp_string fmt "to_float"

let pp_logic fmt : logic -> unit = function
  | AUFLIA -> pp_string fmt "AUFLIA"
  | AUFLIRA -> pp_string fmt "AUFLIRA"
  | AUFNIRA -> pp_string fmt "AUFNIRA"
  | LIA -> pp_string fmt "LIA"
  | LRA -> pp_string fmt "LRA"
  | QF_ABV -> pp_string fmt "QF_ABV"
  | QF_AUFBV -> pp_string fmt "QF_AUFBV"
  | QF_AUFLIA -> pp_string fmt "QF_AUFLIA"
  | QF_AX -> pp_string fmt "QF_AX"
  | QF_BV -> pp_string fmt "QF_BV"
  | QF_BVFP -> pp_string fmt "QF_BVFP"
  | QF_IDL -> pp_string fmt "QF_IDL"
  | QF_LIA -> pp_string fmt "QF_LIA"
  | QF_LRA -> pp_string fmt "QF_LRA"
  | QF_NIA -> pp_string fmt "QF_NIA"
  | QF_NRA -> pp_string fmt "QF_NRA"
  | QF_RDL -> pp_string fmt "QF_RDL"
  | QF_S -> pp_string fmt "QF_S"
  | QF_UF -> pp_string fmt "QF_UF"
  | QF_UFBV -> pp_string fmt "QF_UFBV"
  | QF_UFIDL -> pp_string fmt "QF_UFIDL"
  | QF_UFLIA -> pp_string fmt "QF_UFLIA"
  | QF_UFLRA -> pp_string fmt "QF_UFLRA"
  | QF_UFNRA -> pp_string fmt "QF_UFNRA"
  | UFLRA -> pp_string fmt "UFLRA"
  | UFNIA -> pp_string fmt "UFNIA"

let equal t1 t2 =
  match (t1, t2) with
  | Ty_int, Ty_int | Ty_real, Ty_real | Ty_bool, Ty_bool | Ty_str, Ty_str ->
    true
  | Ty_bitv n1, Ty_bitv n2 | Ty_fp n1, Ty_fp n2 -> n1 = n2
  | _ -> false

let equal_op (type a) (op1 : a op) (op2 : a op) : bool =
  match (op1, op2) with
  | String_trim, String_trim -> true
  | String_prefix, String_prefix -> true
  | String_suffix, String_suffix -> true
  | String_contains, String_contains -> true
  | String_last_index, String_last_index -> true
  | String_extract, String_extract -> true
  | String_replace, String_replace -> true
  | String_index, String_index -> true
  | String_to_code, String_to_code -> true
  | String_from_code, String_from_code -> true
  | String_to_int, String_to_int -> true
  | String_from_int, String_from_int -> true
  | _ -> false

let string_of_type (ty : t) : string = Format.asprintf "%a" pp ty

let size (ty : t) : int =
  match ty with
  | Ty_bitv n | Ty_fp n -> n / 8
  | Ty_int | Ty_bool -> 4
  | Ty_real | Ty_str | Ty_list | Ty_app | Ty_unit -> assert false

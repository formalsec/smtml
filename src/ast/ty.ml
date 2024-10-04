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

type _ cast =
  | C8 : int cast
  | C32 : int32 cast
  | C64 : int64 cast

type t =
  | Ty_app
  | Ty_bitv of int
  | Ty_bool
  | Ty_fp of int
  | Ty_int
  | Ty_list
  | Ty_none
  | Ty_real
  | Ty_str
  | Ty_unit
  | Ty_regexp

type unop =
  | Neg
  | Not
  | Clz
  | Ctz
  (* Float *)
  | Abs
  | Sqrt
  | Is_nan
  | Ceil
  | Floor
  | Trunc
  | Nearest
  | Head
  | Tail
  | Reverse
  | Length
  (* String *)
  | Trim
  (* RegExp *)
  | Regexp_star
  | Regexp_loop of (int * int)
  | Regexp_plus
  | Regexp_opt
  | Regexp_comp

let unop_equal o1 o2 =
  match (o1, o2) with
  | Neg, Neg
  | Not, Not
  | Clz, Clz
  | Ctz, Ctz
  | Abs, Abs
  | Sqrt, Sqrt
  | Is_nan, Is_nan
  | Ceil, Ceil
  | Floor, Floor
  | Trunc, Trunc
  | Nearest, Nearest
  | Head, Head
  | Tail, Tail
  | Reverse, Reverse
  | Length, Length
  | Trim, Trim
  | Regexp_star, Regexp_star
  | Regexp_loop _, Regexp_loop _
  | Regexp_plus, Regexp_plus
  | Regexp_opt, Regexp_opt
  | Regexp_comp, Regexp_comp ->
    true
  | ( ( Neg | Not | Clz | Ctz | Abs | Sqrt | Is_nan | Ceil | Floor | Trunc
      | Nearest | Head | Tail | Reverse | Length | Trim | Regexp_star
      | Regexp_loop _ | Regexp_plus | Regexp_opt | Regexp_comp )
    , _ ) ->
    false

type binop =
  | Add
  | Sub
  | Mul
  | Div
  | DivU
  | Rem
  | RemU
  | Shl
  | ShrA
  | ShrL
  | And
  | Or
  | Xor
  | Pow
  | Min
  | Max
  | Rotl
  | Rotr
  | At
  | List_cons
  | List_append
  (* String *)
  | String_prefix
  | String_suffix
  | String_contains
  | String_last_index
  | String_in_re
  (* Regexp *)
  | Regexp_range

let binop_equal o1 o2 =
  match (o1, o2) with
  | Add, Add
  | Sub, Sub
  | Mul, Mul
  | Div, Div
  | DivU, DivU
  | Rem, Rem
  | RemU, RemU
  | Shl, Shl
  | ShrA, ShrA
  | ShrL, ShrL
  | And, And
  | Or, Or
  | Xor, Xor
  | Pow, Pow
  | Min, Min
  | Max, Max
  | Rotl, Rotl
  | Rotr, Rotr
  | At, At
  | List_cons, List_cons
  | List_append, List_append
  | String_prefix, String_prefix
  | String_suffix, String_suffix
  | String_contains, String_contains
  | String_last_index, String_last_index
  | String_in_re, String_in_re
  | Regexp_range, Regexp_range ->
    true
  | ( ( Add | Sub | Mul | Div | DivU | Rem | RemU | Shl | ShrA | ShrL | And | Or
      | Xor | Pow | Min | Max | Rotl | Rotr | At | List_cons | List_append
      | String_prefix | String_suffix | String_contains | String_last_index
      | String_in_re | Regexp_range )
    , _ ) ->
    false

type relop =
  | Eq
  | Ne
  | Lt
  | LtU
  | Gt
  | GtU
  | Le
  | LeU
  | Ge
  | GeU

let relop_equal op1 op2 =
  match (op1, op2) with
  | Eq, Eq
  | Ne, Ne
  | Lt, Lt
  | LtU, LtU
  | Gt, Gt
  | GtU, GtU
  | Le, Le
  | LeU, LeU
  | Ge, Ge
  | GeU, GeU ->
    true
  | (Eq | Ne | Lt | LtU | Gt | GtU | Le | LeU | Ge | GeU), _ -> false

type triop =
  | Ite
  | List_set
  (* String *)
  | String_extract
  | String_replace
  | String_index

let triop_equal op1 op2 =
  match (op1, op2) with
  | Ite, Ite
  | List_set, List_set
  | String_extract, String_extract
  | String_replace, String_replace
  | String_index, String_index ->
    true
  | (Ite | List_set | String_extract | String_replace | String_index), _ ->
    false

type cvtop =
  | ToString
  | OfString
  | ToBool
  | OfBool
  | Reinterpret_int
  | Reinterpret_float
  | DemoteF64
  | PromoteF32
  | ConvertSI32
  | ConvertUI32
  | ConvertSI64
  | ConvertUI64
  | TruncSF32
  | TruncUF32
  | TruncSF64
  | TruncUF64
  | WrapI64
  | Sign_extend of int
  | Zero_extend of int
  (* String *)
  | String_to_code
  | String_from_code
  | String_to_int
  | String_from_int
  | String_to_float
  | String_to_re

let cvtop_equal op1 op2 =
  match (op1, op2) with
  | ToString, ToString
  | OfString, OfString
  | ToBool, ToBool
  | OfBool, OfBool
  | Reinterpret_int, Reinterpret_int
  | Reinterpret_float, Reinterpret_float
  | DemoteF64, DemoteF64
  | PromoteF32, PromoteF32
  | ConvertSI32, ConvertSI32
  | ConvertUI32, ConvertUI32
  | ConvertSI64, ConvertSI64
  | ConvertUI64, ConvertUI64
  | TruncSF32, TruncSF32
  | TruncUF32, TruncUF32
  | TruncSF64, TruncSF64
  | TruncUF64, TruncUF64
  | WrapI64, WrapI64
  | String_to_code, String_to_code
  | String_from_code, String_from_code
  | String_to_int, String_to_int
  | String_from_int, String_from_int
  | String_to_float, String_to_float
  | String_to_re, String_to_re ->
    true
  | Sign_extend x1, Sign_extend x2 | Zero_extend x1, Zero_extend x2 -> x1 = x2
  | ( ( ToString | OfString | ToBool | OfBool | Reinterpret_int
      | Reinterpret_float | DemoteF64 | PromoteF32 | ConvertSI32 | ConvertUI32
      | ConvertSI64 | ConvertUI64 | TruncSF32 | TruncUF32 | TruncSF64
      | TruncUF64 | WrapI64 | Sign_extend _ | Zero_extend _ | String_to_code
      | String_from_code | String_to_int | String_from_int | String_to_float
      | String_to_re )
    , _ ) ->
    false

type naryop =
  | Logand
  | Logor
  | Concat
  | Regexp_union

let naryop_equal op1 op2 =
  match (op1, op2) with
  | Logand, Logand | Logor, Logor | Concat, Concat | Regexp_union, Regexp_union
    ->
    true
  | (Logand | Logor | Concat | Regexp_union), _ -> false

type logic =
  | ALL
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

let pp_unop fmt (op : unop) =
  match op with
  | Neg -> Fmt.string fmt "neg"
  | Not -> Fmt.string fmt "not"
  | Clz -> Fmt.string fmt "clz"
  | Ctz -> Fmt.string fmt "ctz"
  | Abs -> Fmt.string fmt "abs"
  | Sqrt -> Fmt.string fmt "sqrt"
  | Is_nan -> Fmt.string fmt "is_nan"
  | Ceil -> Fmt.string fmt "ceil"
  | Floor -> Fmt.string fmt "floor"
  | Trunc -> Fmt.string fmt "trunc"
  | Nearest -> Fmt.string fmt "nearest"
  | Head -> Fmt.string fmt "head"
  | Tail -> Fmt.string fmt "tail"
  | Reverse -> Fmt.string fmt "reverse"
  | Length -> Fmt.string fmt "length"
  | Trim -> Fmt.string fmt "trim"
  | Regexp_star -> Fmt.string fmt "*"
  | Regexp_loop _ -> Fmt.string fmt "loop"
  | Regexp_plus -> Fmt.string fmt "+"
  | Regexp_opt -> Fmt.string fmt "opt"
  | Regexp_comp -> Fmt.string fmt "comp"

let pp_binop fmt (op : binop) =
  match op with
  | Add -> Fmt.string fmt "add"
  | Sub -> Fmt.string fmt "sub"
  | Mul -> Fmt.string fmt "mul"
  | Div -> Fmt.string fmt "div"
  | DivU -> Fmt.string fmt "div_u"
  | Rem -> Fmt.string fmt "rem"
  | RemU -> Fmt.string fmt "rem_u"
  | Shl -> Fmt.string fmt "shl"
  | ShrA -> Fmt.string fmt "shr"
  | ShrL -> Fmt.string fmt "shr_u"
  | And -> Fmt.string fmt "and"
  | Or -> Fmt.string fmt "or"
  | Xor -> Fmt.string fmt "xor"
  | Pow -> Fmt.string fmt "pow"
  | Min -> Fmt.string fmt "min"
  | Max -> Fmt.string fmt "max"
  | Rotl -> Fmt.string fmt "rotl"
  | Rotr -> Fmt.string fmt "rotr"
  | At -> Fmt.string fmt "at"
  | List_cons -> Fmt.string fmt "cons"
  | List_append -> Fmt.string fmt "append"
  | String_prefix -> Fmt.string fmt "prefixof"
  | String_suffix -> Fmt.string fmt "suffixof"
  | String_contains -> Fmt.string fmt "contains"
  | String_last_index -> Fmt.string fmt "last_indexof"
  | String_in_re -> Fmt.string fmt "in_re"
  | Regexp_range -> Fmt.string fmt "range"

let pp_triop fmt (op : triop) =
  match op with
  | Ite -> Fmt.string fmt "ite"
  | String_extract -> Fmt.string fmt "substr"
  | String_replace -> Fmt.string fmt "replace"
  | String_index -> Fmt.string fmt "indexof"
  | List_set -> Fmt.string fmt "set"

let pp_relop fmt (op : relop) =
  match op with
  | Eq -> Fmt.string fmt "eq"
  | Ne -> Fmt.string fmt "ne"
  | Lt -> Fmt.string fmt "lt"
  | LtU -> Fmt.string fmt "lt_u"
  | Gt -> Fmt.string fmt "gt"
  | GtU -> Fmt.string fmt "gt_u"
  | Le -> Fmt.string fmt "le"
  | LeU -> Fmt.string fmt "le_u"
  | Ge -> Fmt.string fmt "ge"
  | GeU -> Fmt.string fmt "ge_u"

let pp_cvtop fmt (op : cvtop) =
  match op with
  | ToString -> Fmt.string fmt "to_string"
  | OfString -> Fmt.string fmt "of_string"
  | ToBool -> Fmt.string fmt "to_bool"
  | OfBool -> Fmt.string fmt "of_bool"
  | Reinterpret_int -> Fmt.string fmt "reinterpret_int"
  | Reinterpret_float -> Fmt.string fmt "reinterpret_float"
  | DemoteF64 -> Fmt.string fmt "demote_f64"
  | PromoteF32 -> Fmt.string fmt "promote_f32"
  | ConvertSI32 -> Fmt.string fmt "convert_i32_s"
  | ConvertUI32 -> Fmt.string fmt "convert_i32_u"
  | ConvertSI64 -> Fmt.string fmt "convert_i64_s"
  | ConvertUI64 -> Fmt.string fmt "convert_i64_u"
  | TruncSF32 -> Fmt.string fmt "trunc_f32_s"
  | TruncUF32 -> Fmt.string fmt "trunc_f32_u"
  | TruncSF64 -> Fmt.string fmt "trunc_f64_s"
  | TruncUF64 -> Fmt.string fmt "trunc_f64_u"
  | WrapI64 -> Fmt.string fmt "wrap_i64"
  | Sign_extend sz -> Fmt.pf fmt "extend_i%d_s" sz
  | Zero_extend sz -> Fmt.pf fmt "extend_i%d_u" sz
  | String_to_code -> Fmt.string fmt "to_code"
  | String_from_code -> Fmt.string fmt "from_code"
  | String_to_int -> Fmt.string fmt "to_int"
  | String_from_int -> Fmt.string fmt "from_int"
  | String_to_float -> Fmt.string fmt "to_float"
  | String_to_re -> Fmt.string fmt "to_re"

let pp_naryop fmt (op : naryop) =
  match op with
  | Logand -> Fmt.string fmt "and"
  | Logor -> Fmt.string fmt "or"
  | Concat -> Fmt.string fmt "++"
  | Regexp_union -> Fmt.string fmt "union"

let pp fmt = function
  | Ty_int -> Fmt.string fmt "int"
  | Ty_real -> Fmt.string fmt "real"
  | Ty_bool -> Fmt.string fmt "bool"
  | Ty_str -> Fmt.string fmt "str"
  | Ty_bitv n -> Fmt.pf fmt "i%d" n
  | Ty_fp n -> Fmt.pf fmt "f%d" n
  | Ty_list -> Fmt.string fmt "list"
  | Ty_app -> Fmt.string fmt "app"
  | Ty_unit -> Fmt.string fmt "unit"
  | Ty_none -> Fmt.string fmt "none"
  | Ty_regexp -> Fmt.string fmt "regexp"

let pp_logic fmt = function
  | ALL -> Fmt.string fmt "ALL"
  | AUFLIA -> Fmt.string fmt "AUFLIA"
  | AUFLIRA -> Fmt.string fmt "AUFLIRA"
  | AUFNIRA -> Fmt.string fmt "AUFNIRA"
  | LIA -> Fmt.string fmt "LIA"
  | LRA -> Fmt.string fmt "LRA"
  | QF_ABV -> Fmt.string fmt "QF_ABV"
  | QF_AUFBV -> Fmt.string fmt "QF_AUFBV"
  | QF_AUFLIA -> Fmt.string fmt "QF_AUFLIA"
  | QF_AX -> Fmt.string fmt "QF_AX"
  | QF_BV -> Fmt.string fmt "QF_BV"
  | QF_BVFP -> Fmt.string fmt "QF_BVFP"
  | QF_IDL -> Fmt.string fmt "QF_IDL"
  | QF_LIA -> Fmt.string fmt "QF_LIA"
  | QF_LRA -> Fmt.string fmt "QF_LRA"
  | QF_NIA -> Fmt.string fmt "QF_NIA"
  | QF_NRA -> Fmt.string fmt "QF_NRA"
  | QF_RDL -> Fmt.string fmt "QF_RDL"
  | QF_S -> Fmt.string fmt "QF_S"
  | QF_UF -> Fmt.string fmt "QF_UF"
  | QF_UFBV -> Fmt.string fmt "QF_UFBV"
  | QF_UFIDL -> Fmt.string fmt "QF_UFIDL"
  | QF_UFLIA -> Fmt.string fmt "QF_UFLIA"
  | QF_UFLRA -> Fmt.string fmt "QF_UFLRA"
  | QF_UFNRA -> Fmt.string fmt "QF_UFNRA"
  | UFLRA -> Fmt.string fmt "UFLRA"
  | UFNIA -> Fmt.string fmt "UFNIA"

let logic_of_string logic =
  match logic with
  | "ALL" -> Ok ALL
  | "AUFLIA" -> Ok AUFLIA
  | "AUFLIRA" -> Ok AUFLIRA
  | "AUFNIRA" -> Ok AUFNIRA
  | "LIA" -> Ok LIA
  | "LRA" -> Ok LRA
  | "QF_ABV" -> Ok QF_ABV
  | "QF_AUFBV" -> Ok QF_AUFBV
  | "QF_AUFLIA" -> Ok QF_AUFLIA
  | "QF_AX" -> Ok QF_AX
  | "QF_BV" -> Ok QF_BV
  | "QF_BVFP" -> Ok QF_BVFP
  | "QF_IDL" -> Ok QF_IDL
  | "QF_LIA" -> Ok QF_LIA
  | "QF_LRA" -> Ok QF_LRA
  | "QF_NIA" -> Ok QF_NIA
  | "QF_NRA" -> Ok QF_NRA
  | "QF_RDL" -> Ok QF_RDL
  | "QF_S" -> Ok QF_S
  | "QF_UF" -> Ok QF_UF
  | "QF_UFBV" -> Ok QF_UFBV
  | "QF_UFIDL" -> Ok QF_UFIDL
  | "QF_UFLIA" -> Ok QF_UFLIA
  | "QF_UFLRA" -> Ok QF_UFLRA
  | "QF_UFNRA" -> Ok QF_UFNRA
  | "UFLRA" -> Ok UFLRA
  | "UFNIA" -> Ok UFNIA
  | _ -> Error (`Msg (Fmt.str "unknown logic %s" logic))

let discr = function
  | Ty_app -> 0
  | Ty_bool -> 1
  | Ty_int -> 2
  | Ty_list -> 3
  | Ty_none -> 4
  | Ty_real -> 5
  | Ty_str -> 6
  | Ty_unit -> 7
  | Ty_regexp -> 8
  | Ty_bitv n -> 9 + n
  | Ty_fp n -> 10 + n

let compare t1 t2 = compare (discr t1) (discr t2)

let equal t1 t2 = compare t1 t2 = 0

let string_of_type (ty : t) : string = Fmt.str "%a" pp ty

let size (ty : t) : int =
  match ty with
  | Ty_bitv n | Ty_fp n -> n / 8
  | Ty_int | Ty_bool -> 4
  | Ty_real | Ty_str | Ty_list | Ty_app | Ty_unit | Ty_none | Ty_regexp ->
    assert false

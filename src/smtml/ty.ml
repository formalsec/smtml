(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2024 formalsec *)
(* Written by the Smtml programmers *)

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
  | Ty_roundingMode

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
  | Ty_roundingMode -> 9
  | Ty_bitv n -> 10 + n
  | Ty_fp n -> 11 + n

let compare t1 t2 = compare (discr t1) (discr t2)

let equal t1 t2 = compare t1 t2 = 0

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
  | Ty_roundingMode -> Fmt.string fmt "RoudingMode"

let string_of_type (ty : t) : string = Fmt.str "%a" pp ty

let of_string = function
  | "int" -> Ok Ty_int
  | "real" -> Ok Ty_real
  | "bool" -> Ok Ty_bool
  | "str" -> Ok Ty_str
  | "list" -> Ok Ty_list
  | "app" -> Ok Ty_app
  | "unit" -> Ok Ty_unit
  | "none" -> Ok Ty_none
  | "regexp" -> Ok Ty_regexp
  | "RoundingMode" -> Ok Ty_roundingMode
  | s ->
    if String.starts_with ~prefix:"i" s then begin
      let s = String.sub s 1 (String.length s - 1) in
      match int_of_string_opt s with
      | None -> Fmt.error_msg "can not parse type %s" s
      | Some n when n < 0 ->
        Fmt.error_msg "size of bitvectors must be a positive integer"
      | Some n -> Ok (Ty_bitv n)
    end
    else if String.starts_with ~prefix:"f" s then begin
      let s = String.sub s 1 (String.length s - 1) in
      match int_of_string_opt s with
      | None -> Fmt.error_msg "can not parse type %s" s
      | Some n when n < 0 ->
        Fmt.error_msg "size of fp must be a positive integer"
      | Some n -> Ok (Ty_fp n)
    end
    else Fmt.error_msg "can not parse type %s" s

let size (ty : t) : int =
  match ty with
  | Ty_bitv n | Ty_fp n -> n / 8
  | Ty_int | Ty_bool -> 4
  | Ty_real | Ty_str | Ty_list | Ty_app | Ty_unit | Ty_none | Ty_regexp
  | Ty_roundingMode ->
    assert false

module Unop = struct
  type t =
    | Neg
    | Not
    | Clz
    | Ctz
    | Popcnt
    (* Float *)
    | Abs
    | Sqrt
    | Is_normal
    | Is_subnormal
    | Is_negative
    | Is_positive
    | Is_infinite
    | Is_nan
    | Is_zero
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

  let equal o1 o2 =
    match (o1, o2) with
    | Neg, Neg
    | Not, Not
    | Clz, Clz
    | Popcnt, Popcnt
    | Ctz, Ctz
    | Abs, Abs
    | Sqrt, Sqrt
    | Is_normal, Is_normal
    | Is_subnormal, Is_subnormal
    | Is_negative, Is_negative
    | Is_positive, Is_positive
    | Is_infinite, Is_infinite
    | Is_nan, Is_nan
    | Is_zero, Is_zero
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
    | ( ( Neg | Not | Clz | Popcnt | Ctz | Abs | Sqrt | Is_normal | Is_subnormal
        | Is_negative | Is_positive | Is_infinite | Is_nan | Is_zero | Ceil
        | Floor | Trunc | Nearest | Head | Tail | Reverse | Length | Trim
        | Regexp_star | Regexp_loop _ | Regexp_plus | Regexp_opt | Regexp_comp
          )
      , _ ) ->
      false

  let compare (e1 : t) (e2 : t) =
    match (e1, e2) with
    | x, y when equal x y -> 0
    | Neg, _ -> 1
    | _, Neg -> -1
    | Not, _ -> 1
    | _, Not -> -1
    | Clz, _ -> 1
    | _, Clz -> -1
    | Ctz, _ -> 1
    | _, Ctz -> -1
    | Popcnt, _ -> 1
    | _, Popcnt -> -1
    | Abs, _ -> 1
    | _, Abs -> -1
    | Sqrt, _ -> 1
    | _, Sqrt -> -1
    | Is_normal, _ -> 1
    | _, Is_normal -> -1
    | Is_subnormal, _ -> 1
    | _, Is_subnormal -> -1
    | Is_negative, _ -> 1
    | _, Is_negative -> -1
    | Is_positive, _ -> 1
    | _, Is_positive -> -1
    | Is_infinite, _ -> 1
    | _, Is_infinite -> -1
    | Is_nan, _ -> 1
    | _, Is_nan -> -1
    | Is_zero, _ -> 1
    | _, Is_zero -> -1
    | Ceil, _ -> 1
    | _, Ceil -> -1
    | Floor, _ -> 1
    | _, Floor -> -1
    | Trunc, _ -> 1
    | _, Trunc -> -1
    | Nearest, _ -> 1
    | _, Nearest -> -1
    | Head, _ -> 1
    | _, Head -> -1
    | Tail, _ -> 1
    | _, Tail -> -1
    | Reverse, _ -> 1
    | _, Reverse -> -1
    | Length, _ -> 1
    | _, Length -> -1
    | Trim, _ -> 1
    | _, Trim -> -1
    | Regexp_star, _ -> 1
    | _, Regexp_star -> -1
    | Regexp_loop (_, _), _ -> 1
    | _, Regexp_loop (_, _) -> -1
    | Regexp_plus, _ -> 1
    | _, Regexp_plus -> -1
    | Regexp_opt, _ -> 1
    | _, Regexp_opt -> -1
    | Regexp_comp, _ -> 1

  let pp fmt = function
    | Neg -> Fmt.string fmt "neg"
    | Not -> Fmt.string fmt "not"
    | Clz -> Fmt.string fmt "clz"
    | Ctz -> Fmt.string fmt "ctz"
    | Popcnt -> Fmt.string fmt "popcnt"
    | Abs -> Fmt.string fmt "abs"
    | Sqrt -> Fmt.string fmt "sqrt"
    | Is_normal -> Fmt.string fmt "isNormal"
    | Is_subnormal -> Fmt.string fmt "isSubnormal"
    | Is_negative -> Fmt.string fmt "isNegative"
    | Is_positive -> Fmt.string fmt "isPositive"
    | Is_infinite -> Fmt.string fmt "isInfinite"
    | Is_nan -> Fmt.string fmt "is_nan"
    | Is_zero -> Fmt.string fmt "isZero"
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
end

module Binop = struct
  type t =
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
    | Implies
    | Pow
    | Min
    | Max
    | Copysign
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
    | Regexp_inter

  let equal o1 o2 =
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
    | Implies, Implies
    | Pow, Pow
    | Min, Min
    | Max, Max
    | Copysign, Copysign
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
    | Regexp_range, Regexp_range
    | Regexp_inter, Regexp_inter ->
      true
    | ( ( Add | Sub | Mul | Div | DivU | Rem | RemU | Shl | ShrA | ShrL | And
        | Or | Xor | Implies | Pow | Min | Max | Copysign | Rotl | Rotr | At
        | List_cons | List_append | String_prefix | String_suffix
        | String_contains | String_last_index | String_in_re | Regexp_range
        | Regexp_inter )
      , _ ) ->
      false

  let compare (e1 : t) (e2 : t) =
    match (e1, e2) with
    | x, y when equal x y -> 0
    | Add, _ -> 1
    | _, Add -> -1
    | Sub, _ -> 1
    | _, Sub -> -1
    | Mul, _ -> 1
    | _, Mul -> -1
    | Div, _ -> 1
    | _, Div -> -1
    | DivU, _ -> 1
    | _, DivU -> -1
    | Rem, _ -> 1
    | _, Rem -> -1
    | RemU, _ -> 1
    | _, RemU -> -1
    | Shl, _ -> 1
    | _, Shl -> -1
    | ShrA, _ -> 1
    | _, ShrA -> -1
    | ShrL, _ -> 1
    | _, ShrL -> -1
    | And, _ -> 1
    | _, And -> -1
    | Or, _ -> 1
    | _, Or -> -1
    | Xor, _ -> 1
    | _, Xor -> -1
    | Implies, _ -> 1
    | _, Implies -> -1
    | Pow, _ -> 1
    | _, Pow -> -1
    | Min, _ -> 1
    | _, Min -> -1
    | Max, _ -> 1
    | _, Max -> -1
    | Copysign, _ -> 1
    | _, Copysign -> -1
    | Rotl, _ -> 1
    | _, Rotl -> -1
    | Rotr, _ -> 1
    | _, Rotr -> -1
    | At, _ -> 1
    | _, At -> -1
    | List_cons, _ -> 1
    | _, List_cons -> -1
    | List_append, _ -> 1
    | _, List_append -> -1
    | String_prefix, _ -> 1
    | _, String_prefix -> -1
    | String_suffix, _ -> 1
    | _, String_suffix -> -1
    | String_contains, _ -> 1
    | _, String_contains -> -1
    | String_last_index, _ -> 1
    | _, String_last_index -> -1
    | String_in_re, _ -> 1
    | _, String_in_re -> -1
    | Regexp_range, _ -> 1
    | _, Regexp_range -> -1
    | Regexp_inter, _ -> 1

  let pp fmt = function
    | Add -> Fmt.string fmt "add"
    | Sub -> Fmt.string fmt "sub"
    | Mul -> Fmt.string fmt "mul"
    | Div -> Fmt.string fmt "div_s"
    | DivU -> Fmt.string fmt "div_u"
    | Rem -> Fmt.string fmt "rem_s"
    | RemU -> Fmt.string fmt "rem_u"
    | Shl -> Fmt.string fmt "shl"
    | ShrA -> Fmt.string fmt "shr_s"
    | ShrL -> Fmt.string fmt "shr_u"
    | And -> Fmt.string fmt "and"
    | Or -> Fmt.string fmt "or"
    | Xor -> Fmt.string fmt "xor"
    | Implies -> Fmt.string fmt "=>"
    | Pow -> Fmt.string fmt "pow"
    | Min -> Fmt.string fmt "min"
    | Max -> Fmt.string fmt "max"
    | Copysign -> Fmt.string fmt "copysign"
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
    | Regexp_inter -> Fmt.string fmt "inter"
end

module Relop = struct
  type t =
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

  let equal op1 op2 =
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

  let compare (e1 : t) (e2 : t) =
    match (e1, e2) with
    | x, y when equal x y -> 0
    | Eq, _ -> 1
    | _, Eq -> -1
    | Ne, _ -> 1
    | _, Ne -> -1
    | Lt, _ -> 1
    | _, Lt -> -1
    | LtU, _ -> 1
    | _, LtU -> -1
    | Gt, _ -> 1
    | _, Gt -> -1
    | GtU, _ -> 1
    | _, GtU -> -1
    | Le, _ -> 1
    | _, Le -> -1
    | LeU, _ -> 1
    | _, LeU -> -1
    | Ge, _ -> 1
    | _, Ge -> -1
    | GeU, _ -> 1

  let pp fmt = function
    | Eq -> Fmt.string fmt "eq"
    | Ne -> Fmt.string fmt "ne"
    | Lt -> Fmt.string fmt "lt_s"
    | LtU -> Fmt.string fmt "lt_u"
    | Gt -> Fmt.string fmt "gt_s"
    | GtU -> Fmt.string fmt "gt_u"
    | Le -> Fmt.string fmt "le_s"
    | LeU -> Fmt.string fmt "le_u"
    | Ge -> Fmt.string fmt "ge_s"
    | GeU -> Fmt.string fmt "ge_u"
end

module Triop = struct
  type t =
    | Ite
    | List_set
    (* String *)
    | String_extract
    | String_replace
    | String_index
    | String_replace_all

  let equal op1 op2 =
    match (op1, op2) with
    | Ite, Ite
    | List_set, List_set
    | String_extract, String_extract
    | String_replace, String_replace
    | String_index, String_index
    | String_replace_all, String_replace_all ->
      true
    | ( ( Ite | List_set | String_extract | String_replace | String_index
        | String_replace_all )
      , _ ) ->
      false

  let compare (e1 : t) (e2 : t) =
    match (e1, e2) with
    | x, y when equal x y -> 0
    | Ite, _ -> 1
    | _, Ite -> -1
    | List_set, _ -> 1
    | _, List_set -> -1
    | String_extract, _ -> 1
    | _, String_extract -> -1
    | String_replace, _ -> 1
    | _, String_replace -> -1
    | String_index, _ -> 1
    | _, String_index -> -1
    | String_replace_all, _ -> 1

  let pp fmt = function
    | Ite -> Fmt.string fmt "ite"
    | String_extract -> Fmt.string fmt "substr"
    | String_replace -> Fmt.string fmt "replace"
    | String_index -> Fmt.string fmt "indexof"
    | String_replace_all -> Fmt.string fmt "replace_all"
    | List_set -> Fmt.string fmt "set"
end

module Cvtop = struct
  type t =
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
    | Trunc_sat_f32_s
    | Trunc_sat_f32_u
    | Trunc_sat_f64_s
    | Trunc_sat_f64_u
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

  let equal op1 op2 =
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
    | Trunc_sat_f32_s, Trunc_sat_f32_s
    | Trunc_sat_f32_u, Trunc_sat_f32_u
    | Trunc_sat_f64_s, Trunc_sat_f64_s
    | Trunc_sat_f64_u, Trunc_sat_f64_u
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
        | TruncUF64 | Trunc_sat_f32_s | Trunc_sat_f32_u | Trunc_sat_f64_s
        | Trunc_sat_f64_u | WrapI64 | Sign_extend _ | Zero_extend _
        | String_to_code | String_from_code | String_to_int | String_from_int
        | String_to_float | String_to_re )
      , _ ) ->
      false

  let compare (e1 : t) (e2 : t) =
    match (e1, e2) with
    | x, y when equal x y -> 0
    | ToString, _ -> 1
    | _, ToString -> -1
    | OfString, _ -> 1
    | _, OfString -> -1
    | ToBool, _ -> 1
    | _, ToBool -> -1
    | OfBool, _ -> 1
    | _, OfBool -> -1
    | Reinterpret_int, _ -> 1
    | _, Reinterpret_int -> -1
    | Reinterpret_float, _ -> 1
    | _, Reinterpret_float -> -1
    | DemoteF64, _ -> 1
    | _, DemoteF64 -> -1
    | PromoteF32, _ -> 1
    | _, PromoteF32 -> -1
    | ConvertSI32, _ -> 1
    | _, ConvertSI32 -> -1
    | ConvertUI32, _ -> 1
    | _, ConvertUI32 -> -1
    | ConvertSI64, _ -> 1
    | _, ConvertSI64 -> -1
    | ConvertUI64, _ -> 1
    | _, ConvertUI64 -> -1
    | TruncSF32, _ -> 1
    | _, TruncSF32 -> -1
    | TruncUF32, _ -> 1
    | _, TruncUF32 -> -1
    | TruncSF64, _ -> 1
    | _, TruncSF64 -> -1
    | TruncUF64, _ -> 1
    | _, TruncUF64 -> -1
    | Trunc_sat_f32_s, _ -> 1
    | _, Trunc_sat_f32_s -> -1
    | Trunc_sat_f32_u, _ -> 1
    | _, Trunc_sat_f32_u -> -1
    | Trunc_sat_f64_s, _ -> 1
    | _, Trunc_sat_f64_s -> -1
    | Trunc_sat_f64_u, _ -> 1
    | _, Trunc_sat_f64_u -> -1
    | WrapI64, _ -> 1
    | _, WrapI64 -> -1
    | Sign_extend _, _ -> 1
    | _, Sign_extend _ -> -1
    | Zero_extend _, _ -> 1
    | _, Zero_extend _ -> -1
    | String_to_code, _ -> 1
    | _, String_to_code -> -1
    | String_from_code, _ -> 1
    | _, String_from_code -> -1
    | String_to_int, _ -> 1
    | _, String_to_int -> -1
    | String_from_int, _ -> 1
    | _, String_from_int -> -1
    | String_to_float, _ -> 1
    | _, String_to_float -> -1
    | String_to_re, _ -> 1

  let pp fmt = function
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
    | Trunc_sat_f32_s -> Fmt.string fmt "trunc_sat_f32_s"
    | Trunc_sat_f32_u -> Fmt.string fmt "trunc_sat_f32_u"
    | Trunc_sat_f64_s -> Fmt.string fmt "trunc_sat_f64_s"
    | Trunc_sat_f64_u -> Fmt.string fmt "trunc_sat_f64_u"
    | WrapI64 -> Fmt.string fmt "wrap_i64"
    | Sign_extend sz -> Fmt.pf fmt "extend_i%d_s" sz
    | Zero_extend sz -> Fmt.pf fmt "extend_i%d_u" sz
    | String_to_code -> Fmt.string fmt "to_code"
    | String_from_code -> Fmt.string fmt "from_code"
    | String_to_int -> Fmt.string fmt "to_int"
    | String_from_int -> Fmt.string fmt "from_int"
    | String_to_float -> Fmt.string fmt "to_float"
    | String_to_re -> Fmt.string fmt "to_re"
end

module Naryop = struct
  type t =
    | Logand
    | Logor
    | Concat
    | Regexp_union

  let equal op1 op2 =
    match (op1, op2) with
    | Logand, Logand
    | Logor, Logor
    | Concat, Concat
    | Regexp_union, Regexp_union ->
      true
    | (Logand | Logor | Concat | Regexp_union), _ -> false

  let compare (e1 : t) (e2 : t) =
    match (e1, e2) with
    | x, y when equal x y -> 0
    | Logand, _ -> 1
    | _, Logand -> -1
    | Logor, _ -> 1
    | _, Logor -> -1
    | Concat, _ -> 1
    | _, Concat -> -1
    | Regexp_union, _ -> 1

  let pp fmt = function
    | Logand -> Fmt.string fmt "and"
    | Logor -> Fmt.string fmt "or"
    | Concat -> Fmt.string fmt "++"
    | Regexp_union -> Fmt.string fmt "union"
end

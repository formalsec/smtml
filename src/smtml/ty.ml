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

(* Optimized mixer (DJB2 variant). Inlines to simple arithmetic. *)
let[@inline] combine h v = (h * 33) + v

let hash = function
  | Ty_app -> 1
  | Ty_bitv width -> combine 2 width
  | Ty_bool -> 3
  | Ty_fp width -> combine 4 width
  | Ty_int -> 5
  | Ty_list -> 6
  | Ty_none -> 7
  | Ty_real -> 8
  | Ty_str -> 9
  | Ty_unit -> 10
  | Ty_regexp -> 11
  | Ty_roundingMode -> 12

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
  [@@deriving ord]

  let hash = function
    | Neg -> 0
    | Not -> 1
    | Clz -> 2
    | Ctz -> 3
    | Popcnt -> 4
    (* Float *)
    | Abs -> 5
    | Sqrt -> 6
    | Is_normal -> 7
    | Is_subnormal -> 8
    | Is_negative -> 9
    | Is_positive -> 10
    | Is_infinite -> 11
    | Is_nan -> 12
    | Is_zero -> 13
    | Ceil -> 14
    | Floor -> 15
    | Trunc -> 16
    | Nearest -> 17
    | Head -> 18
    | Tail -> 19
    | Reverse -> 20
    | Length -> 21
    (* String *)
    | Trim -> 22
    (* RegExp *)
    | Regexp_star -> 23
    | Regexp_loop (min, max) ->
      let h = 24 in
      let h = combine h min in
      combine h max
    | Regexp_plus -> 25
    | Regexp_opt -> 26
    | Regexp_comp -> 27

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
    | Regexp_diff
  [@@deriving ord]

  let hash = function
    | Add -> 0
    | Sub -> 1
    | Mul -> 2
    | Div -> 3
    | DivU -> 4
    | Rem -> 5
    | RemU -> 6
    | Shl -> 7
    | ShrA -> 8
    | ShrL -> 9
    | And -> 10
    | Or -> 11
    | Xor -> 12
    | Implies -> 13
    | Pow -> 14
    | Min -> 15
    | Max -> 16
    | Copysign -> 17
    | Rotl -> 18
    | Rotr -> 19
    | At -> 20
    | List_cons -> 21
    | List_append -> 22
    (* String *)
    | String_prefix -> 23
    | String_suffix -> 24
    | String_contains -> 25
    | String_last_index -> 26
    | String_in_re -> 27
    (* Regexp *)
    | Regexp_range -> 28
    | Regexp_inter -> 29
    | Regexp_diff -> 30

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
    | Regexp_inter, Regexp_inter
    | Regexp_diff, Regexp_diff ->
      true
    | ( ( Add | Sub | Mul | Div | DivU | Rem | RemU | Shl | ShrA | ShrL | And
        | Or | Xor | Implies | Pow | Min | Max | Copysign | Rotl | Rotr | At
        | List_cons | List_append | String_prefix | String_suffix
        | String_contains | String_last_index | String_in_re | Regexp_range
        | Regexp_inter | Regexp_diff )
      , _ ) ->
      false

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
    | Regexp_diff -> Fmt.string fmt "diff"
end

module Relop = struct
  type t =
    | Eq
    | Ne
    | Lt
    | LtU
    | Le
    | LeU
  [@@deriving ord]

  let hash = function
    | Eq -> 0
    | Ne -> 1
    | Lt -> 2
    | LtU -> 3
    | Le -> 4
    | LeU -> 5

  let equal op1 op2 =
    match (op1, op2) with
    | Eq, Eq | Ne, Ne | Lt, Lt | LtU, LtU | Le, Le | LeU, LeU -> true
    | (Eq | Ne | Lt | LtU | Le | LeU), _ -> false

  let pp fmt = function
    | Eq -> Fmt.string fmt "eq"
    | Ne -> Fmt.string fmt "ne"
    | Lt -> Fmt.string fmt "lt_s"
    | LtU -> Fmt.string fmt "lt_u"
    | Le -> Fmt.string fmt "le_s"
    | LeU -> Fmt.string fmt "le_u"
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
    | String_replace_re
    | String_replace_re_all
  [@@deriving ord]

  let hash = function
    | Ite -> 0
    | List_set -> 1
    (* String *)
    | String_extract -> 2
    | String_replace -> 3
    | String_index -> 4
    | String_replace_all -> 5
    | String_replace_re -> 6
    | String_replace_re_all -> 7

  let equal op1 op2 =
    match (op1, op2) with
    | Ite, Ite
    | List_set, List_set
    | String_extract, String_extract
    | String_replace, String_replace
    | String_index, String_index
    | String_replace_all, String_replace_all
    | String_replace_re, String_replace_re
    | String_replace_re_all, String_replace_re_all ->
      true
    | ( ( Ite | List_set | String_extract | String_replace | String_index
        | String_replace_all | String_replace_re | String_replace_re_all )
      , _ ) ->
      false

  let pp fmt = function
    | Ite -> Fmt.string fmt "ite"
    | String_extract -> Fmt.string fmt "substr"
    | String_replace -> Fmt.string fmt "replace"
    | String_index -> Fmt.string fmt "indexof"
    | String_replace_all -> Fmt.string fmt "replace_all"
    | String_replace_re -> Fmt.string fmt "replace_re"
    | String_replace_re_all -> Fmt.string fmt "replace_re_all"
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
  [@@deriving ord]

  let hash = function
    | ToString -> 0
    | OfString -> 1
    | ToBool -> 2
    | OfBool -> 3
    | Reinterpret_int -> 4
    | Reinterpret_float -> 5
    | DemoteF64 -> 6
    | PromoteF32 -> 7
    | ConvertSI32 -> 8
    | ConvertUI32 -> 9
    | ConvertSI64 -> 10
    | ConvertUI64 -> 11
    | TruncSF32 -> 12
    | TruncUF32 -> 13
    | TruncSF64 -> 14
    | TruncUF64 -> 15
    | Trunc_sat_f32_s -> 16
    | Trunc_sat_f32_u -> 17
    | Trunc_sat_f64_s -> 18
    | Trunc_sat_f64_u -> 19
    | WrapI64 -> 20
    | Sign_extend i -> combine 21 i
    | Zero_extend i -> combine 22 i
    (* String *)
    | String_to_code -> 23
    | String_from_code -> 24
    | String_to_int -> 25
    | String_from_int -> 26
    | String_to_float -> 27
    | String_to_re -> 28

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
  [@@deriving ord]

  let hash = function
    | Logand -> 0
    | Logor -> 1
    | Concat -> 2
    | Regexp_union -> 3

  let equal op1 op2 =
    match (op1, op2) with
    | Logand, Logand
    | Logor, Logor
    | Concat, Concat
    | Regexp_union, Regexp_union ->
      true
    | (Logand | Logor | Concat | Regexp_union), _ -> false

  let pp fmt = function
    | Logand -> Fmt.string fmt "and"
    | Logor -> Fmt.string fmt "or"
    | Concat -> Fmt.string fmt "++"
    | Regexp_union -> Fmt.string fmt "union"
end

module Smtlib = struct
  let pp fmt = function
    | Ty_int -> Fmt.string fmt "Int"
    | Ty_real -> Fmt.string fmt "Real"
    | Ty_bool -> Fmt.string fmt "Bool"
    | Ty_str -> Fmt.string fmt "String"
    | Ty_bitv n -> Fmt.pf fmt "(_ BitVec %d)" n
    | Ty_fp _n -> assert false
    | Ty_list -> assert false
    | Ty_app -> assert false
    | Ty_unit -> assert false
    | Ty_none -> assert false
    | Ty_regexp -> Fmt.string fmt "RegLan"
    | Ty_roundingMode -> Fmt.string fmt "RoudingMode"

  let pp_unop fmt ((ty, op) : t * Unop.t) =
    match (ty, op) with
    | Ty_bool, Not -> Fmt.string fmt "not"
    | _ -> assert false

  let pp_binop fmt ((ty, op) : t * Binop.t) =
    match (ty, op) with
    | (Ty_int | Ty_real), Add -> Fmt.string fmt "+"
    | (Ty_int | Ty_real), Sub -> Fmt.string fmt "-"
    | (Ty_int | Ty_real), Mul -> Fmt.string fmt "*"
    | (Ty_int | Ty_real), Div -> Fmt.string fmt "/"
    | Ty_str, At -> Fmt.string fmt "str.at"
    | Ty_str, String_prefix -> Fmt.string fmt "str.prefixof"
    | Ty_str, String_suffix -> Fmt.string fmt "str.suffixof"
    | Ty_str, String_contains -> Fmt.string fmt "str.contains"
    | Ty_str, String_last_index -> assert false
    | Ty_str, String_in_re -> Fmt.string fmt "str.in_re"
    | _ -> assert false

  let pp_relop fmt ((ty, op) : t * Relop.t) =
    match (ty, op) with
    | Ty_fp _, Eq -> Fmt.string fmt "fp.eq"
    | _, Eq -> Fmt.string fmt "="
    | _, Ne -> assert false
    | Ty_str, Lt -> Fmt.string fmt "str.<"
    | Ty_bitv _, Lt -> Fmt.string fmt "bvslt"
    | _, Lt -> Fmt.string fmt "<"
    | Ty_bitv _, LtU -> Fmt.string fmt "bvult"
    | _, LtU -> Fmt.string fmt "<"
    | Ty_str, Le -> Fmt.string fmt "str.<="
    | Ty_bitv _, Le -> Fmt.string fmt "bvsle"
    | _, Le -> Fmt.string fmt "<="
    | Ty_bitv _, LeU -> Fmt.string fmt "bvule"
    | _, LeU -> Fmt.string fmt ">="
end

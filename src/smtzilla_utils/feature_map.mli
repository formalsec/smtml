(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2026 formalsec *)
(* Written by the Smtml programmers *)

type feat =
  (* Expr kinds *)
  | Val
  | Ptr
  | Symbol
  | List_expr
  | App
  | Unop_expr
  | Binop_expr
  | Triop_expr
  | Relop_expr
  | Cvtop_expr
  | Naryop_expr
  | Extract
  | Concat
  | Binder
  (* Unops *)
  | Neg
  | Not
  | Clz
  | Ctz
  | Popcnt
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
  | Trim
  | Regexp_star
  | Regexp_loop
  | Regexp_plus
  | Regexp_opt
  | Regexp_comp
  (* Binops *)
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
  | Ext_rotl
  | Ext_rotr
  | At
  | List_cons
  | List_append
  | String_prefix
  | String_suffix
  | String_contains
  | String_last_index
  | String_in_re
  | Regexp_range
  | Regexp_inter
  | Regexp_diff
  (* Triops *)
  | Ite
  | List_set
  | String_extract
  | String_replace
  | String_index
  | String_replace_all
  | String_replace_re
  | String_replace_re_all
  (* Relops *)
  | Eq
  | Ne
  | Lt
  | LtU
  | Le
  | LeU
  (* Cvtops *)
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
  | Sign_extend
  | Zero_extend
  | String_to_code
  | String_from_code
  | String_to_int
  | String_from_int
  | String_to_float
  | String_to_re
  (* Naryops *)
  | Logand
  | Logor
  | Regexp_union
  | Distinct
  (* Types *)
  | Ty_app
  | Ty_bitv
  | Ty_bool
  | Ty_fp
  | Ty_int
  | Ty_list
  | Ty_none
  | Ty_real
  | Ty_str
  | Ty_unit
  | Ty_regexp
  | Ty_roundingMode
  (* Metadata *)
  | Depth
  | Max_depth
  | Time
  | Nb_queries
  | Mean_depth

type t

val empty : t

val union : t -> t -> t

val incr_feat : feat -> t -> t

val get_feat : feat -> t -> int

val add_time : int -> t -> t

val add_depth : int -> t -> t

val get_depth : t -> int

val add_nb_queries : int -> t -> t

val add_mean_depth : int -> t -> t

val rename_depth_to_max_depth : t -> t

val feat_to_string : feat -> string

val feat_of_string : string -> feat

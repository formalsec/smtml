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
  | Trim (* uninterpreted *)
  (* Regexp *)
  | Regexp_star
  | Regexp_loop of (int * int)
  | Regexp_plus
  | Regexp_opt
  | Regexp_comp

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
  | String_prefix (* (str.prefixof String String Bool) *)
  | String_suffix (* (str.suffixof String String Bool) *)
  | String_contains (* (str.contains String String Bool) *)
  | String_last_index
  | String_in_re
  (* Regexp *)
  | Regexp_range

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

type triop =
  | Ite
  | List_set
  (* String *)
  | String_extract (* (str.substr String Int Int String) *)
  | String_replace (* (str.replace String String String String) *)
  | String_index (* (str.indexof String String Int Int) *)

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
  | String_to_code (* (str.to_code String Int) *)
  | String_from_code (* (str.from_code Int String) *)
  | String_to_int (* (str.to_int String Int) *)
  | String_from_int (* (str.from_int Int String) *)
  | String_to_float
  | String_to_re

type naryop =
  | Logand
  | Logor
  | Concat
  | Regexp_union

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

val compare : t -> t -> int

val unop_equal : unop -> unop -> bool

val binop_equal : binop -> binop -> bool

val relop_equal : relop -> relop -> bool

val triop_equal : triop -> triop -> bool

val cvtop_equal : cvtop -> cvtop -> bool

val naryop_equal : naryop -> naryop -> bool

val equal : t -> t -> bool

val logic_of_string : string -> (logic, [> `Msg of string ]) result

val pp_unop : unop Fmt.t

val pp_binop : binop Fmt.t

val pp_triop : triop Fmt.t

val pp_relop : relop Fmt.t

val pp_cvtop : cvtop Fmt.t

val pp_naryop : naryop Fmt.t

val pp : t Fmt.t

val pp_logic : logic Fmt.t

val string_of_type : t -> string

val size : t -> int

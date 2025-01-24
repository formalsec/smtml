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

val compare : t -> t -> int

val equal : t -> t -> bool

val pp : t Fmt.t

val string_of_type : t -> string

val of_string : string -> (t, [> `Msg of string ]) Result.t

val size : t -> int

module Unop : sig
  type t =
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

  val equal : t -> t -> bool

  val pp : t Fmt.t
end

module Binop : sig
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

  val equal : t -> t -> bool

  val pp : t Fmt.t
end

module Relop : sig
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

  val equal : t -> t -> bool

  val pp : t Fmt.t
end

module Triop : sig
  type t =
    | Ite
    | List_set
    (* String *)
    | String_extract (* (str.substr String Int Int String) *)
    | String_replace (* (str.replace String String String String) *)
    | String_index (* (str.indexof String String Int Int) *)

  val equal : t -> t -> bool

  val pp : t Fmt.t
end

module Cvtop : sig
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

  val equal : t -> t -> bool

  val pp : t Fmt.t
end

module Naryop : sig
  type t =
    | Logand
    | Logor
    | Concat
    | Regexp_union

  val equal : t -> t -> bool

  val pp : t Fmt.t
end

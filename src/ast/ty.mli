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

type naryop =
  | Logand
  | Logor
  | Concat

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

val logic_of_string : string -> (logic, [> `Msg of string]) result

val pp_unop : Fmt.formatter -> unop -> unit

val pp_binop : Fmt.formatter -> binop -> unit

val pp_triop : Fmt.formatter -> triop -> unit

val pp_relop : Fmt.formatter -> relop -> unit

val pp_cvtop : Fmt.formatter -> cvtop -> unit

val pp_naryop : Fmt.formatter -> naryop -> unit

val pp : Fmt.formatter -> t -> unit

val pp_logic : Fmt.formatter -> logic -> unit

val string_of_type : t -> string

val size : t -> int

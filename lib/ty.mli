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

val pp : Format.formatter -> t -> unit

val pp_op : Format.formatter -> 'a op -> unit

val pp_logic : Format.formatter -> logic -> unit

val equal : t -> t -> bool

val equal_op : 'a op -> 'a op -> bool

val string_of_type : t -> string

val size : t -> int

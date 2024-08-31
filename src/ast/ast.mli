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

type t =
  | Assert of Expr.t
  | Check_sat of Expr.t list
  | Declare_const of { id : Symbol.t; sort : Symbol.t }
  | Echo of string
  | Exit
  | Get_assertions
  | Get_assignment
  | Get_info of string
  | Get_option of string
  | Get_model
  | Get_value of Expr.t list
  | Pop of int
  | Push of int
  | Reset
  | Reset_assertions
  | Set_info of Expr.t
  | Set_logic of Ty.logic
  | Set_option of Expr.t

type script = t list

val pp : Fmt.formatter -> t -> unit

val to_string : t -> string

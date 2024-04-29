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

open Ty

type op_type

exception DivideByZero

exception Value of Ty.t

exception
  TypeError of
    { index : int
    ; value : Value.t
    ; ty : Ty.t
    ; op : op_type
    }

val unop : Ty.t -> unop -> Value.t -> Value.t

val binop : Ty.t -> binop -> Value.t -> Value.t -> Value.t

val triop : Ty.t -> triop -> Value.t -> Value.t -> Value.t -> Value.t

val relop : Ty.t -> relop -> Value.t -> Value.t -> bool

val cvtop : Ty.t -> cvtop -> Value.t -> Value.t

val naryop : Ty.t -> naryop -> Value.t list -> Value.t

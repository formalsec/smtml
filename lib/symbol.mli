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
  { ty : Ty.t
  ; name : string
  }

val ( @: ) : string -> Ty.t -> t

val compare : t -> t -> int

val equal : t -> t -> Bool.t

val make : Ty.t -> string -> t

val mk_symbol : Ty.t -> string -> t [@@deprecated "Please use 'make' instead"]

val pp : Format.formatter -> t -> unit

val rename : t -> string -> t

val to_string : t -> string

val to_json : t -> Yojson.Basic.t

val type_of : t -> Ty.t

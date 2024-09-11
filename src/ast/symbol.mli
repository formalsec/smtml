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

type name =
  | Simple of string
  | Indexed of
      { basename : string
      ; indices : string list
      }

type namespace =
  | Attr
  | Sort
  | Term
  | Var

type t =
  { ty : Ty.t
  ; name : name
  ; namespace : namespace
  }

val attr : namespace

val sort : namespace

val term : namespace

val var : namespace

val ( @: ) : string -> Ty.t -> t

val name : t -> name

val namespace : t -> namespace

val compare : t -> t -> int

val equal : t -> t -> Bool.t

val make : Ty.t -> string -> t

val make3 : Ty.t -> name -> namespace -> t

val mk : namespace -> string -> t

val indexed : namespace -> string -> string list -> t

val pp_namespace : Fmt.formatter -> namespace -> unit

val pp : Fmt.formatter -> t -> unit

val to_string : t -> string

val to_json : t -> Yojson.Basic.t

val type_of : t -> Ty.t

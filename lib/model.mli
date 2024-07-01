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

type t = (Symbol.t, Value.t) Hashtbl.t

val iter : (Symbol.t * Value.t -> unit) -> t -> unit

val get_symbols : t -> Symbol.t list

(** bindings are sorted by symbol *)
val get_bindings : t -> (Symbol.t * Value.t) list

val evaluate : t -> Symbol.t -> Value.t option

val pp : Format.formatter -> ?no_values:bool -> t -> unit

val to_string : t -> string

val to_json : t -> Yojson.t

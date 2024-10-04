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

exception Syntax_error of string

module Smtml : sig
  val from_file : Fpath.t -> Ast.script

  (** [from_file] Parse smtml scripts from [file] *)
  val from_string : string -> Ast.script
end

module Smtlib : sig
  (** [from_file file] Parse smtlib compliant scripts from [file] *)
  val from_file : Fpath.t -> Ast.script
end

(** [from_file file] Tries to parse an smtml (.smtml) or smtlib (.smt2) script
    depending on their file extensions. *)
val from_file : Fpath.t -> Ast.script

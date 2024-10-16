(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2024 formalsec *)
(* Written by the Smtml programmers *)

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

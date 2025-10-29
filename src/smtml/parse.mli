(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2025 formalsec *)
(* Written by the Smtml programmers *)

(** SMT Script Parsing Module. This module provides functionality for parsing
    Smt.ml and SMT-LIB scripts from files or strings. It supports both custom
    Smt.ml syntax and the standard SMT-LIB format. *)

(** {1 Exceptions} *)

(** Exception raised when a syntax error occurs during parsing. *)
exception Syntax_error of string

(** {1 Smt.ml Parsing} *)

module Smtml : sig
  (** [from_file file] parses an SMT-ML script from the given [file]. *)
  val from_file : Fpath.t -> Ast.Script.t

  (** [from_string s] parses an SMT-ML script from the given string [s]. *)
  val from_string : string -> Ast.Script.t
end

(** {1 SMT-LIB Parsing} *)

module Smtlib : sig
  (** [from_file file] parses an SMT-LIB compliant script from the given [file].
  *)
  val from_file : Fpath.t -> Ast.Script.t
end

(** {1 Generic Parsing} *)

(** [from_file file] attempts to parse an SMT-ML (.smtml) or SMT-LIB (.smt2)
    script based on the file extension of [file]. *)
val from_file : Fpath.t -> Ast.Script.t

(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2025 formalsec *)
(* Written by the Smtml programmers *)

(** Solver Type Module. This module defines types and utilities for working with
    different SMT solvers, including parsing, pretty-printing, availability
    checks, and mapping retrieval. *)

(** {1 Solver Types} *)

(** The type [t] represents different SMT solvers. *)
type t =
  | Z3_solver  (** Represents the Z3 solver. *)
  | Bitwuzla_solver  (** Represents the Bitwuzla solver. *)
  | Colibri2_solver  (** Represents the Colibri2 solver. *)
  | Cvc5_solver  (** Represents the CVC5 solver. *)
  | Altergo_solver  (** Represents the Alt-Ergo solver. *)

(** {1 Parsing} *)

(** [of_string s] attempts to convert the string [s] into a solver type. Returns
    [Ok solver] if successful, or an error message otherwise. *)
val of_string : string -> (t, [> `Msg of string ]) result

(** {1 Pretty Printing} *)

(** [pp fmt solver] pretty-prints the solver type [solver] using the formatter
    [fmt]. *)
val pp : t Fmt.t

(** {1 Command-Line Argument Handling} *)

(** [conv] provides a command-line argument converter for solver types. *)
val conv : t Cmdliner.Arg.conv

(** {1 Solver Availability} *)

(** [is_available solver] checks whether the given solver is available in the
    current environment. *)
val is_available : t -> bool

(** {1 Solver Mappings} *)

(** [to_mappings solver] retrieves the corresponding solver mappings module for
    the given solver type. *)
val to_mappings : t -> (module Mappings.S_with_fresh)

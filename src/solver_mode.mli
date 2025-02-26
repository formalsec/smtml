(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2024 formalsec *)
(* Written by the Smtml programmers *)

(** Solver Mode Type. This module defines different solver modes and provides
    utilities for conversion, pretty-printing, and command-line argument
    handling. *)

(** {1 Solver Modes} *)

(** The type [t] represents different solver modes. *)
type t =
  | Batch  (** Represents batch mode, where all queries are solved at once. *)
  | Cached  (** Represents cached mode, where previous results may be reused. *)
  | Incremental
    (** Represents incremental mode, where constraints are solved step by step.
    *)

(** {1 Pretty Printing} *)

(** [pp fmt mode] pretty-prints the solver mode [mode] using the formatter
    [fmt]. *)
val pp : t Fmt.t

(** {1 Parsing} *)

(** [of_string str] parses a string into a solver mode.

    [str] is the input string. Returns [`Ok mode] if parsing is successful, or
    [`Error `Msg] if the string does not match a known solver mode. *)
val of_string : string -> (t, [> `Msg of string ]) result

(** {1 Command-Line Argument Handling} *)

(** [conv] provides a command-line argument converter for solver modes. *)
val conv : t Cmdliner.Arg.conv

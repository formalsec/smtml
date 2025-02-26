(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2024 formalsec *)
(* Written by the Smtml programmers *)

(** Quantifiers and Binding Constructs. This module defines types and utilities
    for representing quantifiers (universal and existential) and let-bindings,
    which are commonly used in SMT-LIB formulas for logical quantification and
    local definitions. *)

(** {1 Quantifier and Binding Types} *)

(** A type representing quantifiers and let-bindings in SMT-LIB. *)
type t =
  | Forall  (** Universal quantifier ([forall x. P(x)]). *)
  | Exists  (** Existential quantifier ([exists x. P(x)]). *)
  | Let_in
    (** Let-binding ([let x = e in P(x)]), used for local definitions. *)

(** [equal q1 q2] returns [true] if the quantifiers or binding constructs [q1]
    and [q2] are equal. *)
val equal : t -> t -> bool

(** {1 Pretty Printing} *)

(** Pretty-printer for quantifiers and let-bindings. Formats a value of type [t]
    for human-readable output. *)
val pp : t Fmt.t

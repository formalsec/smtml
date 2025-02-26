(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2024 formalsec *)
(* Written by the Smtml programmers *)

(** SMT-LIB Logics. This module defines the set of SMT-LIB logics, which specify
    the theories and operations that a solver may handle. Each logic represents
    a combination of supported theories, such as arithmetic, arrays, bitvectors,
    and uninterpreted functions. *)

(** {1 Logic Types} *)

(** A type representing various SMT-LIB logics. *)
type t =
  | ALL  (** The logic that encompasses all theories. *)
  | AUFLIA
    (** Arrays, uninterpreted functions, and linear integer arithmetic. *)
  | AUFLIRA
    (** Arrays, uninterpreted functions, linear integer and real arithmetic. *)
  | AUFNIRA
    (** Arrays, uninterpreted functions, and non-linear integer and real
        arithmetic. *)
  | LIA  (** Linear integer arithmetic. *)
  | LRA  (** Linear real arithmetic. *)
  | QF_ABV  (** Quantifier-free arrays and bitvectors. *)
  | QF_AUFBV
    (** Quantifier-free arrays, uninterpreted functions, and bitvectors. *)
  | QF_AUFLIA
    (** Quantifier-free arrays, uninterpreted functions, and linear integer
        arithmetic. *)
  | QF_AX  (** Quantifier-free array theory. *)
  | QF_BV  (** Quantifier-free bitvector theory. *)
  | QF_BVFP  (** Quantifier-free bitvectors and floating-point arithmetic. *)
  | QF_IDL  (** Quantifier-free integer difference logic. *)
  | QF_LIA  (** Quantifier-free linear integer arithmetic. *)
  | QF_LRA  (** Quantifier-free linear real arithmetic. *)
  | QF_NIA  (** Quantifier-free non-linear integer arithmetic. *)
  | QF_NRA  (** Quantifier-free non-linear real arithmetic. *)
  | QF_RDL  (** Quantifier-free real difference logic. *)
  | QF_S  (** Quantifier-free string theory. *)
  | QF_UF  (** Quantifier-free uninterpreted functions. *)
  | QF_UFBV  (** Quantifier-free uninterpreted functions with bitvectors. *)
  | QF_UFIDL
    (** Quantifier-free uninterpreted functions with integer difference logic.
    *)
  | QF_UFLIA
    (** Quantifier-free uninterpreted functions with linear integer arithmetic.
    *)
  | QF_UFLRA
    (** Quantifier-free uninterpreted functions with linear real arithmetic. *)
  | QF_UFNRA
    (** Quantifier-free uninterpreted functions with non-linear real arithmetic.
    *)
  | UFLRA  (** Uninterpreted functions with linear real arithmetic. *)
  | UFNIA  (** Uninterpreted functions with non-linear integer arithmetic. *)

(** [pp fmt logic] pretty-prints an SMT-LIB logic using the formatter [fmt]. *)
val pp : t Fmt.t

(** [of_string s] parses an SMT-LIB logic from a string. Returns [`Ok t] if
    parsing is successful, or [`Error `Msg] if the string does not match a known
    logic. *)
val of_string : string -> (t, [> `Msg of string ]) result

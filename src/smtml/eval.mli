(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2025 formalsec *)
(* Written by the Smtml programmers *)

(** Operators and Evaluation Functions. This module defines types and functions
    for representing and evaluating various kinds of operations, including
    unary, binary, ternary, relational, conversion, and n-ary operations. It
    also defines exceptions for handling errors during evaluation. *)

(** {1 Operation Types} *)

(** A type representing various kinds of operations. *)
type op_type =
  [ `Unop of Ty.Unop.t  (** Unary operation. *)
  | `Binop of Ty.Binop.t  (** Binary operation. *)
  | `Relop of Ty.Relop.t  (** Relational operation. *)
  | `Triop of Ty.Triop.t  (** Ternary operation. *)
  | `Cvtop of Ty.Cvtop.t  (** Conversion operation. *)
  | `Naryop of Ty.Naryop.t  (** N-ary operation. *)
  ]

(** {1 Exceptions} *)

(** Exception raised when a division by zero occurs during evaluation. *)
exception DivideByZero
(* FIXME: use snake case instead *)

exception Conversion_to_integer

exception Integer_overflow

exception Index_out_of_bounds

(** Exception raised when an invalid value is encountered during evaluation. *)
exception Value of Ty.t

(** Exception raised when a type error occurs during evaluation. *)
exception
  TypeError of
    { index : int  (** The position of the erroneous value in the operation. *)
    ; value : Value.t  (** The actual value that caused the error. *)
    ; ty : Ty.t  (** The expected type. *)
    ; op : op_type  (** The operation that led to the error. *)
    ; msg : string
    }
(* FIXME: use snake case instead *)

(** {1 Evaluation Functions} *)

(** [unop ty op v] applies a unary operation [op] on the value [v] of type [ty].
    Raises [TypeError] if the value does not match the expected type. *)
val unop : Ty.t -> Ty.Unop.t -> Value.t -> Value.t

(** [binop ty op v1 v2] applies a binary operation [op] on the values [v1] and
    [v2] of type [ty]. Raises [DivideByZero] if the operation involves division
    by zero. Raises [TypeError] if the values do not match the expected type. *)
val binop : Ty.t -> Ty.Binop.t -> Value.t -> Value.t -> Value.t

(** [triop ty op v1 v2 v3] applies a ternary operation [op] on the values [v1],
    [v2], and [v3] of type [ty]. Raises [TypeError] if any value does not match
    the expected type. *)
val triop : Ty.t -> Ty.Triop.t -> Value.t -> Value.t -> Value.t -> Value.t

(** [relop ty op v1 v2] applies a relational operation [op] on the values [v1]
    and [v2] of type [ty]. Returns [true] if the relation holds, otherwise
    [false]. Raises [TypeError] if the values do not match the expected type. *)
val relop : Ty.t -> Ty.Relop.t -> Value.t -> Value.t -> bool

(** [cvtop ty op v] applies a conversion operation [op] on the value [v] of type
    [ty]. Raises [TypeError] if the value does not match the expected type. *)
val cvtop : Ty.t -> Ty.Cvtop.t -> Value.t -> Value.t

(** [naryop ty op vs] applies an n-ary operation [op] on the list of values [vs]
    of type [ty]. Raises [TypeError] if any value does not match the expected
    type. *)
val naryop : Ty.t -> Ty.Naryop.t -> Value.t list -> Value.t

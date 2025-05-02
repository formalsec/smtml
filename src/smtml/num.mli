(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2025 formalsec *)
(* Written by the Smtml programmers *)

(** Typed Values Representation. This module defines types and utilities for
    representing values with different numeric types, including integers and
    floating-point numbers. It also provides functions for type checking,
    comparison, formatting, and conversion. *)

(** {1 Value Types} *)

(** The type [t] represents values with different numeric types. *)
type t =
  | F32 of int32  (** 32-bit floating-point value, stored as an [int32]. *)
  | F64 of int64  (** 64-bit floating-point value, stored as an [int64]. *)

(** Representation options for value printing. *)
type printer =
  [ `Pretty  (** Human-readable format. *)
  | `Hexadecimal  (** Hexadecimal representation. *)
  ]

(** [type_of v] returns the type of the given value [v]. *)
val type_of : t -> Ty.t

(** [compare v1 v2] provides a total ordering over values of type [t]. It
    returns a negative integer if [v1] is less than [v2], zero if they are
    equal, and a positive integer if [v1] is greater than [v2]. *)
val compare : t -> t -> int

(** [equal v1 v2] returns [true] if [v1] and [v2] are equal, otherwise [false].
*)
val equal : t -> t -> bool

(** {1 Pretty Printing} *)

(** [set_default_printer p] sets the default printer format for displaying
    values. *)
val set_default_printer : printer -> unit

(** [pp] is a formatter for values of type [t], using the currently set printer.
*)
val pp : t Fmt.t

(** [pp_no_type] is a formatter that prints a value of type [t] without
    displaying its type. *)
val pp_no_type : t Fmt.t

(** {1 Serialization} *)

(** [to_string v] converts the value [v] to a string representation. *)
val to_string : t -> string

(** [of_string ty s] attempts to parse the string [s] as a value of type [ty].
    Returns [Ok v] on success, or an error message if parsing fails. *)
val of_string : Ty.t -> string -> (t, [> `Msg of string ]) result

(** [to_json v] converts the value [v] into a JSON representation. *)
val to_json : t -> Yojson.Basic.t

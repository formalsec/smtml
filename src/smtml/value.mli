(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2025 formalsec *)
(* Written by the Smtml programmers *)

(** Concrete Values Module. This module defines types and utilities for working
    with concrete values, including integers, floats, strings, lists, and
    applications. It provides functions for type checking, comparison, mapping,
    and conversion to/from strings and JSON. *)

(** {1 Value Types} *)

(** The type [t] represents concrete values. *)
type t =
  | True  (** Boolean true. *)
  | False  (** Boolean false. *)
  | Unit  (** Unit value. *)
  | Int of int  (** Integer value. *)
  | Real of float  (** Real number value. *)
  | Str of string  (** String value. *)
  | Num of Num.t  (** Numeric value. *)
  | Bitv of Bitvector.t  (** Bitvector value. *)
  | List of t list  (** List of values. *)
  | App : [> `Op of string ] * t list -> t
    (** Application of an operator to a list of values. *)
  | Nothing  (** Represents an undefined or missing value. *)

(** [type_of v] returns the type of the value [v]. *)
val type_of : t -> Ty.t

(** {1 Comparison} *)

val hash : t -> int

(** [compare v1 v2] provides a total ordering over values of type [t]. It
    returns a negative integer if [v1] is less than [v2], zero if they are
    equal, and a positive integer if [v1] is greater than [v2]. *)
val compare : t -> t -> int

(** [equal v1 v2] returns [true] if [v1] and [v2] are equal, otherwise [false].
*)
val equal : t -> t -> bool

(** {1 Mapping} *)

(** [map v f] applies the function [f] to the value [v]. If [v] is a list, [f]
    is applied to each element. *)
val map : t -> (t -> t) -> t

(** [let+ v f] is a convenience operator for applying [f] to [v]. *)
val ( let+ ) : t -> (t -> t) -> t

(** [default_of_type ty] returns the default value for type [ty]. *)
val default_of_type : Ty.t -> t

(** {1 Pretty Printing} *)

(** [pp fmt v] pretty-prints the value [v] using the formatter [fmt]. *)
val pp : t Fmt.t

(** {1 Serialization} *)

(** [to_string v] converts the value [v] to a string representation. *)
val to_string : t -> string

(** [of_string ty s] attempts to parse the string [s] as a value of type [ty].
    Returns [Ok v] on success, or an error message if parsing fails. *)
val of_string : Ty.t -> string -> (t, [> `Msg of string ]) result

(** [to_json v] converts the value [v] into a JSON representation. *)
val to_json : t -> Yojson.Basic.t

module Smtlib : sig
  val pp : t Fmt.t
end

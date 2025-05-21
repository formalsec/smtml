(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2025 formalsec *)
(* Written by the Smtml programmers *)

(** Symbol Module. This module defines names, namespaces, and typed symbols,
    providing utilities for creating, comparing, and manipulating symbols. *)

(** {1 Name Types} *)

(** The type [name] represents a symbol name, which can either be a simple
    string or an indexed name with a base name and a list of indices. *)
type name =
  | Simple of string  (** A simple name represented as a string. *)
  | Indexed of
      { basename : string  (** The base name. *)
      ; indices : string list
          (** A list of indices associated with the name. *)
      }

(** {1 Namespace Types} *)

(** The type [namespace] classifies symbols into different kinds of identifiers.
*)
type namespace =
  | Attr  (** Attributes. *)
  | Sort  (** Sorts (types). *)
  | Term  (** Terms (functions, constants). *)
  | Var  (** Variables. *)

(** {1 Symbol Types} *)

(** The type [t] represents a symbol, consisting of a type, a name, and a
    namespace. *)
type t =
  { ty : Ty.t  (** The type of the symbol. *)
  ; name : name  (** The name of the symbol. *)
  ; namespace : namespace  (** The namespace to which the symbol belongs. *)
  }

(** {1 Namespace Constants} *)

(** [attr] represents the attribute namespace. *)
val attr : namespace

(** [sort] represents the sort (type) namespace. *)
val sort : namespace

(** [term] represents the term (function, constant) namespace. *)
val term : namespace

(** [var] represents the variable namespace. *)
val var : namespace

(** {1 Symbol Creation} *)

(** [s @: ty] creates a symbol with name [s] and type [ty], belonging to the
    term (function, constant) namespace. *)
val ( @: ) : string -> Ty.t -> t

(** [make ty s] creates a symbol with type [ty] and name [s] in the term
    (function, constant) namespace. *)
val make : Ty.t -> string -> t

(** [make3 ty name ns] creates a symbol with type [ty], name [name], and
    namespace [ns]. *)
val make3 : Ty.t -> name -> namespace -> t

(** [mk ns s] creates a symbol with name [s] in the specified namespace [ns]
    with a default type. *)
val mk : namespace -> string -> t

(** [indexed ns basename indices] creates a symbol with an indexed name, where
    [basename] is the base name and [indices] are the associated indices. *)
val indexed : namespace -> string -> string list -> t

(** {1 Symbol Accessors} *)

(** [name sym] returns the name of the symbol [sym]. *)
val name : t -> name

(** [namespace sym] returns the namespace of the symbol [sym]. *)
val namespace : t -> namespace

(** [type_of sym] returns the type of the symbol [sym]. *)
val type_of : t -> Ty.t

(** {1 Comparison} *)

(** [compare sym1 sym2] performs a total order comparison of [sym1] and [sym2].
*)
val compare : t -> t -> int

(** [equal sym1 sym2] checks if [sym1] and [sym2] are equal. *)
val equal : t -> t -> Bool.t

(** {1 Pretty Printing} *)

(** [pp_namespace fmt ns] pretty-prints the namespace [ns] using the formatter
    [fmt]. *)
val pp_namespace : namespace Fmt.t

(** [pp fmt sym] pretty-prints the symbol [sym] using the formatter [fmt]. *)
val pp : t Fmt.t

(** {1 Serialization} *)

(** [to_string sym] converts the symbol [sym] to a string representation. *)
val to_string : t -> string

(** [to_json sym] converts the symbol [sym] to a JSON representation. *)
val to_json : t -> Yojson.Basic.t

module Map : Map.S with type key = t

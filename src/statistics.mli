(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2024 formalsec *)
(* Written by the Smtml programmers *)

(** Statistics Module. This module defines types and utilities for managing and
    manipulating solver statistics, including merging and pretty-printing. *)

(** {1 Entry Types} *)

(** The type [entry] represents a single statistic entry, which can be either an
    integer or a floating-point number. *)
type entry =
  [ `Int of int  (** An integer statistic entry. *)
  | `Float of float  (** A floating-point statistic entry. *)
  ]

(** {1 Statistics Map} *)

(** [Map] is a module for managing statistics as a map from string keys to
    statistic entries. *)
module Map : Map.S with type key = string

(** The type [t] represents a collection of statistics as a map from string keys
    to statistic entries. *)
type t = entry Map.t

(** {1 Merging and Aggregation} *)

(** [sum_entries e1 e2] sums two statistic entries [e1] and [e2]. If both
    entries are integers, the result is an integer. If either entry is a float,
    the result is a float. *)
val sum_entries : entry -> entry -> entry

(** [merge t1 t2] merges two statistics maps [t1] and [t2]. If a key exists in
    both maps, the corresponding entries are summed using [sum_entries]. *)
val merge : t -> t -> t

(** {1 Pretty Printing} *)

(** [pp_entry fmt entry] pretty-prints a single statistic entry [entry] using
    the formatter [fmt]. *)
val pp_entry : entry Fmt.t

(** [pp fmt stats] pretty-prints the statistics map [stats] using the formatter
    [fmt]. *)
val pp : t Fmt.t

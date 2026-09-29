(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2026 formalsec *)
(* Written by the Smtml programmers *)

(** A simple bounded hash table with least-recently-used eviction.

    This is a drop-in replacement for the subset of [Hashtbl.S] used across the
    code base, with the addition of a [capacity]. Once the number of bindings
    exceeds the capacity, the least recently used binding is discarded.

    A non-positive capacity disables eviction, making the table unbounded. *)

module type S = sig
  type key

  type !'a t

  val create : int -> 'a t

  (** [capacity t] returns the maximum number of bindings, or [0] when the table
      is unbounded. *)
  val capacity : 'a t -> int

  val clear : 'a t -> unit

  val length : 'a t -> int

  val add : 'a t -> key -> 'a -> unit

  val replace : 'a t -> key -> 'a -> unit

  val remove : 'a t -> key -> unit

  val find_opt : 'a t -> key -> 'a option

  val mem : 'a t -> key -> bool

  val iter : (key -> 'a -> unit) -> 'a t -> unit

  val fold : (key -> 'a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc

  val filter_map_inplace : (key -> 'a -> 'a option) -> 'a t -> unit

  val copy : 'a t -> 'a t

  val to_seq : 'a t -> (key * 'a) Seq.t

  val to_seq_keys : 'a t -> key Seq.t

  val to_seq_values : 'a t -> 'a Seq.t

  val add_seq : 'a t -> (key * 'a) Seq.t -> unit

  val replace_seq : 'a t -> (key * 'a) Seq.t -> unit

  val stats : 'a t -> Hashtbl.statistics
end

module Make (H : Hashtbl.HashedType) : S with type key = H.t

(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2024 formalsec *)
(* Written by the Smtml programmers *)

module type S = sig
  type key = Expr.Set.t

  type !'a t

  val hits : 'a t -> int

  val misses : 'a t -> int

  val create : int -> 'a t

  val reset : 'a t -> unit

  val copy : 'a t -> 'a t

  val add : 'a t -> key -> 'a -> unit

  val remove : 'a t -> key -> unit

  val find_opt : 'a t -> key -> 'a option

  val replace : 'a t -> key -> 'a -> unit

  val mem : 'a t -> key -> bool

  val iter : (key -> 'a -> unit) -> 'a t -> unit

  val filter_map_inplace : (key -> 'a -> 'a option) -> 'a t -> unit

  val fold : (key -> 'a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc

  val length : 'a t -> int

  val stats : 'a t -> Hashtbl.statistics

  val to_seq : 'a t -> (key * 'a) Seq.t

  val to_seq_keys : 'a t -> key Seq.t

  val to_seq_values : 'a t -> 'a Seq.t

  val add_seq : 'a t -> (key * 'a) Seq.t -> unit

  val replace_seq : 'a t -> (key * 'a) Seq.t -> unit
end

module type Intf = sig
  module type S = S

  module Strong : S
end

(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2026 formalsec *)
(* Written by the Smtml programmers *)

include Cache_intf

module Strong : S = struct
  type key = Expr.Set.t

  module Lru = Lru.Make (Expr.Set)

  type nonrec !'a t =
    { data : 'a Lru.t
    ; hits : int Atomic.t
    ; misses : int Atomic.t
    }

  let hits { hits; _ } = Atomic.get hits

  let misses { misses; _ } = Atomic.get misses

  let create sz =
    { data = Lru.create sz; hits = Atomic.make 0; misses = Atomic.make 0 }

  let reset { data; hits; misses } =
    Lru.clear data;
    Atomic.set hits 0;
    Atomic.set misses 0

  let copy { data; hits; misses } =
    { data = Lru.copy data
    ; hits = Atomic.(make (get hits))
    ; misses = Atomic.(make (get misses))
    }

  let add { data; _ } k v = Lru.add data k v

  let remove { data; _ } k = Lru.remove data k

  let find_opt { data; hits; misses } k =
    match Lru.find_opt data k with
    | Some _ as v ->
      Atomic.incr hits;
      v
    | None as v ->
      Atomic.incr misses;
      v

  let replace { data; _ } k v = Lru.replace data k v

  let mem { data; _ } k = Lru.mem data k

  let iter f { data; _ } = Lru.iter f data

  let filter_map_inplace f { data; _ } = Lru.filter_map_inplace f data

  let fold f { data; _ } acc = Lru.fold f data acc

  let length { data; _ } = Lru.length data

  let stats { data; _ } = Lru.stats data

  let to_seq { data; _ } = Lru.to_seq data

  let to_seq_keys { data; _ } = Lru.to_seq_keys data

  let to_seq_values { data; _ } = Lru.to_seq_values data

  let add_seq { data; _ } seq = Lru.add_seq data seq

  let replace_seq { data; _ } seq = Lru.replace_seq data seq
end

(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2024 formalsec *)
(* Written by the Smtml programmers *)

include Cache_intf

module Strong : S = struct
  include Hashtbl.Make (Expr.Set)

  type nonrec !'a t =
    { data : 'a t
    ; hits : int Atomic.t
    ; misses : int Atomic.t
    }

  let hits { hits; _ } = Atomic.get hits

  let misses { misses; _ } = Atomic.get misses

  let create sz =
    { data = create sz; hits = Atomic.make 0; misses = Atomic.make 0 }

  let reset { data; _ } = reset data

  let copy { data; hits; misses } =
    { data = copy data
    ; hits = Atomic.(make (get hits))
    ; misses = Atomic.(make (get misses))
    }

  let add { data; _ } k v = add data k v

  let remove { data; _ } k = remove data k

  let find_opt { data; hits; misses } k =
    match find_opt data k with
    | Some _ as v ->
      Atomic.incr hits;
      v
    | None as v ->
      Atomic.incr misses;
      v

  let replace { data; _ } k v = replace data k v

  let mem { data; _ } k = mem data k

  let iter f { data; _ } = iter f data

  let filter_map_inplace f { data; _ } = filter_map_inplace f data

  let fold f { data; _ } acc = fold f data acc

  let length { data; _ } = length data

  let stats { data; _ } = stats data

  let to_seq { data; _ } = to_seq data

  let to_seq_keys { data; _ } = to_seq_keys data

  let to_seq_values { data; _ } = to_seq_values data

  let add_seq { data; _ } seq = add_seq data seq

  let replace_seq { data; _ } seq = replace_seq data seq
end

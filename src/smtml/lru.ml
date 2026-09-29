(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2026 formalsec *)
(* Written by the Smtml programmers *)

module type S = sig
  type key

  type !'a t

  val create : int -> 'a t

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

module Make (H : Hashtbl.HashedType) : S with type key = H.t = struct
  type key = H.t

  module Tbl = Hashtbl.Make (H)

  type 'a node =
    { key : key
    ; mutable value : 'a
    ; mutable prev : 'a node option
    ; mutable next : 'a node option
    }

  type !'a t =
    { tbl : 'a node Tbl.t
    ; mutable head : 'a node option (* most recently used *)
    ; mutable tail : 'a node option (* least recently used *)
    ; capacity : int
    ; mutable size : int
    }

  let create capacity =
    { tbl = Tbl.create (max 16 (min capacity 65536))
    ; head = None
    ; tail = None
    ; capacity
    ; size = 0
    }

  let capacity t = t.capacity

  let length t = t.size

  let unlink t n =
    (match n.prev with Some p -> p.next <- n.next | None -> t.head <- n.next);
    (match n.next with Some q -> q.prev <- n.prev | None -> t.tail <- n.prev);
    n.prev <- None;
    n.next <- None

  let insert_front t n =
    n.prev <- None;
    n.next <- t.head;
    (match t.head with Some h -> h.prev <- Some n | None -> ());
    t.head <- Some n;
    if Option.is_none t.tail then t.tail <- Some n

  let touch t n =
    match t.head with
    | Some h when phys_equal h n -> ()
    | _ ->
      unlink t n;
      insert_front t n

  let remove_node t n =
    unlink t n;
    Tbl.remove t.tbl n.key;
    t.size <- t.size - 1

  let evict t = match t.tail with None -> () | Some n -> remove_node t n

  let add t k v =
    match Tbl.find_opt t.tbl k with
    | Some n ->
      n.value <- v;
      touch t n
    | None ->
      let n = { key = k; value = v; prev = None; next = None } in
      Tbl.add t.tbl k n;
      insert_front t n;
      t.size <- t.size + 1;
      if t.capacity > 0 && t.size > t.capacity then evict t

  let replace = add

  let find_opt t k =
    match Tbl.find_opt t.tbl k with
    | None -> None
    | Some n ->
      touch t n;
      Some n.value

  let mem t k = Tbl.mem t.tbl k

  let remove t k =
    match Tbl.find_opt t.tbl k with None -> () | Some n -> remove_node t n

  let clear t =
    Tbl.reset t.tbl;
    t.head <- None;
    t.tail <- None;
    t.size <- 0

  let stats t = Tbl.stats t.tbl

  let iter f t =
    let rec loop = function
      | None -> ()
      | Some n ->
        f n.key n.value;
        loop n.next
    in
    loop t.head

  let fold f t acc =
    let rec loop acc = function
      | None -> acc
      | Some n -> loop (f n.key n.value acc) n.next
    in
    loop acc t.head

  let filter_map_inplace f t =
    let rec loop = function
      | None -> ()
      | Some n ->
        let next = n.next in
        ( match f n.key n.value with
        | Some v -> n.value <- v
        | None -> remove_node t n );
        loop next
    in
    loop t.head

  let copy t =
    let t' = create t.capacity in
    (* [acc] ends up ordered tail-to-head, so adding it in order leaves the
       most recently used binding at the front. *)
    let rec collect acc = function
      | None -> acc
      | Some n -> collect ((n.key, n.value) :: acc) n.next
    in
    List.iter (fun (k, v) -> add t' k v) (collect [] t.head);
    t'

  let to_seq t =
    let rec loop = function
      | None -> Seq.Nil
      | Some n -> Seq.Cons ((n.key, n.value), fun () -> loop n.next)
    in
    fun () -> loop t.head

  let to_seq_keys t =
    let rec loop = function
      | None -> Seq.Nil
      | Some n -> Seq.Cons (n.key, fun () -> loop n.next)
    in
    fun () -> loop t.head

  let to_seq_values t =
    let rec loop = function
      | None -> Seq.Nil
      | Some n -> Seq.Cons (n.value, fun () -> loop n.next)
    in
    fun () -> loop t.head

  let add_seq t seq = Seq.iter (fun (k, v) -> add t k v) seq

  let replace_seq t seq = Seq.iter (fun (k, v) -> replace t k v) seq
end

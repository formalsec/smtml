(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2024 formalsec *)
(* Written by the Smtml programmers *)

type t = int

let fresh =
  let next = ref 0 in
  fun () ->
    let id = !next in
    incr next;
    id

let compare = Int.compare

let equal a b = compare a b = 0

let hash a = a

let pp = Fmt.int

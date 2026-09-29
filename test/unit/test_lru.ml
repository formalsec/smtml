(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2026 formalsec *)
(* Written by the Smtml programmers *)

open Smtml
module Cache = Cache.Strong

let key n = Expr.Set.singleton (Expr.value (Value.Int (Z.of_int n)))

let test_eviction () =
  let cache = Cache.create 2 in
  Cache.add cache (key 1) "a";
  Cache.add cache (key 2) "b";
  (* Touch [key 1] so that [key 2] becomes the least recently used entry. *)
  ignore (Cache.find_opt cache (key 1));
  Cache.add cache (key 3) "c";
  Alcotest.(check int) "length is capped" 2 (Cache.length cache);
  Alcotest.(check (option string))
    "most recently used entry is kept" (Some "a")
    (Cache.find_opt cache (key 1));
  Alcotest.(check (option string))
    "least recently used entry is evicted" None
    (Cache.find_opt cache (key 2));
  Alcotest.(check (option string))
    "new entry is present" (Some "c")
    (Cache.find_opt cache (key 3))

let () =
  Alcotest.run "LRU"
    [ ("LRU", [ Alcotest.test_case "test_eviction" `Quick test_eviction ]) ]

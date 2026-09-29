(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2026 formalsec *)
(* Written by the Smtml programmers *)

(* Micro-benchmarks for the encoding memoization and the LRU caches.

   Run with the default profile:

     dune exec test/benchmarks/bench_micro.exe

   For more stable numbers, build in the [benchmark] profile ([-O3 -unsafe
   -noassert]):

     dune exec --profile benchmark test/benchmarks/bench_micro.exe

   The encoding memoization can be toggled with [SMTML_MAX_MEMO_ENTRIES] (0
   disables it), see [test/benchmarks/run_compare.sh]. *)

open Smtml

let now = Unix.gettimeofday

(* [time_it] returns the average time per operation, in seconds. *)
let time_it ?(warmup = 1) ~iters f =
  for _ = 1 to warmup do
    f ()
  done;
  let start = now () in
  for _ = 1 to iters do
    f ()
  done;
  let stop = now () in
  (stop -. start) /. float_of_int iters

let report name seconds = Fmt.pr "  %-42s %12.3f us/op@." name (seconds *. 1e6)

let int_expr n = Expr.value (Value.Int (Z.of_int n))

let symbol name = Expr.symbol (Symbol.make_const Ty.Ty_int name)

(* A closed-ish subexpression reused across every assertion, so that the
   encoding memo has something to cache. *)
let shared_expr ~width ~depth =
  let shared =
    List.init depth (fun i ->
      Expr.binop Ty.Ty_int Ty.Binop.Add
        (symbol (Printf.sprintf "y%d" i))
        (int_expr (i + 1)) )
    |> List.fold_left (Expr.binop Ty.Ty_int Ty.Binop.Add) (int_expr 0)
  in
  let base = symbol "x0" in
  let rec loop acc i =
    if i >= width then acc
    else
      let acc = Expr.binop Ty.Ty_int Ty.Binop.Add acc shared in
      let acc = Expr.binop Ty.Ty_int Ty.Binop.Mul acc (int_expr 2) in
      loop acc (i + 1)
  in
  loop base 0

let bench_expr_build () =
  let iters = 2_000 in
  let width = 64
  and depth = 8 in
  let secs = time_it ~iters (fun () -> ignore (shared_expr ~width ~depth)) in
  Fmt.pr "  expression construction@.";
  report "build (hash-consing)" secs

let bench_simplify () =
  let iters = 2_000 in
  let counter = ref 0 in
  let shared = shared_expr ~width:16 ~depth:4 in
  let secs =
    time_it ~iters (fun () ->
      incr counter;
      let e = Expr.binop Ty.Ty_int Ty.Binop.Add shared (int_expr !counter) in
      ignore (Expr.simplify e) )
  in
  Fmt.pr "@.  expression simplification@.";
  report "simplify (LRU cache, capacity 4096)" secs

let bench_cache () =
  let module Cache = Cache.Strong in
  let capacity = 4096 in
  let keys =
    Array.init (2 * capacity) (fun i -> Expr.Set.singleton (int_expr i))
  in
  let cache = Cache.create capacity in
  Array.iteri (fun i k -> if i < capacity then Cache.add cache k i) keys;
  let n = Array.length keys in
  let iters = 500_000 in
  let idx = ref 0 in
  let secs =
    time_it ~iters (fun () ->
      let i = !idx mod n in
      incr idx;
      match Cache.find_opt cache keys.(i) with
      | Some _ -> ()
      | None -> Cache.add cache keys.(i) i )
  in
  Fmt.pr "@.  result cache@.";
  report "find/add (LRU, capacity 4096)" secs

module Z3 = Solver.Batch (Z3_mappings)

let bench_encode ~assertions ~checks =
  Fmt.pr "@.  encoding and checking (z3)@.";
  if not Z3_mappings.is_available then
    Fmt.pr "  z3 backend unavailable, skipping@."
  else begin
    let solver = Z3.create ~logic:Logic.QF_LIA () in
    let pool = Array.init 16 (fun i -> symbol (Printf.sprintf "s%d" i)) in
    let shared = shared_expr ~width:16 ~depth:16 in
    let es =
      List.init assertions (fun i ->
        let a = pool.(i mod Array.length pool) in
        let b = pool.((i + 5) mod Array.length pool) in
        Expr.relop Ty.Ty_int Ty.Relop.Le
          (Expr.binop Ty.Ty_int Ty.Binop.Add a shared)
          (Expr.binop Ty.Ty_int Ty.Binop.Add b (int_expr i)) )
    in
    Z3.add solver es;
    let t0 = now () in
    ignore (Z3.check solver []);
    let t_cold = now () -. t0 in
    let warm = checks - 1 in
    let t1 = now () in
    for _ = 1 to warm do
      ignore (Z3.check solver [])
    done;
    let t_warm = now () -. t1 in
    Fmt.pr "  %-42s %12.3f ms (cold, encodes all %d assertions)@." "first check"
      (t_cold *. 1e3) assertions;
    if warm > 0 then
      Fmt.pr "  %-42s %12.3f ms (%d warm checks, %.3f ms/check)@."
        "warm checks (re-encodes assumed set)" (t_warm *. 1e3) warm
        (t_warm /. float_of_int warm *. 1e3)
  end

let () =
  Fmt.pr "@.Smt.ml micro-benchmarks@.";
  Fmt.pr "  SMTML_MAX_MEMO_ENTRIES=%s@."
    ( match Sys.getenv_opt "SMTML_MAX_MEMO_ENTRIES" with
    | Some s -> s
    | None -> "<default>" );
  bench_expr_build ();
  bench_simplify ();
  bench_cache ();
  bench_encode ~assertions:500 ~checks:20

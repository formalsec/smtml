(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2024 formalsec *)
(* Written by the Smtml programmers *)

include Solver_intf

let slice_algorithm = ref false

let unsat_cache = ref false

let unsat_plus = ref false

let ( let+ ) o f = Option.map f o

module Base (M : Mappings_intf.S) = struct
  type t = M.solver

  type solver = t

  let solver_time = ref 0.0

  let solver_count = ref 0

  let pp_statistics _fmt _solver = ()

  let create ?params ?logic () : t = M.Solver.make ?params ?logic ()

  let interrupt solver = M.Solver.interrupt solver

  let clone (solver : t) : t = M.Solver.clone solver

  let push (solver : t) : unit = M.Solver.push solver

  let pop (solver : t) (lvl : int) : unit = M.Solver.pop solver lvl

  let reset (solver : t) : unit = M.Solver.reset solver

  let add (solver : t) (es : Expr.t list) : unit = M.Solver.add solver es

  let add_set s es = add s @@ Expr.Set.to_list es

  let get_assertions (_solver : t) : Expr.t list = assert false

  let get_statistics (solver : t) : Statistics.t =
    M.Solver.get_statistics solver

  let check (solver : M.solver) (es : Expr.t list) =
    incr solver_count;
    Utils.run_and_time_call
      ~use:(fun time -> solver_time := !solver_time +. time)
      (fun () -> M.Solver.check solver ~assumptions:es)

  let check_set solver es = check solver @@ Expr.Set.to_list es

  let get_value (solver : M.solver) (e : Expr.t) : Expr.t =
    match M.Solver.model solver with
    | Some m -> Expr.value @@ M.value m e
    | None ->
      Fmt.failwith "get_value: Trying to get a value from an unsat solver"

  let model ?(symbols : Symbol.t list option) (s : M.solver) : Model.t option =
    let+ model = M.Solver.model s in
    M.values_of_model ?symbols model

  let get_sat_model ?symbols s set =
    match check_set s set with
    | `Sat -> (
      match model ?symbols s with
      | Some model -> `Model model
      | None ->
        (* Should never happen *)
        assert false )
    | (`Unsat | `Unknown) as no_model -> no_model
end

module Incremental (M : Mappings_intf.S) : Solver_intf.S =
  Base [@inlined hint] (M)

module Batch (Mappings : Mappings.S) = struct
  include Base (Mappings)

  type solver = Mappings.solver

  type t =
    { solver : solver
    ; mutable top : Expr.t list
    ; stack : Expr.t list Stack.t
    }

  let pp_statistics fmt s = pp_statistics fmt s.solver

  let create ?params ?logic () =
    { solver = create ?params ?logic (); top = []; stack = Stack.create () }

  let clone ({ solver; top; stack } : t) : t =
    { solver = clone solver; top; stack = Stack.copy stack }

  let push ({ top; stack; solver } : t) : unit =
    Mappings.Solver.push solver;
    Stack.push top stack

  let rec pop (s : t) (lvl : int) : unit =
    assert (lvl <= Stack.length s.stack);
    if lvl <= 0 then ()
    else begin
      Mappings.Solver.pop s.solver 1;
      match Stack.pop_opt s.stack with
      | None -> assert false
      | Some v ->
        s.top <- v;
        pop s (lvl - 1)
    end

  let reset (s : t) =
    Mappings.Solver.reset s.solver;
    Stack.clear s.stack;
    s.top <- []

  let add (s : t) (es : Expr.t list) : unit = s.top <- es @ s.top

  let add_set s es = s.top <- Expr.Set.to_list es @ s.top

  let get_assertions (s : t) : Expr.t list = s.top [@@inline]

  let get_statistics (s : t) : Statistics.t = get_statistics s.solver

  let get_sat_model ?symbols s set =
    let assert_ = Expr.Set.union set (Expr.Set.of_list s.top) in
    get_sat_model ?symbols s.solver assert_

  let check (s : t) (es : Expr.t list) = check s.solver (es @ s.top)

  let check_set s es = check s @@ Expr.Set.to_list es

  let get_value (solver : t) (e : Expr.t) : Expr.t = get_value solver.solver e

  let model ?(symbols : Symbol.t list option) (s : t) : Model.t option =
    model ?symbols s.solver

  let interrupt { solver; _ } = interrupt solver
end

module Cached (Mappings_ : Mappings.S) = struct
  module Make (Mappings : Mappings.S) = struct
    include Base (Mappings)

    type solver = Mappings.solver

    type t =
      { solver : solver
      ; mutable top : Expr.Set.t
      ; stack : Expr.Set.t Stack.t
      ; mutable last_check : Expr.Set.t option
      ; mutable sliced : int * int * float
      ; mutable unsat : int * int * float * float
      }

    module Cache = Cache.Strong

    let cache = Cache.create 256

    let unsat = Cache.create 32

    let cache_hits () = Cache.hits cache

    let cache_misses () = Cache.misses cache

    let pp_statistics fmt s = pp_statistics fmt s.solver

    let create ?params ?logic () =
      { solver = create ?params ?logic ()
      ; top = Expr.Set.empty
      ; stack = Stack.create ()
      ; last_check = None
      ; sliced = (0, 0, 0.0)
      ; unsat = (0, 0, 0.0, 0.0)
      }

    let clone ({ solver; top; stack; last_check; sliced; unsat } : t) : t =
      { solver = clone solver
      ; top
      ; stack = Stack.copy stack
      ; last_check
      ; sliced
      ; unsat
      }

    let push ({ top; stack; solver; _ } : t) : unit =
      Mappings.Solver.push solver;
      Stack.push top stack

    let rec pop (s : t) (lvl : int) : unit =
      assert (lvl <= Stack.length s.stack);
      if lvl <= 0 then ()
      else begin
        Mappings.Solver.pop s.solver 1;
        match Stack.pop_opt s.stack with
        | None -> assert false
        | Some v ->
          s.top <- v;
          pop s (lvl - 1)
      end

    let reset (s : t) =
      Mappings.Solver.reset s.solver;
      Stack.clear s.stack;
      s.top <- Expr.Set.empty

    let add (s : t) (es : Expr.t list) : unit =
      s.top <- Expr.Set.(union (of_list es) s.top)

    let add_set s es = s.top <- Expr.Set.union es s.top

    let get_assertions (s : t) : Expr.t list = Expr.Set.to_list s.top [@@inline]

    let get_statistics (s : t) : Statistics.t =
      let stats = get_statistics s.solver in
      let cache_hits = Cache.hits cache in
      let cache_misses = Cache.misses cache in
      let unsat_hits = Cache.hits unsat in
      let unsat_misses = Cache.misses unsat in
      let slice_count, slice_factor, slice_ts = s.sliced in
      let unsat_cache, unsat_plus, unsat_cache_ts, unsat_plus_ts = s.unsat in
      stats
      |> Statistics.Map.add "cache misses" (`Int cache_misses)
      |> Statistics.Map.add "cache hits" (`Int cache_hits)
      |> Statistics.Map.add "slice count" (`Int slice_count)
      |> Statistics.Map.add "slice factor" (`Int slice_factor)
      |> Statistics.Map.add "slice ts" (`Float slice_ts)
      |> Statistics.Map.add "unsat caches" (`Int unsat_cache)
      |> Statistics.Map.add "unsat hits" (`Int unsat_hits)
      |> Statistics.Map.add "unsat misses" (`Int unsat_misses)
      |> Statistics.Map.add "unsat plus" (`Int unsat_plus)
      |> Statistics.Map.add "unsat cache ts" (`Float unsat_cache_ts)
      |> Statistics.Map.add "unsat plus ts" (`Float unsat_plus_ts)

    let get_sat_model ?symbols s set =
      let assert_ = Expr.Set.union set s.top in
      get_sat_model ?symbols s.solver assert_

    let model ?(symbols : Symbol.t list option) (s : t) : Model.t option =
      let open Option in
      let* last_check = s.last_check in
      (* We need to explicitly check_set because we don't want a cached response *)
      match check_set s.solver last_check with
      | `Sat -> model ?symbols s.solver
      | `Unsat | `Unknown -> None

    let rec break_conjunctions (e : Expr.t) : Expr.t list =
      match Expr.view e with
      | Binop (_, And, e1, e2) -> break_conjunctions e1 @ break_conjunctions e2
      | Naryop (_, Logand, es) -> List.concat_map break_conjunctions es
      | _ -> [ e ]

    let has_unsat_plus s es =
      if !unsat_plus then (
        let start_ts = Unix.gettimeofday () in
        let cores = Cache.to_seq_keys unsat in
        let unsat_caches, unsat_plus, unsat_cache_ts, unsat_plus_ts = s.unsat in
        if Seq.exists (fun core -> Expr.Set.subset core es) cores then (
          let end_ts = Unix.gettimeofday () -. start_ts in
          let unsat_plus' = unsat_plus + 1 in
          let unsat_plus_ts' = unsat_plus_ts +. end_ts in
          s.unsat <- (unsat_caches, unsat_plus', unsat_cache_ts, unsat_plus_ts');
          true )
        else
          let end_ts = Unix.gettimeofday () -. start_ts in
          let unsat_plus_ts' = unsat_plus_ts +. end_ts in
          s.unsat <- (unsat_caches, unsat_plus, unsat_cache_ts, unsat_plus_ts');
          false )
      else false

    let has_unsat_core s es =
      if !unsat_cache then
        if Option.is_some (Cache.find_opt unsat es) then true
        else has_unsat_plus s es
      else false

    let cache_unsat_core s res =
      match (!unsat_cache, res) with
      | true, `Unsat ->
        let start_ts = Unix.gettimeofday () in
        let unsat_caches, unsat_plus, unsat_cache_ts, unsat_plus_ts = s.unsat in
        let core = Mappings.Solver.unsat_core s.solver in
        Option.iter (fun c -> Cache.add unsat c `Unsat) core;
        let end_ts = Unix.gettimeofday () -. start_ts in
        let unsat_cache_ts' = unsat_cache_ts +. end_ts in
        s.unsat <- (unsat_caches + 1, unsat_plus, unsat_cache_ts', unsat_plus_ts)
      | _ -> ()

    let slice_formula s es =
      let start_ts = Unix.gettimeofday () in
      let slices = Slicing.slice es in
      let end_ts = Unix.gettimeofday () -. start_ts in
      let count, factor, ts = s.sliced in
      if List.length slices = 1 then s.sliced <- (count, factor, ts +. end_ts)
      else s.sliced <- (count + 1, factor + List.length slices, ts +. end_ts);
      slices

    let rec check_set_rec ?(sliced = false) s es =
      let check_sliced = check_set_rec ~sliced:true in
      let break_f e k = Expr.Set.(union (of_list (break_conjunctions e)) k) in
      let check_f k e = match k with `Sat -> check_sliced s e | _ -> k in
      let es' = if sliced then es else Expr.Set.(fold break_f es empty) in
      s.last_check <- Some es';
      match Cache.find_opt cache es' with
      | Some res -> res
      | None when (not sliced) && has_unsat_core s es' ->
        Cache.add cache es' `Unsat;
        `Unsat
      | None when (not sliced) && !slice_algorithm ->
        let slices = slice_formula s es in
        List.fold_left check_f `Sat slices
      | None ->
        let result = check_set s.solver es' in
        cache_unsat_core s result;
        Cache.add cache es' result;
        result

    let check_set s es = check_set_rec s (Expr.Set.union es s.top)

    let check (s : t) (es : Expr.t list) = check_set s (Expr.Set.of_list es)

    let get_value (solver : t) (e : Expr.t) : Expr.t = get_value solver.solver e

    let interrupt { solver; _ } = interrupt solver
  end

  module Fresh () = Make (Mappings_)
  include Make (Mappings_)
end

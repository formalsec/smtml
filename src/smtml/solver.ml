(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2024 formalsec *)
(* Written by the Smtml programmers *)

include Solver_intf

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
      }

    module Cache = Cache.Strong

    let cache = Cache.create 256

    let cache_hits () = Cache.hits cache

    let cache_misses () = Cache.misses cache

    let pp_statistics fmt s = pp_statistics fmt s.solver

    let create ?params ?logic () =
      { solver = create ?params ?logic ()
      ; top = Expr.Set.empty
      ; stack = Stack.create ()
      ; last_check = None
      }

    let clone ({ solver; top; stack; last_check } : t) : t =
      { solver = clone solver; top; stack = Stack.copy stack; last_check }

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

    let get_statistics (s : t) : Statistics.t = get_statistics s.solver

    let get_sat_model ?symbols s set =
      let assert_ = Expr.Set.union set s.top in
      get_sat_model ?symbols s.solver assert_

    let model ?(symbols : Symbol.t list option) (s : t) : Model.t option =
      let open Option in
      let* last_check = s.last_check in
      (* We need to explicitly check_set because we don't want a cached reseponse *)
      match check_set s.solver last_check with
      | `Sat -> model ?symbols s.solver
      | `Unsat | `Unknown -> None

    let check_set s es =
      let assert_ = Expr.Set.union es s.top in
      s.last_check <- Some assert_;
      match Cache.find_opt cache assert_ with
      | Some res -> res
      | None ->
        let result = check_set s.solver assert_ in
        Cache.add cache es result;
        result

    let check (s : t) (es : Expr.t list) = check_set s (Expr.Set.of_list es)

    let get_value (solver : t) (e : Expr.t) : Expr.t = get_value solver.solver e

    let interrupt { solver; _ } = interrupt solver
  end

  module Fresh () = Make (Mappings_)
  include Make (Mappings_)
end

include Solver_intf

let ( let+ ) o f = Option.map f o

module Base (M : Mappings_intf.S) = struct
  let solver_time = ref 0.0

  let solver_count = ref 0

  let interrupt solver = M.Solver.interrupt solver

  let pp_statistics fmt solver = M.Solver.pp_statistics fmt solver

  let check (solver : M.solver) (es : Expr.t list) : satisfiability =
    solver_count := !solver_count + 1;
    Utils.run_and_time_call
      ~use:(fun time -> solver_time := !solver_time +. time)
      (fun () -> M.Solver.check solver ~assumptions:es)

  let get_value (solver : M.solver) (e : Expr.t) : Expr.t =
    match M.Solver.model solver with
    | Some m -> Expr.(make @@ Val (M.value m e))
    | None -> Log.err "get_value: Trying to get a value from an unsat solver"

  let model ?(symbols : Symbol.t list option) (s : M.solver) : Model.t option =
    let+ model = M.Solver.model s in
    M.values_of_model ?symbols model
end

module Make_batch (Mappings : Mappings_intf.S) = struct
  include Base (Mappings)

  type solver = Mappings.solver

  type t =
    { solver : solver
    ; mutable top : Expr.t list
    ; stack : Expr.t list Stack.t
    }

  let pp_statistics fmt s = pp_statistics fmt s.solver

  let create ?params ?logic () =
    { solver = Mappings.Solver.make ?params ?logic ()
    ; top = []
    ; stack = Stack.create ()
    }

  let clone ({ solver; top; stack } : t) : t =
    { solver; top; stack = Stack.copy stack }

  let push ({ top; stack; _ } : t) : unit = Stack.push top stack

  let pop (s : t) (lvl : int) : unit =
    assert (lvl <= Stack.length s.stack);
    for _ = 1 to lvl do
      s.top <- Stack.pop s.stack
    done

  let reset (s : t) =
    Mappings.Solver.reset s.solver;
    Stack.clear s.stack;
    s.top <- []

  let add (s : t) (es : Expr.t list) : unit = s.top <- es @ s.top

  let get_assertions (s : t) : Expr.t list = s.top [@@inline]

  let check (s : t) (es : Expr.t list) : satisfiability =
    check s.solver (es @ s.top)

  let get_value (solver : t) (e : Expr.t) : Expr.t = get_value solver.solver e

  let model ?(symbols : Symbol.t list option) (s : t) : Model.t option =
    model ?symbols s.solver

  let interrupt { solver; _ } = interrupt solver
end

(* TODO: Our base solver can be incrmental itself? *)
module Make_incremental (Mappings : Mappings_intf.S) = struct
  include Base (Mappings)

  type t = Mappings.solver

  type solver = t

  let create ?params ?logic () : t =
    Mappings.Solver.make ?params ?logic () |> Mappings.Solver.add_simplifier

  let clone (solver : t) : t = Mappings.Solver.clone solver

  let push (solver : t) : unit = Mappings.Solver.push solver

  let pop (solver : t) (lvl : int) : unit = Mappings.Solver.pop solver lvl

  let reset (solver : t) : unit = Mappings.Solver.reset solver

  let add (solver : t) (es : Expr.t list) : unit = Mappings.Solver.add solver es

  let get_assertions (_solver : t) : Expr.t list = assert false
end

module Batch (M : Mappings_intf.S) : Solver_intf.S = Make_batch (M)

module Incremental (M : Mappings_intf.S) : Solver_intf.S = Make_incremental (M)

module Z3_batch : Solver_intf.S = Batch (Z3_mappings)

module Z3_incremental : Solver_intf.S = Incremental (Z3_mappings)

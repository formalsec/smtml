exception Unknown

let ( let+ ) o f = Option.map f o

module Base (M : Mappings_intf.S) = struct
  let solver_time = ref 0.0
  let solver_count = ref 0

  let time_call f acc =
    let start = Stdlib.Sys.time () in
    let ret = f () in
    acc := !acc +. (Stdlib.Sys.time () -. start);
    ret

  let update_param_values params =
    M.update_param_value Timeout (Params.get params Timeout);
    M.update_param_value Model (Params.get params Model);
    M.update_param_value Unsat_core (Params.get params Unsat_core);
    M.update_param_value Ematching (Params.get params Ematching)

  let interrupt () = M.interrupt ()
  let pp_statistics fmt solver = M.pp_statistics fmt solver
end

module Batch (Mappings : Mappings_intf.S) = struct
  include Base (Mappings)

  type solver = Mappings.solver

  type t =
    { solver : solver
    ; mutable top : Expr.t list
    ; stack : Expr.t list Stack.t
    }

  let pp_statistics fmt s = pp_statistics fmt s.solver

  let create ?params () =
    Option.iter update_param_values params;
    { solver = Mappings.mk_solver (); top = []; stack = Stack.create () }

  let clone ({ solver; top; stack } : t) : t =
    { solver; top; stack = Stack.copy stack }

  let push ({ top; stack; _ } : t) : unit = Stack.push top stack

  let pop (s : t) (lvl : int) : unit =
    assert (lvl <= Stack.length s.stack);
    for _ = 1 to lvl do
      s.top <- Stack.pop s.stack
    done

  let reset (s : t) =
    Mappings.reset s.solver;
    Stack.clear s.stack;
    s.top <- []

  let add (s : t) (es : Expr.t list) : unit = s.top <- es @ s.top
  let get_assertions (s : t) : Expr.t list = s.top [@@inline]

  let check (s : t) (es : Expr.t list) : bool =
    let es' = es @ s.top in
    solver_count := !solver_count + 1;
    let sat = time_call (fun () -> Mappings.check s.solver es') solver_time in
    match Mappings.satisfiability sat with
    | Mappings_intf.Satisfiable -> true
    | Mappings_intf.Unsatisfiable -> false
    | Mappings_intf.Unknown -> raise Unknown

  let get_value (solver : t) (e : Expr.t) : Expr.t =
    match Mappings.solver_model solver.solver with
    | Some m -> Expr.(Val (Mappings.value m e) @: e.ty)
    | None -> assert false

  let model ?(symbols : Symbol.t list option) (s : t) : Model.t option =
    let+ model = Mappings.solver_model s.solver in
    Mappings.values_of_model ?symbols model
end

module Incremental (Mappings : Mappings_intf.S) = struct
  include Base (Mappings)

  type t = Mappings.solver
  type solver = t

  let create ?params () : t =
    Option.iter update_param_values params;
    Mappings.mk_solver ()

  let clone (solver : t) : t = Mappings.translate solver
  let push (solver : t) : unit = Mappings.push solver
  let pop (solver : t) (lvl : int) : unit = Mappings.pop solver lvl
  let reset (solver : t) : unit = Mappings.reset solver
  let add (solver : t) (es : Expr.t list) : unit = Mappings.add_solver solver es
  let get_assertions (_solver : t) : Expr.t list = assert false

  let check (solver : t) (es : Expr.t list) : bool =
    solver_count := !solver_count + 1;
    let sat = time_call (fun () -> Mappings.check solver es) solver_time in
    match Mappings.satisfiability sat with
    | Mappings_intf.Satisfiable -> true
    | Mappings_intf.Unknown -> raise Unknown
    | Mappings_intf.Unsatisfiable -> false

  let get_value (solver : t) (e : Expr.t) : Expr.t =
    match Mappings.solver_model solver with
    | Some m -> Expr.(Val (Mappings.value m e) @: e.ty)
    | None -> assert false

  let model ?(symbols : Symbol.t list option) (solver : t) : Model.t Option.t =
    let+ model = Mappings.solver_model solver in
    Mappings.values_of_model ?symbols model
end

module Batch' (M : Mappings_intf.S) : Solver_intf.S = Batch (M)
module Incremental' (M : Mappings_intf.S) : Solver_intf.S = Incremental (M)

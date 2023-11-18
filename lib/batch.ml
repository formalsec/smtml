exception Unknown

let ( let+ ) o f = Option.map f o

module Make (Mappings : Mappings_intf.S) = struct
  type solver = Mappings.solver

  type t =
    { solver : solver
    ; mutable top : Expr.t list
    ; stack : Expr.t list Stack.t
    }

  let solver_time = ref 0.0
  let solver_count = ref 0

  let time_call f acc =
    let start = Stdlib.Sys.time () in
    let ret = f () in
    acc := !acc +. (Stdlib.Sys.time () -. start);
    ret

  let update_param_values params =
    Mappings.update_param_value Model (Params.get params Model);
    Mappings.update_param_value Unsat_core (Params.get params Unsat_core)

  let create ?params () =
    Option.iter update_param_values params;
    { solver = Mappings.mk_solver (); top = []; stack = Stack.create () }

  let interrupt () = Mappings.interrupt ()

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
    | Some m ->  Expr.(Val (Mappings.value m e) @: e.ty)
    | None -> assert false

  let model ?(symbols : Symbol.t list option) (s : t) : Model.t option =
    let+ model = Mappings.solver_model s.solver in
    Mappings.values_of_model ?symbols model
end

module Make' (M : Mappings_intf.S) : Solver_intf.S = Make (M)

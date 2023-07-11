exception Unknown

module Make (Mappings : Mappings_intf.S) = struct
  open Core

  type t = { solver : s; pc : Expression.t ref }
  and s = Mappings.solver

  let solver_time = ref 0.0
  let solver_count = ref 0

  let time_call f acc =
    let start = Caml.Sys.time () in
    let ret = f () in
    acc := !acc +. (Caml.Sys.time () -. start);
    ret

  let create () =
    { solver = Mappings.mk_solver (); pc = ref (Boolean.mk_val true) }

  let interrupt () = Mappings.interrupt ()
  let clone (s : t) : t = { s with pc = ref !(s.pc) }

  let add (s : t) (e : Expression.t) : unit =
    s.pc := Expression.add_constraint e !(s.pc)

  let get_assertions (s : t) : Expression.t = !(s.pc)

  let set_default_axioms (s : t) : unit =
    Mappings.add_solver s.solver
      (List.map ~f:Mappings.encode_expr Axioms.axioms)

  let check_sat (s : t) (es : Expression.t list) : bool =
    let es' = List.map ~f:Mappings.encode_expr es in
    solver_count := !solver_count + 1;
    let sat = time_call (fun () -> Mappings.check s.solver es') solver_time in
    match Mappings.satisfiability sat with
    | Mappings_intf.Satisfiable -> true
    | Mappings_intf.Unsatisfiable -> false
    | Mappings_intf.Unknown -> raise Unknown

  let check (s : t) (expr : Expression.t option) : bool =
    let expression =
      Mappings.encode_expr
        (Option.fold expr ~init:!(s.pc) ~f:(fun f e ->
             Expression.add_constraint e f))
    in
    solver_count := !solver_count + 1;
    let sat =
      time_call (fun () -> Mappings.check s.solver [ expression ]) solver_time
    in
    match Mappings.satisfiability sat with
    | Mappings_intf.Satisfiable -> true
    | Mappings_intf.Unsatisfiable -> false
    | Mappings_intf.Unknown -> raise Unknown

  let fork (s : t) (e : Expression.t) : bool * bool =
    (check s (Some e), check s (Some (Expression.negate_relop e)))

  let model (s : t) : Mappings.model Option.t = Mappings.get_model s.solver

  let eval (s : t) (e : Expression.t) (es : Expression.t list) : Value.t option
      =
    let es' = List.map ~f:Mappings.encode_expr es in
    ignore (time_call (fun () -> Mappings.check s.solver es') solver_time);
    Option.value_map (model s) ~default:None ~f:(fun m ->
        Mappings.value_of_const m e)

  let value_binds ?(symbols : Symbol.t list option) (s : t) : Model.t Option.t =
    Option.map (model s) ~f:(Mappings.value_binds ?symbols)

  let string_binds (s : t) : (string * string * string) list =
    Option.value_map (model s) ~default:[] ~f:Mappings.string_binds

  let find_model (s : t) (es : Expression.t list) : Model.t Option.t =
    if check_sat s es then value_binds ~symbols:(Expression.get_symbols es) s
    else None
end

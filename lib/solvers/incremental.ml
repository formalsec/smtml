exception Unknown

open Core

module Make (Mappings : Mappings_intf.S) = struct
  let solver_time = ref 0.0
  let solver_count = ref 0

  let time_call f acc =
    let start = Caml.Sys.time () in
    let ret = f () in
    acc := !acc +. (Caml.Sys.time () -. start);
    ret

  type s = Mappings.solver
  type t = { solver : s; pc : Expression.t ref }

  let create () : t =
    { solver = Mappings.mk_solver (); pc = ref (Boolean.mk_val true) }

  let interrupt () = Mappings.interrupt ()

  let clone (e : t) : t =
    { solver = Mappings.translate e.solver; pc = ref !(e.pc) }

  let add (e : t) (c : Expression.t) : unit =
    e.pc := Expression.add_constraint c !(e.pc);
    let ec = Mappings.encode_expr c in
    Mappings.add_solver e.solver [ ec ]

  let get_assertions (e : t) : Expression.t = !(e.pc)

  let check (e : t) (expr : Expression.t option) : bool =
    let expr' = Option.to_list (Option.map ~f:Mappings.encode_expr expr) in
    let b =
      solver_count := !solver_count + 1;
      let sat =
        time_call (fun () -> Mappings.check e.solver expr') solver_time
      in
      match Mappings.satisfiability sat with
      | Mappings_intf.Satisfiable -> true
      | Mappings_intf.Unknown -> raise Unknown
      | Mappings_intf.Unsatisfiable -> false
    in
    b

  let fork (s : t) (e : Expression.t) : bool * bool =
    (check s (Some e), check s (Some (Expression.negate_relop e)))

  let model (e : t) : Mappings.model Option.t = Mappings.get_model e.solver

  let value_binds ?(symbols : Symbol.t list option) (e : t) : Model.t Option.t =
    Option.map (model e) ~f:(Mappings.value_binds ?symbols)

  let string_binds (e : t) : (string * string * string) list =
    Option.value_map (model e) ~default:[] ~f:Mappings.string_binds
end

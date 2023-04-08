open Base
open Z3
open Types
open Common

exception Unknown

let solver_time = ref 0.0
let solver_count = ref 0

let time_call f acc =
  let start = Caml.Sys.time () in
  let ret = f () in
  acc := !acc +. (Caml.Sys.time () -. start);
  ret

type s = Solver.solver
type t = { solver : s; pc : Formula.t ref }

let create () : t =
  { solver = Solver.mk_solver ctx None; pc = ref (Formula.create ()) }

let interrupt () = Tactic.interrupt ctx

let clone (e : t) : t =
  { solver = Solver.translate e.solver ctx; pc = ref !(e.pc) }

let add (e : t) (c : Expression.t) : unit =
  e.pc := Formula.add_constraint c !(e.pc);
  let ec = encode_expr ~bool_to_bv:false c in
  Solver.add e.solver [ ec ]

let add_formula (e : t) (f : Formula.t) : unit =
  e.pc := Formula.conjunct [ f; !(e.pc) ];
  let ef = encode_formula f in
  Solver.add e.solver [ ef ]

let check (e : t) (expr : Expression.t option) : bool =
  let expr' =
    Option.to_list (Option.map ~f:(encode_expr ~bool_to_bv:false) expr)
  in
  let b =
    solver_count := !solver_count + 1;
    let sat = time_call (fun () -> Solver.check e.solver expr') solver_time in
    match sat with
    | Solver.SATISFIABLE -> true
    | Solver.UNKNOWN -> raise Unknown
    | Solver.UNSATISFIABLE -> false
  in
  b

let fork (s : t) (e : Expression.t) : bool * bool =
  (check s (Some e), check s (Some (Expression.negate_relop e)))

let model (e : t) : Model.model Option.t = Solver.get_model e.solver

let value_binds (e : t) (vars : (string * expr_type) list) :
    (string * Expression.value) list =
  Option.value_map (model e) ~default:[] ~f:(fun m -> Common.value_binds m vars)

let string_binds (e : t) : (string * string * string) list =
  Option.value_map (model e) ~default:[] ~f:Common.string_binds

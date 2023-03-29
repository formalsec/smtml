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

let create () : t = { solver = Solver.mk_solver ctx None; pc = ref (Formula.create ()) }

let clone (e : t) : t =
  { solver = Solver.translate e.solver ctx; pc = ref !(e.pc) }

let add (e : t) (c : Expression.t) : unit =
  e.pc := Formula.add_constraint c !(e.pc);
  let ec = encode_expr ~bool_to_bv:false c in
  Solver.add e.solver [ ec ]

let add_formula (e : t) (f : Formula.t) : unit =
  e.pc := Formula.conjunct [f ;!(e.pc)];
  let ef = encode_formula f in
  Solver.add e.solver [ ef ]

let check (e : t) (expr : Expression.t option) : bool =
  let expr' = Option.to_list (Option.map (encode_expr ~bool_to_bv:false) expr) in
  let b =
    solver_count := !solver_count + 1;
    let sat = time_call (fun () -> Solver.check e.solver expr') solver_time in
    match sat with
    | Solver.SATISFIABLE -> true
    | Solver.UNKNOWN ->
        failwith ("unknown: " ^ Solver.get_reason_unknown e.solver) (* fail? *)
    | Solver.UNSATISFIABLE -> false
  in
  b

let fork (s : t) (e : Expression.t) : bool * bool =
  (check s (Some e), check s (Some (Expression.negate_relop e)))

(** fails if solver isn't currently SAT *)
let model (e : t) : Model.model =
  Option.get (Solver.get_model e.solver)

(** fails if solver isn't currently SAT *)
let value_binds (e : t) (vars : (string * expr_type) list) :
    (string * Num.t) list =
  let m = model e in
  Common.value_binds m vars

(** fails if solver isn't currently SAT *)
let string_binds (e : t) (vars : (string * expr_type) list) :
    (string * string * string) list =
  let m = model e in
  Common.string_binds m vars

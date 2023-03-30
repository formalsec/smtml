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
type t = { solver : s; pc : Expression.pc ref }

let create () : t = { solver = Solver.mk_solver ctx None; pc = ref [] }

let clone (e : t) : t =
  { solver = Solver.translate e.solver ctx; pc = ref !(e.pc) }

let add (e : t) (c : Expression.t) : unit =
  e.pc := c :: !(e.pc);
  let ec = encode_expr ~bool_to_bv:false c in
  Solver.add e.solver [ ec ]

let check (e : t) (vs : Expression.t list) : bool =
  let vs' = List.map ~f:(encode_expr ~bool_to_bv:false) vs in
  let b =
    solver_count := !solver_count + 1;
    let sat = time_call (fun () -> Solver.check e.solver vs') solver_time in
    match sat with
    | Solver.SATISFIABLE -> true
    | Solver.UNKNOWN -> raise Unknown
    | Solver.UNSATISFIABLE -> false
  in
  b

let fork (e : t) (co : Expression.t) : bool * bool =
  let negated_co = Expression.negate_relop co in
  (check e [ co ], check e [ negated_co ])

(** fails if solver isn't currently SAT *)
let model (e : t) : Model.model =
  assert (check e []);
  Option.value_exn (Solver.get_model e.solver)

(** fails if solver isn't currently SAT *)
let value_binds (e : t) (vars : (string * expr_type) list) :
    (string * Expression.value) list =
  Common.value_binds (model e) vars

(** fails if solver isn't currently SAT *)
let string_binds (e : t) : (string * string * string) list =
  Common.string_binds (model e)

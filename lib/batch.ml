open Base
open Z3
open Common

exception Unknown

type t = { solver : s; pc : Formula.t ref }
and s = Solver.solver

let solver_time = ref 0.0
let solver_count = ref 0

let time_call f acc =
  let start = Caml.Sys.time () in
  let ret = f () in
  acc := !acc +. (Caml.Sys.time () -. start);
  ret

let create () =
  { solver = Solver.mk_solver ctx None; pc = ref (Formula.create ()) }

let interrupt () = Tactic.interrupt ctx
let clone (s : t) : t = { s with pc = ref !(s.pc) }

let add (s : t) (e : Expression.t) : unit =
  s.pc := Formula.add_constraint e !(s.pc)

let add_formula (s : t) (f : Formula.t) : unit =
  s.pc := Formula.conjunct [ f; !(s.pc) ]

let check_formulas (s : t) (formulas : Formula.t list) : bool =
  let expressions = List.map ~f:encode_formula formulas in
  solver_count := !solver_count + 1;
  let sat =
    time_call (fun () -> Solver.check s.solver expressions) solver_time
  in
  match sat with
  | Solver.SATISFIABLE -> true
  | Solver.UNSATISFIABLE -> false
  | Solver.UNKNOWN -> raise Unknown

let check_sat (s : t) (es : Expression.t list) : bool =
  let es' = List.map ~f:encode_expr es in
  solver_count := !solver_count + 1;
  let sat = time_call (fun () -> Solver.check s.solver es') solver_time in
  match sat with
  | Solver.SATISFIABLE -> true
  | Solver.UNSATISFIABLE -> false
  | Solver.UNKNOWN -> raise Unknown

let check (s : t) (expr : Expression.t option) : bool =
  let expression =
    encode_formula
      (Option.fold ~init:!(s.pc)
         ~f:(fun f e -> Formula.add_constraint e f)
         expr)
  in
  solver_count := !solver_count + 1;
  let sat =
    time_call (fun () -> Solver.check s.solver [ expression ]) solver_time
  in
  let b =
    match sat with
    | Solver.SATISFIABLE -> true
    | Solver.UNSATISFIABLE -> false
    | Solver.UNKNOWN -> raise Unknown
  in
  b

let fork (s : t) (e : Expression.t) : bool * bool =
  (check s (Some e), check s (Some (Expression.negate_relop e)))

let model (s : t) : Model.model Option.t = Solver.get_model s.solver

let eval (s : t) (e : Expression.t) (es : Expression.t list) :
    Expression.value option =
  let es' = List.map ~f:encode_expr es in
  ignore (time_call (fun () -> Solver.check s.solver es') solver_time);
  Option.value_map (model s) ~default:None ~f:(fun m -> value_of_const m e)

let value_binds (s : t) vars : (string * Expression.value) list =
  Option.value_map (model s) ~default:[] ~f:(fun m -> Common.value_binds m vars)

let string_binds (s : t) : (string * string * string) list =
  Option.value_map (model s) ~default:[] ~f:Common.string_binds

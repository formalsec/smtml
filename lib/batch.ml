open Base
open Common

exception Unknown

type t = { solver : s; pc : Expression.t ref }
and s = Z3.Solver.solver

let solver_time = ref 0.0
let solver_count = ref 0

let time_call f acc =
  let start = Caml.Sys.time () in
  let ret = f () in
  acc := !acc +. (Caml.Sys.time () -. start);
  ret

let create () =
  { solver = Z3.Solver.mk_solver ctx None; pc = ref (Boolean.mk_val true) }

let interrupt () = Z3.Tactic.interrupt ctx
let clone (s : t) : t = { s with pc = ref !(s.pc) }

let add (s : t) (e : Expression.t) : unit =
  s.pc := Expression.add_constraint e !(s.pc)

let set_default_axioms (s : Z3.Solver.solver) : unit =
  Z3.Solver.add s (List.map ~f:encode_expr Axioms.axioms)

let check_sat (s : t) (es : Expression.t list) : bool =
  let es' = List.map ~f:encode_expr es in
  solver_count := !solver_count + 1;
  let sat = time_call (fun () -> Z3.Solver.check s.solver es') solver_time in
  match sat with
  | Z3.Solver.SATISFIABLE -> true
  | Z3.Solver.UNSATISFIABLE -> false
  | Z3.Solver.UNKNOWN -> raise Unknown

let check (s : t) (expr : Expression.t option) : bool =
  let expression =
    encode_expr
      (Option.fold ~init:!(s.pc)
         ~f:(fun f e -> Expression.add_constraint e f)
         expr)
  in
  solver_count := !solver_count + 1;
  let sat =
    time_call (fun () -> Z3.Solver.check s.solver [ expression ]) solver_time
  in
  let b =
    match sat with
    | Z3.Solver.SATISFIABLE -> true
    | Z3.Solver.UNSATISFIABLE -> false
    | Z3.Solver.UNKNOWN -> raise Unknown
  in
  b

let fork (s : t) (e : Expression.t) : bool * bool =
  (check s (Some e), check s (Some (Expression.negate_relop e)))

let model (s : t) : Z3.Model.model Option.t = Z3.Solver.get_model s.solver

let eval (s : t) (e : Expression.t) (es : Expression.t list) :
    Expression.value option =
  let es' = List.map ~f:encode_expr es in
  ignore (time_call (fun () -> Z3.Solver.check s.solver es') solver_time);
  Option.value_map (model s) ~default:None ~f:(fun m -> value_of_const m e)

let value_binds (s : t) vars : (string * Expression.value) list =
  Option.value_map (model s) ~default:[] ~f:(fun m -> Common.value_binds m vars)

let string_binds (s : t) : (string * string * string) list =
  Option.value_map (model s) ~default:[] ~f:Common.string_binds

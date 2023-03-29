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

(*
let formulas_to_smt2_file output_dir =
  let counter = ref 0 in
  let file () : string =
    let () = Int.incr counter in
    Printf.sprintf "query-%d.smt2" !counter
  in
  fun f status ->
    Params.set_print_mode ctx Z3enums.PRINT_SMTLIB2_COMPLIANT;
    let query_out = Caml.Filename.concat output_dir "queries" in
    let query_file = Caml.Filename.concat query_out (file ()) in
    ignore (Unix.system ("mkdir -p " ^ query_out));
    Io.save_file query_file
      (SMT.benchmark_to_smtstring ctx query_file "" status "" (List.tl_exn f)
         (List.hd_exn f))
    *)

let ccheck (s : t) (formula : Formula.t) : bool =
  let expression = encode_formula formula in
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

let check (s : t) (expr : Expression.t option) : bool =
  let formula = !(s.pc) in
  let formula =
    match expr with
    | Some expr -> Formula.add_constraint expr formula
    | None -> formula
  in
  let expression = encode_formula formula in
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

let model (s : t) : Model.model = Option.get (Solver.get_model s.solver)

let value_binds (s : t) vars : (string * Num.t) list =
  let m = model s in
  Common.value_binds m vars

let string_binds (s : t) vars : (string * string * string) list =
  let m = model s in
  Common.string_binds m vars

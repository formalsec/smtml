open Base
open Z3
open Types
open Common
open Formula
open Expression

exception Unknown

type t = { solver : s; pc : pc ref }
and s = Solver.solver

let time_solver = ref 0.0

let time_call f acc =
  let start = Caml.Sys.time () in
  let ret = f () in
  acc := !acc +. (Caml.Sys.time () -. start);
  ret

let create () = { solver = Solver.mk_solver ctx None; pc = ref [] }
let interrupt () = Tactic.interrupt ctx
let clone (s : t) : t = { s with pc = ref !(s.pc) }
let add (s : t) (e : Expression.t) : unit = s.pc := e :: !(s.pc)

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

let check (s : t) (es : Expression.t list) : bool =
  let es' = es @ !(s.pc) in
  let formulas' = List.map ~f:encode_formula (to_formulas es') in
  let sat = time_call (fun () -> Solver.check s.solver formulas') time_solver in
  let b =
    match sat with
    | Solver.SATISFIABLE -> true
    | Solver.UNSATISFIABLE -> false
    | Solver.UNKNOWN -> raise Unknown
  in
  b

let fork (s : t) (e : Expression.t) : bool * bool =
  (check s [ e ], check s [ negate_relop e ])

let get_model (s : t) : Model.model =
  match Solver.get_model s.solver with
  | Some m -> m
  | None -> assert false (* should not happen after sat check *)

let model (s : t) : Model.model =
  assert (check s []);
  get_model s

let value_of_const (model : Model.model) ((c, t) : Expression.t * num_type) :
    Num.t option =
  let interp = Model.eval model (encode_expr c) true in
  let f (e : Expr.expr) : Num.t =
    let v =
      match Sort.get_sort_kind (Expr.get_sort e) with
      | Z3enums.INT_SORT -> int64_of_int e
      | Z3enums.BV_SORT -> int64_of_bv e
      | Z3enums.FLOATING_POINT_SORT ->
        let ebits = FloatingPoint.get_ebits ctx (Expr.get_sort e)
        and sbits = FloatingPoint.get_sbits ctx (Expr.get_sort e) in
        int64_of_fp e ebits (sbits - 1)
      | _ -> assert false
    in
    match t with
    | IntType -> Int (Int64.to_int_trunc v)
    | I32Type -> I32 (Int64.to_int32_trunc v)
    | I64Type -> I64 v
    | F32Type -> F32 (Int64.to_int32_trunc v)
    | F64Type -> F64 v
  in
  Option.map ~f interp

let model_binds (model : Model.model) (vars : (string * num_type) list) :
    (string * Num.t) list =
  List.fold_left ~init:[]
    ~f:(fun a (x, t) ->
      let v = value_of_const model (symbolic t x, t) in
      Option.fold ~init:a ~f:(fun a v' -> (x, v') :: a) v)
    vars

let value_binds (s : t) (vars : (string * num_type) list) :
    (string * Num.t) list =
  let model = model s in
  model_binds model vars

let string_binds _ _ = []

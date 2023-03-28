open Base
open Z3
open Types
open Common
open Expression

exception Unknown

let time_solver = ref 0.0

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
    let sat = time_call (fun () -> Solver.check e.solver vs') time_solver in
    match sat with
    | Solver.SATISFIABLE -> true
    | Solver.UNKNOWN ->
        failwith ("unknown: " ^ Solver.get_reason_unknown e.solver) (* fail? *)
    | Solver.UNSATISFIABLE -> false
  in
  b

let fork (e : t) (co : Expression.t) : bool * bool =
  let negated_co = Expression.negate_relop co in
  (check e [ co ], check e [ negated_co ])

let value_of_const (model : Model.model) (c : Expression.t) : Num.t option =
  let interp = Model.get_const_interp_e model (encode_expr c) in
  let f e : Num.t =
    let v =
      match Sort.get_sort_kind (Expr.get_sort e) with
      | Z3enums.INT_SORT -> int64_of_int e
      | Z3enums.SEQ_SORT -> raise (Error "Not implemented")
      | Z3enums.BV_SORT -> int64_of_bv e
      | Z3enums.FLOATING_POINT_SORT ->
        let ebits = FloatingPoint.get_ebits ctx (Expr.get_sort e)
        and sbits = FloatingPoint.get_sbits ctx (Expr.get_sort e) in
        int64_of_fp e ebits (sbits - 1)
      | _ -> assert false
    in
    match Expression.type_of c with
    | `IntType -> Int (Int64.to_int_trunc v)
    | `StrType -> raise (Error "Not implemented")
    | `I32Type -> I32 (Int64.to_int32_trunc v)
    | `I64Type -> I64 v
    | `F32Type -> F32 (Int64.to_int32_trunc v)
    | `F64Type -> F64 v
  in
  Option.map ~f interp

(** fails if solver isn't currently SAT *)
let model (e : t) : Model.model =
  assert (check e []);
  Option.value_exn (Solver.get_model e.solver)

(** fails if solver isn't currently SAT *)
let value_binds (e : t) vars : (string * Num.t) list =
  let m = model e in
  List.fold_left ~init:[]
    ~f:(fun a (x, t) ->
      let v = value_of_const m (symbolic t x) in
      Option.fold ~init:a ~f:(fun a v' -> (x, v') :: a) v)
    vars

(** fails if solver isn't currently SAT *)
let string_binds (e : t) _ : (string * string * string) list =
  let m = model e in
  List.map
    ~f:(fun const ->
      let sort = Sort.to_string (FuncDecl.get_range const)
      and name = Symbol.to_string (FuncDecl.get_name const)
      and interp =
        Option.value_map ~default:"" ~f:Expr.to_string
          (Model.get_const_interp m const)
      in
      (sort, name, interp))
    (Model.get_const_decls m)

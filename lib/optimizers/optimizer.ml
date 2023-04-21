open Base
open Z3
open Z3_mappings

exception Unknown

type t = Optimize.optimize

let solver_time = ref 0.0

let time_call ~f ~accum =
  let start = Caml.Sys.time () in
  let ret = f () in
  accum := !accum +. (Caml.Sys.time () -. start);
  ret

let create () : t = Optimize.mk_opt ctx
let push (opt : t) : unit = Optimize.push opt
let pop (opt : t) : unit = Optimize.pop opt

let add (opt : t) (es : Expression.t list) : unit =
  Optimize.add opt (List.map ~f:(encode_expr ~bool_to_bv:false) es)

let check (opt : t) e (pc : Expression.t list) obj =
  push opt;
  add opt pc;
  ignore (obj opt (encode_expr ~bool_to_bv:false e));
  ignore (time_call ~f:(fun () -> Optimize.check opt) ~accum:solver_time);
  let model = Optimize.get_model opt in
  pop opt;
  model

let maximize (opt : t) (e : Expression.t) (pc : Expression.t list) :
    Expression.value option =
  let model = check opt e pc Optimize.maximize in
  Option.value_map model ~default:None ~f:(fun m -> value_of_const m e)

let minimize (opt : t) (e : Expression.t) (pc : Expression.t list) :
    Expression.value option =
  let model = check opt e pc Optimize.minimize in
  Option.value_map model ~default:None ~f:(fun m -> value_of_const m e)

exception Unknown

let ( let+ ) o f = Option.map f o

module Make (Mappings : Mappings_intf.S) = struct
  open Core
  module Expr = Expression

  let solver_time = ref 0.0

  let solver_count = ref 0

  let time_call f acc =
    let start = Caml.Sys.time () in
    let ret = f () in
    acc := !acc +. (Caml.Sys.time () -. start);
    ret

  type t = Mappings.solver

  type solver = t

  let create () : t = Mappings.mk_solver ()

  let interrupt () = Mappings.interrupt ()

  let clone (solver : t) : t = Mappings.translate solver

  let push (_solver : t) : unit = assert false (* TODO *)

  let pop (_solver : t) (_lvl : int) : unit = assert false (* TODO *)

  let reset (_solver : t) : unit = assert false (* TODO *)

  let add (solver : t) (es : Expr.t list) : unit =
    Mappings.add_solver solver es

  let get_assertions (_solver : t) : Expr.t list = assert false

  let check (solver : t) (es : Expr.t list) : bool =
    let b =
      solver_count := !solver_count + 1;
      let sat = time_call (fun () -> Mappings.check solver es) solver_time in
      match Mappings.satisfiability sat with
      | Mappings_intf.Satisfiable -> true
      | Mappings_intf.Unknown -> raise Unknown
      | Mappings_intf.Unsatisfiable -> false
    in
    b

  let model ?(symbols : Symbol.t list option) (solver : t) : Model.t Option.t =
    let+ m = Mappings.get_model solver in
    Mappings.value_binds ?symbols m
end

module Make' (M : Mappings_intf.S) : Solver_intf.S = Make (M)

exception Unknown

let ( let+ ) o f = Option.map f o

module Make (Mappings : Mappings_intf.S) = struct
  open Core
  module Expr = Expression

  type solver = Mappings.solver

  type t =
    { solver : solver
    ; mutable pc : Expr.t list
    }

  let solver_time = ref 0.0

  let solver_count = ref 0

  let time_call f acc =
    let start = Stdlib.Sys.time () in
    let ret = f () in
    acc := !acc +. (Stdlib.Sys.time () -. start);
    ret

  let create () = { solver = Mappings.mk_solver (); pc = [] }

  let interrupt () = Mappings.interrupt ()

  let clone (s : t) : t = { s with pc = s.pc }

  let push (_s : t) : unit = assert false (* TODO *)

  let pop (_s : t) (_lvl : int) : unit = assert false (* TODO *)

  let reset (s : t) = s.pc <- []

  let add (s : t) (es : Expr.t list) : unit = s.pc <- es @ s.pc

  let get_assertions (_s : t) : Expr.t list = assert false

  let check (s : t) (es : Expr.t list) : bool =
    let es' = es @ s.pc in
    solver_count := !solver_count + 1;
    let sat = time_call (fun () -> Mappings.check s.solver es') solver_time in
    match Mappings.satisfiability sat with
    | Mappings_intf.Satisfiable -> true
    | Mappings_intf.Unsatisfiable -> false
    | Mappings_intf.Unknown -> raise Unknown

  let model ?(symbols : Symbol.t list option) (s : t) : Model.t Option.t =
    let+ m = Mappings.get_model s.solver in
    Mappings.value_binds ?symbols m
end

module Make' (M : Mappings_intf.S) : Solver_intf.S = Make (M)

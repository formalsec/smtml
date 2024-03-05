type 'a state =
  { stmts : Ast.t list
  ; smap : (string, Ty.t) Hashtbl.t
  ; pc : Expr.t list
  ; solver : 'a
  }

module type S = sig
  type solver

  type exec_state

  val start : ?state:exec_state -> Ast.t list -> exec_state
end

module type Intf = sig
  module Make (Solver : Solver_intf.S) :
    S with type solver = Solver.t and type exec_state = Solver.t state
end

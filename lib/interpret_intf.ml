module type S = sig
  type exec_state

  val start : ?state:exec_state -> Ast.t list -> exec_state
end

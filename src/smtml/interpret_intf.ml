(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2024 formalsec *)
(* Written by the Smtml programmers *)

type 'a state =
  { stmts : Ast.script
  ; smap : (string, Ty.t) Hashtbl.t
  ; solver : 'a
  }

module type S = sig
  type solver

  type exec_state

  val start : ?state:exec_state -> Ast.script -> exec_state
end

module type Intf = sig
  module Make (Solver : Solver_intf.S) :
    S with type solver = Solver.t and type exec_state = Solver.t state
end

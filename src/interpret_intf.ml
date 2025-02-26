(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2024 formalsec *)
(* Written by the Smtml programmers *)

(** {1 Execution State and Interpreter Interface} *)

(** Represents the execution state of the interpreter *)
type 'a state =
  { stmts : Ast.script  (** The script being executed *)
  ; smap : (string, Ty.t) Hashtbl.t  (** A mapping of variable names to types *)
  ; solver : 'a  (** The underlying solver instance *)
  }

(** {2 Interpreter Interface} *)
module type S = sig
  (** The type representing a solver *)
  type solver

  (** The type representing the execution state of interpreter *)
  type exec_state

  (** Initializes execution with an optional state *)
  val start : ?state:exec_state -> Ast.script -> exec_state
end

(** {2 Interpreter functor interface} *)
module type Intf = sig
  (** Functor for creating a solver execution module *)
  module Make (Solver : Solver_intf.S) :
    S with type solver = Solver.t and type exec_state = Solver.t state
end

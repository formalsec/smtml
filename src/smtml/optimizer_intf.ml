(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2024 formalsec *)
(* Written by the Smtml programmers *)

(** Optimizer Module. This module defines interfaces for interacting with
    optimization solvers, including constraint management, optimization
    objectives, and result retrieval. It provides a generic interface for
    working with different optimization backends. *)

(** {1 Module Types} *)

(** The [S] module type defines the core interface for interacting with
    optimization solvers. *)
module type S = sig
  (** The type of optimization solvers. *)
  type t

  (** [create ()] creates a new optimization solver. *)
  val create : unit -> t

  (** [push solver] pushes a new context level onto the solver [solver]. *)
  val push : t -> unit

  (** [pop solver] pops a context level from the solver [solver]. *)
  val pop : t -> unit

  (** [add solver exprs] adds the expressions [exprs] to the solver [solver]. *)
  val add : t -> Expr.t list -> unit

  (** [protect solver f] executes the function [f] within a protected context,
      ensuring that the solver state is restored after execution. *)
  val protect : t -> (unit -> 'a) -> 'a

  (** [check solver] checks the satisfiability of the solver [solver]. Returns
      [`Sat], [`Unsat], or [`Unknown]. *)
  val check : t -> [ `Sat | `Unsat | `Unknown ]

  (** [model solver] retrieves the model from the solver [solver], if one
      exists. *)
  val model : t -> Model.t option

  (** [maximize solver expr] maximizes the expression [expr] in the solver
      [solver]. Returns the optimal value if found. *)
  val maximize : t -> Expr.t -> Value.t option

  (** [minimize solver expr] minimizes the expression [expr] in the solver
      [solver]. Returns the optimal value if found. *)
  val minimize : t -> Expr.t -> Value.t option

  (** [get_statistics solver] retrieves statistics from the solver [solver]. *)
  val get_statistics : t -> Statistics.t
end

(** The [Intf] module type defines the interface for creating and working with
    optimization solvers, including a functor for instantiating solvers and a
    predefined Z3 solver implementation. *)
module type Intf = sig
  (** The [S] module type, which defines the core optimizer interface. *)
  module type S = S

  (** [Make] is a functor that creates an optimizer instance from a given
      mappings module. *)
  module Make (_ : Mappings_intf.S) : S

  (** [Z3] is a predefined optimizer implementation using the Z3 solver. *)
  module Z3 : S
end

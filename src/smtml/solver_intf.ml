(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2025 formalsec *)
(* Written by the Smtml programmers *)

(** Solver Interface Module. This module defines interfaces for interacting with
    SMT solvers, including batch and incremental modes. It provides a generic
    interface for working with different SMT solvers and their functionalities.
*)

(** {1 Module Types} *)

(** The [S] module type defines the core interface for interacting with SMT
    solvers, including solver creation, constraint management, and result
    retrieval. *)
module type S = sig
  (** The type of solvers. *)
  type t

  (** The type of underlying solver instances. *)
  type solver

  (** [solver_time] tracks the time spent inside the SMT solver. *)
  val solver_time : float ref

  (** [solver_count] tracks the number of queries made to the SMT solver. *)
  val solver_count : int ref

  (** [pp_statistics fmt solver] pretty-prints solver statistics using the
      formatter [fmt]. *)
  val pp_statistics : t Fmt.t

  (** [create ?params ?logic ()] creates a new solver.

      - [?params] is of type {!type:Params.t} and is used to modify/set
        parameters inside the solver.
      - [?logic] is of type {!type:Logic.t} and is used to set the theory of the
        assertions used. When the underlying theory is known, setting this
        parameter can improve solver performance. The default logic is {e ALL}.
  *)
  val create : ?params:Params.t -> ?logic:Logic.t -> unit -> t

  (** [interrupt solver] interrupts the current solver operation. *)
  val interrupt : t -> unit

  (** [clone solver] creates a copy of the solver [solver]. *)
  val clone : t -> t

  (** [push solver] creates a backtracking point in the solver [solver]. *)
  val push : t -> unit

  (** [pop solver n] backtracks [n] backtracking points in the solver [solver].
  *)
  val pop : t -> int -> unit

  (** [reset solver] resets the solver [solver], removing all assertions. *)
  val reset : t -> unit

  (** [add solver exprs] asserts one or multiple constraints [exprs] into the
      solver [solver]. *)
  val add : t -> Expr.t list -> unit

  (** [add_set solver set] asserts constraints from the set [set] into the
      solver [solver]. *)
  val add_set : t -> Expr.Set.t -> unit

  (** [get_assertions solver] retrieves the set of assertions in the solver
      [solver]. *)
  val get_assertions : t -> Expr.t list
  [@@deprecated "Please use 'get_statistics' instead"]

  (** [get_statistics solver] retrieves statistics from the solver [solver]. *)
  val get_statistics : t -> Statistics.t

  (** [check solver es] checks the satisfiability of the assertions in the
      solver [solver] using the assumptions in [es]. Returns [`Sat], [`Unsat],
      or [`Unknown]. *)
  val check : t -> Expr.t list -> [ `Sat | `Unsat | `Unknown ]

  (** [check_set solver set] checks the satisfiability of the assertions in the
      solver [solver] using the assumptions in the set [set]. Returns [`Sat],
      [`Unsat], or [`Unknown]. *)
  val check_set : t -> Expr.Set.t -> [ `Sat | `Unsat | `Unknown ]

  (** [get_value solver expr] retrieves an expression denoting the model value
      of the given expression [expr]. Requires that the last {!val:check} query
      returned [`Sat]. *)
  val get_value : t -> Expr.t -> Expr.t

  (** [model ?symbols solver] retrieves the model of the last [check] query. If
      [?symbols] is provided, only the values of the specified symbols are
      included. Returns [None] if [check] was not invoked before or its result
      was not [`Sat]. *)
  val model : ?symbols:Symbol.t list -> t -> Model.t option
end

(** The [Intf] module type defines the interface for creating and working with
    solvers, including batch, cached, and incremental modes. *)
module type Intf = sig
  (** The [S] module type, which defines the core solver interface. *)
  module type S = S

  (** {1 Batch Mode}

      In this module, constraints are handled in a 'batch' mode, meaning that
      the solver delays all interactions with the underlying SMT solver until it
      becomes necessary. It communicates with the underlying solver only when a
      call to {!val:Solver_intf.S.check}, {!val:Solver_intf.S.get_value}, or
      {!val:Solver_intf.S.model} is made. *)

  (** The {!module:Batch} module is parameterized by the mapping module [M]
      implementing {!module-type:Mappings_intf.S}. In this mode, the solver
      delays all interactions with the underlying SMT solver until necessary. *)
  module Batch (_ : Mappings_intf.S) : S

  (** {1 Cached Mode}

      (Experimental) Similar to the Batch mode, but queries are cached for
      improved performance. *)
  module Cached (_ : Mappings_intf.S) : sig
    (** Include the core solver interface. *)
    include S

    (** [cache_hits ()] Returns the number of hits that have already occurred in
        the cache *)
    val cache_hits : unit -> int

    (** [cache_misses ()] Same as [cache_hits] but for misses *)
    val cache_misses : unit -> int
  end

  (** {1 Incremental Mode}

      In the Incremental module, constraints are managed incrementally, meaning
      that every interaction with the solver prompts a corresponding interaction
      with the underlying SMT solver. Unlike the batch solver, nearly every
      interaction with this solver involves the underlying SMT solver. *)

  (** The {!module:Incremental} module, like {!module:Batch}, presents a solver
      parameterized by the mapping module [M]. In this mode, the Incremental
      solver engages with the underlying SMT solver in nearly every interaction.
  *)
  module Incremental (_ : Mappings_intf.S) : S
end

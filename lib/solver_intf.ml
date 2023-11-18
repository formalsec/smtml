module type S = sig
  type t
  type solver

  (** Time spent inside SMT solver. *)
  val solver_time : float ref

  (** Number of queries to the SMT solver. *)
  val solver_count : int ref

  (** Create a new solver. *)
  val create : ?params:Params.t -> unit -> t

  (** Interrupt solver. *)
  val interrupt : unit -> unit

  (** Clone a given solver. *)
  val clone : t -> t

  (** Create a backtracking point. *)
  val push : t -> unit

  (** [pop solver n] backtracks [n] backtracking points. *)
  val pop : t -> int -> unit

  (** Resets the solver, i.e., remove all assertions from the solver. *)
  val reset : t -> unit

  (** Assert one or multiple constraints into the solver. *)
  val add : t -> Expr.t list -> unit

  (** The set of assertions in the solver. *)
  val get_assertions : t -> Expr.t list

  (** Checks the satisfiability of the assertions.

      Raises [Unknown] if SMT solver returns unknown. *)
  val check : t -> Expr.t list -> bool

  (** [get_value solver e] get an expression denoting the model value of a given
      expression.

      Requires that the last {!val:check} query returned [true].

      @param t The solver.
      @param e Expr to query a model for.

      @return An expression denoting the model value of [e]. *)
  val get_value : t -> Expr.t -> Expr.t

  (** The model of the last [check].

      The result is [None] if [check] was not invoked before, or its result was
      not [Satisfiable]. *)
  val model : ?symbols:Symbol.t list -> t -> Model.t option
end

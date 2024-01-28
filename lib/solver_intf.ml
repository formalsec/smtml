type logic =
  | AUFLIA
  | AUFLIRA
  | AUFNIRA
  | LIA
  | LRA
  | QF_ABV
  | QF_AUFBV
  | QF_AUFLIA
  | QF_AX
  | QF_BV
  | QF_BVFP
  | QF_IDL
  | QF_LIA
  | QF_LRA
  | QF_NIA
  | QF_NRA
  | QF_RDL
  | QF_UF
  | QF_UFBV
  | QF_UFIDL
  | QF_UFLIA
  | QF_UFLRA
  | QF_UFNRA
  | UFLRA
  | UFNIA

module type S = sig
  type t

  type solver

  (** Time spent inside SMT solver. *)
  val solver_time : float ref

  (** Number of queries to the SMT solver. *)
  val solver_count : int ref

  (** Print solver statistics. *)
  val pp_statistics : Format.formatter -> t -> unit

  (** [create ?params ?logic ()] creates a new solver.

      [?params] is of type {!type:Params.t} and is used to modify/set parameters
      inside the solver.

      [?logic] is of type {!type:Solver_intf.logic} and is used to set the
      theory of the assertions used. When knowing what the underlying theory is
      going to be, setting this parameter can help the SMT solver be more
      performant. The default logic is {e unknown_theory}. *)
  val create : ?params:Params.t -> ?logic:logic -> unit -> t

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

  (** [check solver es] checks the satisfiability of the assertions in the
      solver using the assumptions in [es].

      Raises [Unknown] if the SMT solver returns unknown. *)
  val check : t -> Expr.t list -> bool

  (** [get_value solver e] get an expression denoting the model value of a given
      expression.

      Requires that the last {!val:check} query returned [true]. *)
  val get_value : t -> Expr.t -> Expr.t

  (** The model of the last [check].

      The result is [None] if [check] was not invoked before, or its result was
      not [Satisfiable]. *)
  val model : ?symbols:Symbol.t list -> t -> Model.t option
end

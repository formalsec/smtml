type satisfiability =
  | Satisfiable
  | Unsatisfiable
  | Unknown

module type S = sig
  type model
  type solver
  type status
  type optimize
  type handle

  exception Error of string

  val update_param_value : 'a Params.param -> 'a -> unit

  val mk_solver : unit -> solver

  val interrupt : unit -> unit

  val translate : solver -> solver

  val push : solver -> unit

  val pop : solver -> int -> unit

  val reset : solver -> unit

  val add_solver : solver -> Expression.t list -> unit

  val check : solver -> Expression.t list -> status

  val satisfiability : status -> satisfiability

  val solver_model : solver -> model option

  val value : model -> Types.expr_type -> Expression.t -> Value.t

  val values_of_model : ?symbols:Symbol.t list -> model -> Model.t

  val expr_to_smtstring : Expression.t list -> bool -> string

  val mk_optimize : unit -> optimize

  val add_optimize : optimize -> Expression.t list -> unit

  val maximize : optimize -> Expression.t -> handle

  val minimize : optimize -> Expression.t -> handle

  val optimize_model : optimize -> model option
end

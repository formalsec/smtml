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

  val update_param_value : 'a Params.param -> 'a -> unit
  val interrupt : unit -> unit
  val satisfiability : status -> satisfiability
  val value : model -> Expr.t -> Value.t
  val values_of_model : ?symbols:Symbol.t list -> model -> Model.t
  val pp_smt : ?status:bool -> Format.formatter -> Expr.t list -> unit
  val set_debug : bool -> unit

  module Solver : sig
    val make : ?logic:Ty.logic -> unit -> solver
    val add_simplifier : solver -> solver
    val clone : solver -> solver
    val push : solver -> unit
    val pop : solver -> int -> unit
    val reset : solver -> unit
    val add : solver -> Expr.t list -> unit
    val check : solver -> assumptions:Expr.t list -> status
    val model : solver -> model option
    val pp_statistics : Format.formatter -> solver -> unit
  end

  module Optimizer : sig
    val make : unit -> optimize
    val push : optimize -> unit
    val pop : optimize -> unit
    val add : optimize -> Expr.t list -> unit
    val check : optimize -> status
    val model : optimize -> model option
    val maximize : optimize -> Expr.t -> handle
    val minimize : optimize -> Expr.t -> handle
    val pp_statistics : Format.formatter -> optimize -> unit
  end
end

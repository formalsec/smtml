open Base
open Z3
open Types
open Expression

type t = { solver : s; pc : pc ref }
and s = Solver.solver

val time_solver : float ref
val create : unit -> t
val clone : t -> t
val interrupt : unit -> unit
val add : t -> Expression.t -> unit
val check : t -> Expression.t list -> bool
val fork : t -> Expression.t -> bool * bool
val model : t -> Model.model
val get_model : t -> Model.model

val model_binds :
  Model.model -> (string * num_type) list -> (string * Num.t) list

val value_binds : t -> (string * num_type) list -> (string * Num.t) list

val string_binds :
  t -> (string * num_type) list -> (string * string * string) list

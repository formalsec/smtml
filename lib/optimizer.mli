open Z3_mappings

type t

exception Unknown

val solver_time : float ref
val create : unit -> t
val push : t -> unit
val pop : t -> unit
val add : t -> Expr.t list -> unit

val check :
     t
  -> Expr.t
  -> Expr.t list
  -> (t -> Expr.t -> Z3.Optimize.handle)
  -> model option

val maximize : t -> Expr.t -> Expr.t list -> Value.t option
val minimize : t -> Expr.t -> Expr.t list -> Value.t option

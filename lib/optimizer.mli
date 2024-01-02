module Z3 : sig
  type t

  val create : unit -> t
  val push : t -> unit
  val pop : t -> unit
  val add : t -> Expr.t list -> unit
  val check : t -> Mappings_intf.satisfiability
  val model : t -> Model.t option
  val maximize : t -> Expr.t -> Value.t option
  val minimize : t -> Expr.t -> Value.t option
end

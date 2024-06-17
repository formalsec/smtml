include Mappings_intf

module type S = sig
  type t

  val create : unit -> t

  val push : t -> unit

  val pop : t -> unit

  val add : t -> Expr.t list -> unit

  val protect : t -> (unit -> 'a) -> 'a

  val check : t -> Mappings_intf.satisfiability

  val model : t -> Model.t option

  val maximize : t -> Expr.t -> Value.t option

  val minimize : t -> Expr.t -> Value.t option
end

module type Intf = sig
  type nonrec satisfiability = satisfiability

  module type S = S

  module Make (_ : Mappings_intf.S) : S

  module Z3 : S
end

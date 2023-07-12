module type S = sig
  type t
  type solver

  val create : unit -> t

  val interrupt : unit -> unit

  val clone : t -> t

  val add : t -> Expression.t list -> unit

  val get_assertions : t -> Expression.t list

  val check : t -> Expression.t list -> bool

  val model : ?symbols:Symbol.t list -> t -> Model.t option
end

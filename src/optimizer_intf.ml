(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2024 formalsec *)
(* Written by the Smtml programmers *)

include Mappings_intf

module type S = sig
  type t

  val create : unit -> t

  val push : t -> unit

  val pop : t -> unit

  val add : t -> Expr.t list -> unit

  val protect : t -> (unit -> 'a) -> 'a

  val check : t -> [ `Sat | `Unsat | `Unknown ]

  val model : t -> Model.t option

  val maximize : t -> Expr.t -> Value.t option

  val minimize : t -> Expr.t -> Value.t option

  val get_statistics : t -> Statistics.t
end

module type Intf = sig
  module type S = S

  module Make (_ : Mappings_intf.S) : S

  module Z3 : S
end

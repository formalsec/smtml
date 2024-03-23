open Ty

exception DivideByZero

exception Value of Ty.t

exception TypeError of int * Value.t * Ty.t

val unop : Ty.t -> unop -> Value.t -> Value.t

val binop : Ty.t -> binop -> Value.t -> Value.t -> Value.t

val triop : Ty.t -> triop -> Value.t -> Value.t -> Value.t -> Value.t

val relop : Ty.t -> relop -> Value.t -> Value.t -> bool

val cvtop : Ty.t -> cvtop -> Value.t -> Value.t

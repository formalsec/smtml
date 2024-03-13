open Ty

exception DivideByZero

exception Value of Ty.t

exception TypeError of int * Value.t * Ty.t

val eval_unop : Ty.t -> unop -> Value.t -> Value.t

val eval_binop : Ty.t -> binop -> Value.t -> Value.t -> Value.t

val eval_relop : Ty.t -> relop -> Value.t -> Value.t -> bool

val eval_cvtop : Ty.t -> cvtop -> Value.t -> Value.t

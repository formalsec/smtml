open Ty

exception DivideByZero

exception Num of Ty.t

exception TypeError of int * Num.t * Ty.t

val eval_unop : Ty.t -> unop -> Num.t -> Num.t

val eval_binop : Ty.t -> binop -> Num.t -> Num.t -> Num.t

val eval_relop : Ty.t -> relop -> Num.t -> Num.t -> bool

val eval_cvtop : Ty.t -> cvtop -> Num.t -> Num.t

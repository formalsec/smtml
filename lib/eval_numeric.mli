open Ty

exception Num of Ty.t

exception Type_error of int * Num.t * Ty.t

exception Integer_overflow

exception Conversion_to_integer

val eval_unop : Ty.t -> unop -> Num.t -> Num.t

val eval_binop : Ty.t -> binop -> Num.t -> Num.t -> Num.t

val eval_relop : Ty.t -> relop -> Num.t -> Num.t -> bool

val eval_cvtop : Ty.t -> cvtop -> Num.t -> Num.t

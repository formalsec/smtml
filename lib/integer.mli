val mk_val : int -> Expression.t
val mk_neg : Expression.t -> Expression.t
val mk_add : Expression.t -> Expression.t -> Expression.t
val mk_sub : Expression.t -> Expression.t -> Expression.t
val mk_mul : Expression.t -> Expression.t -> Expression.t
val mk_div : Expression.t -> Expression.t -> Expression.t
val mk_rem : Expression.t -> Expression.t -> Expression.t
val mk_shl : Expression.t -> Expression.t -> Expression.t
val mk_shr_a : Expression.t -> Expression.t -> Expression.t
val mk_shr_l : Expression.t -> Expression.t -> Expression.t
val mk_and : Expression.t -> Expression.t -> Expression.t
val mk_or : Expression.t -> Expression.t -> Expression.t
val mk_xor : Expression.t -> Expression.t -> Expression.t
val mk_eq : Expression.t -> Expression.t -> Expression.t
val mk_ne : Expression.t -> Expression.t -> Expression.t
val mk_lt : Expression.t -> Expression.t -> Expression.t
val mk_le : Expression.t -> Expression.t -> Expression.t
val mk_gt : Expression.t -> Expression.t -> Expression.t
val mk_ge : Expression.t -> Expression.t -> Expression.t

val mk_val : float -> Types.num_type -> Expression.t
val mk_neg : Expression.t -> Types.num_type -> Expression.t
val mk_abs : Expression.t -> Types.num_type -> Expression.t
val mk_sqrt : Expression.t -> Types.num_type -> Expression.t
val mk_nearest : Expression.t -> Types.num_type -> Expression.t
val mk_is_nan : Expression.t -> Types.num_type -> Expression.t
val mk_add : Expression.t -> Expression.t -> Types.num_type -> Expression.t
val mk_sub : Expression.t -> Expression.t -> Types.num_type -> Expression.t
val mk_mul : Expression.t -> Expression.t -> Types.num_type -> Expression.t
val mk_div : Expression.t -> Expression.t -> Types.num_type -> Expression.t
val mk_min : Expression.t -> Expression.t -> Types.num_type -> Expression.t
val mk_max : Expression.t -> Expression.t -> Types.num_type -> Expression.t
val mk_rem : Expression.t -> Expression.t -> Types.num_type -> Expression.t
val mk_eq : Expression.t -> Expression.t -> Types.num_type -> Expression.t
val mk_ne : Expression.t -> Expression.t -> Types.num_type -> Expression.t
val mk_lt : Expression.t -> Expression.t -> Types.num_type -> Expression.t
val mk_le : Expression.t -> Expression.t -> Types.num_type -> Expression.t
val mk_gt : Expression.t -> Expression.t -> Types.num_type -> Expression.t
val mk_ge : Expression.t -> Expression.t -> Types.num_type -> Expression.t

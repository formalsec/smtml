val mk_val : float -> Types.num_type -> Expression.t
(** [mk_val f] creates a concrete floating-point value. *)

val mk_neg : Expression.t -> Types.num_type -> Expression.t
(** [mk_neg f] create an expression representing [-f]. *)

val mk_abs : Expression.t -> Types.num_type -> Expression.t
(** [mk_abs f] create an expression representing [abs(f)]. *)

val mk_sqrt : Expression.t -> Types.num_type -> Expression.t
(** [mk_sqrt f] create an expression representing [sqrt(f)]. *)

val mk_nearest : Expression.t -> Types.num_type -> Expression.t
(** [mk_nearest f] create an expression representing [round_nearest(f)]. *)

val mk_is_nan : Expression.t -> Types.num_type -> Expression.t
(** [mk_is_nan f] create an expression representing [is_nan(f)]. *)

val mk_add : Expression.t -> Expression.t -> Types.num_type -> Expression.t
(** [mk_add f1 f2] create an expression representing [f1 + f2]. *)

val mk_sub : Expression.t -> Expression.t -> Types.num_type -> Expression.t
(** [mk_sub f1 f2] create an expression representing [f1 - f2]. *)

val mk_mul : Expression.t -> Expression.t -> Types.num_type -> Expression.t
(** [mk_mul f1 f2] create an expression representing [f1 * f2]. *)

val mk_div : Expression.t -> Expression.t -> Types.num_type -> Expression.t
(** [mk_div f1 f2] create an expression representing [f1 / f2]. *)

val mk_min : Expression.t -> Expression.t -> Types.num_type -> Expression.t
(** [mk_min f1 f2] create an expression representing [min f1 f2]. *)

val mk_max : Expression.t -> Expression.t -> Types.num_type -> Expression.t
(** [mk_max f1 f2] create an expression representing [max f1 f2]. *)

val mk_rem : Expression.t -> Expression.t -> Types.num_type -> Expression.t
(** [mk_rem f1 f2] create an expression representing [f1 % f2]. *)

val mk_eq : Expression.t -> Expression.t -> Types.num_type -> Expression.t
(** [mk_eq f1 f2] create an expression representing [f1 = f2]. *)

val mk_ne : Expression.t -> Expression.t -> Types.num_type -> Expression.t
(** [mk_ne f1 f2] create an expression representing [not (f1 = f2)]. *)

val mk_lt : Expression.t -> Expression.t -> Types.num_type -> Expression.t
(** [mk_lt f1 f2] create an expression representing [f1 < f2]. *)

val mk_le : Expression.t -> Expression.t -> Types.num_type -> Expression.t
(** [mk_le f1 f2] create an expression representing [f1 <= f2]. *)

val mk_gt : Expression.t -> Expression.t -> Types.num_type -> Expression.t
(** [mk_gt f1 f2] create an expression representing [f1 > f2]. *)

val mk_ge : Expression.t -> Expression.t -> Types.num_type -> Expression.t
(** [mk_ge f1 f2] create an expression representing [f1 >= f2]. *)

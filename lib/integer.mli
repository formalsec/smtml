val mk_val : int -> Expression.t
(** [mk_val i] creates a concrete integer value. *)

val mk_neg : Expression.t -> Expression.t
(** [mk_neg i] create an expression representing [-i]. *)

val mk_add : Expression.t -> Expression.t -> Expression.t
(** [mk_add i1 i2] create an expression representing [i1 + i2]. *)

val mk_sub : Expression.t -> Expression.t -> Expression.t
(** [mk_sub i1 i2] create an expression representing [i1 - i2]. *)

val mk_mul : Expression.t -> Expression.t -> Expression.t
(** [mk_mul i1 i2] create an expression representing [i1 * i2]. *)

val mk_div : Expression.t -> Expression.t -> Expression.t
(** [mk_div i1 i2] create an expression representing [i1 / i2]. *)

val mk_rem : Expression.t -> Expression.t -> Expression.t
(** [mk_rem i1 i2] create an expression representing [i1 % i2]. *)

val mk_shl : Expression.t -> Expression.t -> Expression.t
(** [mk_shl i1 i2] create an expression representing [i1 << i2]. *)

val mk_shr_a : Expression.t -> Expression.t -> Expression.t
(** [mk_shr_a i1 i2] create an expression representing [i1 >> i2]. *)

val mk_shr_l : Expression.t -> Expression.t -> Expression.t
(** [mk_shr_l i1 i2] create an expression representing [i1 >> i2]. *)

val mk_and : Expression.t -> Expression.t -> Expression.t
(** [mk_and i1 i2] create an expression representing [i1 & i2]. *)

val mk_or : Expression.t -> Expression.t -> Expression.t
(** [mk_or i1 i2] create an expression representing [i1 | i2]. *)

val mk_xor : Expression.t -> Expression.t -> Expression.t
(** [mk_xor i1 i2] create an expression representing [i1 xor i2]. *)

val mk_eq : Expression.t -> Expression.t -> Expression.t
(** [mk_eq i1 i2] create an expression representing [i1 = i2]. *)

val mk_ne : Expression.t -> Expression.t -> Expression.t
(** [mk_ne i1 i2] create an expression representing [not (i1 = i2)]. *)

val mk_lt : Expression.t -> Expression.t -> Expression.t
(** [mk_lt i1 i2] create an expression representing [i1 < i2]. *)

val mk_le : Expression.t -> Expression.t -> Expression.t
(** [mk_le i1 i2] create an expression representing [i1 <= i2]. *)

val mk_gt : Expression.t -> Expression.t -> Expression.t
(** [mk_gt i1 i2] create an expression representing [i1 > i2]. *)

val mk_ge : Expression.t -> Expression.t -> Expression.t
(** [mk_ge i1 i2] create an expression representing [i1 >= i2]. *)

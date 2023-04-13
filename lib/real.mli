val mk_val : float -> Expression.t
(** [mk_val f] creates a concrete floating-point value. *)

val mk_neg : Expression.t -> Expression.t
(** [mk_neg f] create an expression representing [-f]. *)

val mk_add : Expression.t -> Expression.t -> Expression.t
(** [mk_add f1 f2] create an expression representing [f1 + f2]. *)

val mk_sub : Expression.t -> Expression.t -> Expression.t
(** [mk_sub f1 f2] create an expression representing [f1 - f2]. *)

val mk_mul : Expression.t -> Expression.t -> Expression.t
(** [mk_mul f1 f2] create an expression representing [f1 * f2]. *)

val mk_div : Expression.t -> Expression.t -> Expression.t
(** [mk_div f1 f2] create an expression representing [f1 / f2]. *)

val mk_eq : Expression.t -> Expression.t -> Expression.t
(** [mk_eq f1 f2] create an expression representing [f1 = f2]. *)

val mk_ne : Expression.t -> Expression.t -> Expression.t
(** [mk_ne f1 f2] create an expression representing [not (f1 = f2)]. *)

val mk_lt : Expression.t -> Expression.t -> Expression.t
(** [mk_lt f1 f2] create an expression representing [f1 < f2]. *)

val mk_le : Expression.t -> Expression.t -> Expression.t
(** [mk_le f1 f2] create an expression representing [f1 <= f2]. *)

val mk_gt : Expression.t -> Expression.t -> Expression.t
(** [mk_gt f1 f2] create an expression representing [f1 > f2]. *)

val mk_ge : Expression.t -> Expression.t -> Expression.t
(** [mk_ge f1 f2] create an expression representing [f1 >= f2]. *)

val mk_to_string : Expression.t -> Expression.t
(** [mk_to_string f] create an expression representing a string *)

val mk_of_string : Expression.t -> Expression.t
(** [mk_of_string f] create an expression representing a real *)

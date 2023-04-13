val mk_val : bool -> Expression.t
(** [mk_val b] creates a concrete boolean value. *)

val mk_not : Expression.t -> Expression.t
(** [mk_not e] create an expression representing [not e]. *)

val mk_and : Expression.t -> Expression.t -> Expression.t
(** [mk_and e1 e2] create an expression representing [e1 and e2]. *)

val mk_or : Expression.t -> Expression.t -> Expression.t
(** [mk_or e1 e2] create an expression representing [e1 or e2]. *)

val mk_xor : Expression.t -> Expression.t -> Expression.t
(** [mk_xor e1 e2] create an expression representing [e1 xor e2]. *)

val mk_eq : Expression.t -> Expression.t -> Expression.t
(** [mk_eq e1 e2] create an expression representing [e1 = e2]. *)

val mk_ne : Expression.t -> Expression.t -> Expression.t
(** [mk_ne e1 e2] create an expression representing [not (e1 = e2)]. *)

val mk_val : String.t -> Expression.t
(** [mk_val s] creates a concrete string value. *)

val mk_len : Expression.t -> Expression.t
(** [mk_len s] create an expression representing [length s]. *)

val mk_nth : Expression.t -> Expression.t -> Expression.t
(** [mk_nth s i] create an expression representing [s.(i)]. *)

val mk_concat : Expression.t -> Expression.t -> Expression.t
(** [mk_concat s1 s2] create an expression representing [s1 ^ s2]. *)

val mk_eq : Expression.t -> Expression.t -> Expression.t
(** [mk_eq s1 s2] create an expression representing [s1 = s2]. *)

val mk_ne : Expression.t -> Expression.t -> Expression.t
(** [mk_ne s1 s2] create an expression representing [not (s1 = s2)]. *)

val mk_substr :
  Expression.t -> pos:Expression.t -> len:Expression.t -> Expression.t
(** [mk_substr s pos len] create an expression representing the substring of 
    [s] starting in [pos] and with length [len]. *)

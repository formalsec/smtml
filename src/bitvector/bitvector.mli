type t

val make : Z.t -> int -> t

val view : t -> Z.t

val numbits : t -> int

val equal : t -> t -> bool

val compare : t -> t -> int

val pp : Format.formatter -> t -> unit

val neg : t -> t

val lognot : t -> t

val clz : t -> int

val ctz : t -> int

val popcnt : t -> int

val add : t -> t -> t

val sub : t -> t -> t

val mul : t -> t -> t

val div : t -> t -> t

val div_u : t -> t -> t

val logand : t -> t -> t

val logor : t -> t -> t

val logxor : t -> t -> t

val shl : t -> t -> t

val ashr : t -> t -> t

val lshr : t -> t -> t

val rem : t -> t -> t

val rem_u : t -> t -> t

val rotate_left : t -> int -> t

val rotate_right : t -> int -> t

val lt : t -> t -> bool

val lt_u : t -> t -> bool

val gt : t -> t -> bool

val gt_u : t -> t -> bool

val le : t -> t -> bool

val le_u : t -> t -> bool

val ge : t -> t -> bool

val ge_u : t -> t -> bool

val concat : t -> t -> t

val extract : t -> high:int -> low:int -> t

val zero_extend : int -> t -> t

val sign_extend : int -> t -> t

type t

val fresh : unit -> t

val compare : t -> t -> int

val equal : t -> t -> bool

val hash : t -> int

val pp : t Fmt.t

type t

val make : Ty.t -> string -> t

val ( @: ) : string -> Ty.t -> t

val equal : t -> t -> Bool.t

val compare : t -> t -> int

val ty : t -> Ty.t

val name : t -> string

val pp : Format.formatter -> t -> unit

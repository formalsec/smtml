type t =
  { ty : Ty.t
  ; name : string
  }

val ( @: ) : string -> Ty.t -> t

val make : Ty.t -> string -> t

val mk_symbol : Ty.t -> string -> t [@@deprecated "Please use 'make' instead"]

val equal : t -> t -> Bool.t

val rename : t -> string -> t

val type_of : t -> Ty.t

val to_string : t -> string

val compare : t -> t -> int

val pp : Format.formatter -> t -> unit

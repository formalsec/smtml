type t

val mk_symbol : Ty.t -> String.t -> t
val equal : t -> t -> Bool.t
val rename : t -> String.t -> t
val type_of : t -> Ty.t
val to_string : t -> String.t
val pp : Format.formatter -> t -> unit

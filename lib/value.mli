type t =
  | True
  | False
  | Int of int
  | Real of float
  | Str of string
  | Num of Num.t

val equal : t -> t -> bool
val compare : t -> t -> int
val type_of : t -> Ty.t
val pp : Format.formatter -> t -> unit
val pp_num : Format.formatter -> t -> unit
val to_string : t -> string

type t =
  | I8 of int
  | I16 of int
  | I32 of int32
  | I64 of int64
  | F16 of int
  | F32 of int32
  | F64 of int64

val ( = ) : t -> t -> bool

val compare : t -> t -> int

val ty : t -> Ty.t

val pp : Format.formatter -> t -> unit

val pp_hex : Format.formatter -> t -> unit

val to_string : t -> string

val of_bool : bool -> t

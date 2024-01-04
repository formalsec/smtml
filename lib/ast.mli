type t =
  | Declare of Symbol.t
  | Assert of Expr.t
  | CheckSat
  | GetModel

val pp : Format.formatter -> t -> unit
val to_string : t -> string

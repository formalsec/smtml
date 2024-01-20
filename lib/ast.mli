type t =
  | Let_const of Symbol.t
  | Assert of Expr.t
  | CheckSat
  | GetModel

val pp : Format.formatter -> t -> unit
val to_string : t -> string

type t =
  | Assert of term
  | Check_sat
  | Push
  | Pop of int
  | Let_const of Symbol.t
  | Get_model

and term =
  | E of Expr.t
  | Let of binding list * term

and binding = string * term

val pp : Format.formatter -> t -> unit
val to_string : t -> string

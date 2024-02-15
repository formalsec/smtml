type t =
  | Assert of Expr.t
  | Check_sat
  | Push
  | Pop of int
  | Let_const of Symbol.t
  | Get_model
  | Set_logic of Ty.logic

val pp : Format.formatter -> t -> unit
val to_string : t -> string

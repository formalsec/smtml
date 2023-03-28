open Base

type formula =
  | True
  | False
  | Not of formula
  | And of formula * formula
  | Or of formula * formula
  | Relop of Expression.t

type t = formula

val ( && ) : t -> t -> t
val ( || ) : t -> t -> t
val negate : formula -> formula
val conjunct : formula list -> formula
val length : formula -> int
val to_formulas : Expression.t list -> formula list
val to_formula : Expression.t list -> formula
val to_string : formula -> string
val pp_to_string : formula -> string
val get_vars : formula -> (string * Types.expr_type) list

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
val create : unit -> t
val add_constraint : ?neg:bool -> Expression.t -> t -> t
val negate : t -> t
val conjunct : t list -> t
val length : t -> int
val to_formulas : Expression.t list -> t list
val to_formula : Expression.t list -> t
val to_string : t -> string
val pp_to_string : t -> string

val get_symbols : t -> (string * Types.expr_type) list
(** [get_vars f] get the list of unique symbols and their types in
    the formula [f] *)

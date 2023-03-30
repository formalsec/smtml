open Base
open Z3

val ctx : Z3.context
val encode_expr : ?bool_to_bv:bool -> Expression.t -> Expr.expr
val encode_formula : Formula.t -> Expr.expr
val value_of_const : Model.model -> Expression.t -> Expression.value option

val value_binds :
  Model.model ->
  (string * Types.expr_type) list ->
  (string * Expression.value) list

val string_binds : Model.model -> (string * string * string) list

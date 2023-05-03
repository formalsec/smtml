exception Error of string

val ctx : Z3.context
val encode_expr : ?bool_to_bv:bool -> Expression.t -> Z3.Expr.expr
val expr_to_smtstring : Expression.t list -> bool -> string
val value_of_const : Z3.Model.model -> Expression.t -> Value.t option
val value_binds : Z3.Model.model -> Symbol.t list -> (Symbol.t * Value.t) list
val string_binds : Z3.Model.model -> (string * string * string) list

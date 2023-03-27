open Base
open Z3

val ctx : Z3.context
val encode_expr : ?bool_to_bv:bool -> Expression.t -> Expr.expr
val encode_formula : Formula.t -> Expr.expr
val int64_of_int : Expr.expr -> Int64.t
val int64_of_bv : Expr.expr -> Int64.t
val int64_of_fp : Expr.expr -> int -> int -> Int64.t

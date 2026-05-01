val of_unop : Ty.Unop.t -> Feature_map.feat

val of_binop : Ty.Binop.t -> Feature_map.feat

val of_triop : Ty.Triop.t -> Feature_map.feat

val of_relop : Ty.Relop.t -> Feature_map.feat

val of_cvtop : Ty.Cvtop.t -> Feature_map.feat

val of_ty : Ty.t -> Feature_map.feat

val of_naryop : Ty.Naryop.t -> Feature_map.feat

val of_expr_kind : Expr.expr -> 'a -> Feature_map.feat

val feats_to_str : string -> bool -> Feature_map.t -> string

val all_feature_names : string list

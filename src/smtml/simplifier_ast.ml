module Dsl_ast = struct
  type lhs_pattern =
    | LVar of string
    | LConstructor of string * lhs_pattern list
    | LList of lhs_pattern list
    | LInt of int

  type cond_expr =
    | CVar of string
    | CInt of int
    | CInfix of cond_expr * string * cond_expr
    | CAnd of cond_expr * cond_expr
    | COr of cond_expr * cond_expr
    | CNot of cond_expr
    | CApp of string * cond_expr list
    | CNamespacedApp of string * string * cond_expr list
    | CPat of lhs_pattern

  type rhs_expr =
    | RVar of string
    | RConstructor of string * rhs_expr list
    | RList of rhs_expr list
    | RFuncall of string * rhs_expr list
    | RNamespacedFuncall of string * string * rhs_expr list
    | RInt of int

  type rule =
    { lhs : lhs_pattern
    ; rhs : rhs_expr
    ; cond : cond_expr option
    }
end

open Core
open Types

type t [@@deriving compare, sexp, hash]

val mk_symbol : expr_type -> String.t -> t
val equal : t -> t -> Bool.t
val rename : t -> String.t -> t
val type_of : t -> expr_type
val to_string : t -> String.t

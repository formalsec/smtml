(** Term definitions of the abstract syntax *)

open Ty

type t =
  { e : expr
  ; ty : Ty.t
  }

and expr =
  | Val of Value.t
  | Ptr of int32 * t
  | Unop of unop * t
  | Binop of binop * t * t
  | Triop of triop * t * t * t
  | Relop of relop * t * t
  | Cvtop of cvtop * t
  | Symbol of Symbol.t
  | Extract of t * int * int
  | Concat of t * t

val v : expr -> Ty.t -> t
val ( @: ) : expr -> Ty.t -> t
val equal : t -> t -> bool
val hash : t -> int
val mk_symbol : Symbol.t -> t
val get_symbols : t list -> Symbol.t list
val negate_relop : t -> (t, string) Result.t
val pp : Format.formatter -> t -> unit
val pp_smt : Format.formatter -> t list -> unit
val pp_list : Format.formatter -> t list -> unit
val to_string : t -> string
val simplify : ?extract:bool -> t -> t

module Bitv : sig
  val v : 'a Ty.cast -> 'a -> t
  val not : _ cast -> t -> t
end

(** Term definitions of the abstract syntax *)

type t =
  { e : expr
  ; ty : Ty.t
  }

and expr =
  | Val of Value.t
  | Ptr of int32 * t
  | Unop of Ty.unop * t
  | Binop of Ty.binop * t * t
  | Triop of Ty.triop * t * t * t
  | Relop of Ty.relop * t * t
  | Cvtop of Ty.cvtop * t
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
  module I8 : Constructors_intf.Bitv with type elt := int and type t := t
  module I32 : Constructors_intf.Bitv with type elt := int32 and type t := t
  module I64 : Constructors_intf.Bitv with type elt := int64 and type t := t
end

(** Term definitions of the abstract syntax *)
type t = expr Hc.hash_consed

and expr =
  | Val of Value.t
  | Ptr of int32 * t
  | Symbol of Symbol.t
  | Unop of Ty.t * Ty.unop * t
  | Binop of Ty.t * Ty.binop * t * t
  | Triop of Ty.t * Ty.triop * t * t * t
  | Relop of Ty.t * Ty.relop * t * t
  | Cvtop of Ty.t * Ty.cvtop * t
  | Extract of t * int * int
  | Concat of t * t

val equal : t -> t -> bool

val hash : t -> int

val make : expr -> t

val ( @: ) : expr -> Ty.t -> t [@@deprecated "Please use 'make' instead"]

val view : t -> expr

val ty : t -> Ty.t

val mk_symbol : Symbol.t -> t

val get_symbols : t list -> Symbol.t list

val negate_relop : t -> (t, string) Result.t

val pp : Format.formatter -> t -> unit

val pp_smt : Format.formatter -> t list -> unit

val pp_list : Format.formatter -> t list -> unit

val to_string : t -> string

val unop : Ty.t -> Ty.unop -> t -> t

val binop : Ty.t -> Ty.binop -> t -> t -> t

val relop : Ty.t -> Ty.relop -> t -> t -> t

val cvtop : Ty.t -> Ty.cvtop -> t -> t

(** Fixpoint *)
val simplify : t -> t

module Hc : sig
  val clear : unit -> unit

  val stats : unit -> Hashtbl.statistics

  val length : unit -> int
end

module Bool : sig
  val v : bool -> t

  val not_ : t -> t

  val ( = ) : t -> t -> t

  val distinct : t -> t -> t

  val and_ : t -> t -> t

  val or_ : t -> t -> t

  val ite : t -> t -> t -> t
end

module Bitv : sig
  module I8 : Constructors_intf.Infix with type elt := int and type t := t

  module I32 : Constructors_intf.Infix with type elt := int32 and type t := t

  module I64 : Constructors_intf.Infix with type elt := int64 and type t := t
end

module Fpa : sig
  module F32 : Constructors_intf.Infix with type elt := float and type t := t

  module F64 : Constructors_intf.Infix with type elt := float and type t := t
end

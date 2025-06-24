include (
  Expr :
    sig
      type t = expr Hc.hash_consed

      and expr = private
        | Val of Value.t
        | Ptr of
            { base : Bitvector.t
            ; offset : t
            }
        | Symbol of Symbol.t
        | List of t list
        | App of Symbol.t * t list
        | Unop of Ty.t * Ty.Unop.t * t
        | Binop of Ty.t * Ty.Binop.t * t * t
        | Triop of Ty.t * Ty.Triop.t * t * t * t
        | Relop of Ty.t * Ty.Relop.t * t * t
        | Cvtop of Ty.t * Ty.Cvtop.t * t
        | Naryop of Ty.t * Ty.Naryop.t * t list
        | Extract of t * int * int
        | Concat of t * t
        | Binder of Binder.t * t list * t

      val view : t -> expr

      val hash : t -> int

      val equal : t -> t -> bool

      val compare : t -> t -> int

      val ty : t -> Ty.t

      val is_symbolic : t -> bool

      val get_symbols : t list -> Symbol.t list

      val negate_relop : t -> t

      val pp : t Fmt.t

      val pp_smt : t list Fmt.t

      val pp_list : t list Fmt.t

      val to_string : t -> string

      val value : Value.t -> t

      val ptr : int32 -> t -> t

      val list : t list -> t

      val symbol : Symbol.t -> t

      val app : Symbol.t -> t list -> t

      val binder : Binder.t -> t list -> t -> t

      val let_in : t list -> t -> t

      val forall : t list -> t -> t

      val exists : t list -> t -> t

      val raw_unop : Ty.t -> Ty.Unop.t -> t -> t

      val raw_binop : Ty.t -> Ty.Binop.t -> t -> t -> t

      val raw_triop : Ty.t -> Ty.Triop.t -> t -> t -> t -> t

      val raw_relop : Ty.t -> Ty.Relop.t -> t -> t -> t

      val raw_cvtop : Ty.t -> Ty.Cvtop.t -> t -> t

      val raw_naryop : Ty.t -> Ty.Naryop.t -> t list -> t

      val raw_extract : t -> high:int -> low:int -> t

      val raw_concat : t -> t -> t

      module Hc : sig
        val clear : unit -> unit

        val stats : unit -> Hashtbl.statistics

        val length : unit -> int
      end

      module Bool : sig
        val true_ : t

        val false_ : t

        val v : bool -> t
      end

      module Set : sig
        type elt = t

        type key = elt

        type t

        val empty : t

        val is_empty : t -> bool

        val mem : elt -> t -> bool

        val add : elt -> t -> t

        val singleton : elt -> t

        val cardinal : t -> int

        val is_singleton : t -> elt option

        val remove : elt -> t -> t

        val unsigned_min_elt : t -> elt

        val unsigned_max_elt : t -> elt

        val pop_unsigned_minimum : t -> (elt * t) option

        val pop_unsigned_maximum : t -> (elt * t) option

        val iter : (elt -> unit) -> t -> unit

        val filter : (elt -> bool) -> t -> t

        val for_all : (elt -> bool) -> t -> bool

        val fold : (elt -> 'acc -> 'acc) -> t -> 'acc -> 'acc

        val split : elt -> t -> t * bool * t

        val pp : Format.formatter -> t -> unit

        val union : t -> t -> t

        val inter : t -> t -> t

        val disjoint : t -> t -> bool

        val subset : t -> t -> bool

        val diff : t -> t -> t

        val min_elt_inter : t -> t -> elt option

        val max_elt_inter : t -> t -> elt option

        val to_seq : t -> elt Seq.t

        val to_rev_seq : t -> elt Seq.t

        val add_seq : elt Seq.t -> t -> t

        val of_seq : elt Seq.t -> t

        val of_list : elt list -> t

        val to_list : t -> elt list

        val hash : t -> int

        val to_int : t -> int

        val equal : t -> t -> bool

        val compare : t -> t -> int

        val get_symbols : t -> Symbol.t list
      end
    end )

let unop = raw_unop

let binop = raw_binop

let triop = raw_triop

let relop = raw_relop

let cvtop = raw_cvtop

let naryop = raw_naryop

let extract t ~high ~low = raw_extract t ~high ~low

let concat = raw_concat

let simplify = Fun.id

module Bool = struct
  let true_ = Bool.true_

  let false_ = Bool.false_

  let v = Bool.v

  let not a = raw_unop Ty_bool Not a

  let equal a b = raw_relop Ty_bool Eq a b

  let distinct a b = raw_relop Ty_bool Ne a b

  let and_ a b = raw_binop Ty_bool And a b

  let or_ a b = raw_binop Ty_bool Or a b

  let ite a b c = raw_triop Ty_bool Ite a b c
end

(** Abstract Syntax Tree (AST) for Expressions. This module defines the
    representation of terms and expressions in the AST, along with constructors,
    accessors, simplification utilities, and pretty-printing functions. It also
    includes submodules for handling Boolean expressions, sets, bitvectors, and
    floating-point arithmetic. *)

(** {1 Expression Types} *)

(** A term in the abstract syntax tree. *)
type t = expr Hc.hash_consed

(** The different types of expressions. *)
and expr = private
  | Val of Value.t  (** A constant value. *)
  | Ptr of
      { base : Bitvector.t  (** Base address. *)
      ; offset : t  (** Offset from base. *)
      }
  | Loc of Loc.t  (** Abstract location *)
  | Symbol of Symbol.t  (** A symbolic variable. *)
  | List of t list  (** A list of expressions. *)
  | App of Symbol.t * t list  (** Function application. *)
  | Unop of Ty.t * Ty.Unop.t * t  (** Unary operation. *)
  | Binop of Ty.t * Ty.Binop.t * t * t  (** Binary operation. *)
  | Triop of Ty.t * Ty.Triop.t * t * t * t  (** Ternary operation. *)
  | Relop of Ty.t * Ty.Relop.t * t * t  (** Relational operation. *)
  | Cvtop of Ty.t * Ty.Cvtop.t * t  (** Conversion operation. *)
  | Naryop of Ty.t * Ty.Naryop.t * t list  (** N-ary operation. *)
  | Extract of t * int * int  (** Extract a bit range from an expression. *)
  | Concat of t * t  (** Concatenate two expressions. *)
  | Binder of Binder.t * t list * t  (** A binding expression. *)

(** {1 Constructors and Accessors} *)

(** [view term] extracts the underlying expression from a term. *)
val view : t -> expr

(** [hash term] computes the hash of a term. *)
val hash : t -> int

(** [equal t1 t2] compares two terms for equality. *)
val equal : t -> t -> bool

(** [compare t1 t2] compares two terms lexicographically. *)
val compare : t -> t -> int

(** {1 Type and Symbol Handling} *)

(** [ty expr] determines the type of an expression. *)
val ty : t -> Ty.t

(** [is_symbolic expr] checks if an expression is symbolic (i.e., contains
    symbolic variables). *)
val is_symbolic : t -> bool

(** [get_symbols exprs] extracts all symbolic variables from a list of
    expressions. *)
val get_symbols : t list -> Symbol.t list

(** [negate_relop expr] negates a relational operation, if applicable. Returns
    an error if the expression is not a relational operation. *)
val negate_relop : t -> t

(** {1 Pretty Printing} *)

(** [pp fmt term] prints a term in a human-readable format using the formatter
    [fmt]. *)
val pp : t Fmt.t

(** [pp_smt fmt terms] prints a list of terms in SMT-LIB format using the
    formatter [fmt]. *)
val pp_smt : t list Fmt.t

(** [pp_list fmt terms] prints a list of expressions in a human-readable format
    using the formatter [fmt]. *)
val pp_list : t list Fmt.t

(** [to_string term] converts a term to a string representation. *)
val to_string : t -> string

(** {1 Expression Constructors} *)

(** [value v] constructs a value expression from the given value. *)
val value : Value.t -> t

(** [ptr base offset] constructs a pointer expression with the given base
    address and offset. *)
val ptr : int32 -> t -> t

(** [loc l] constructs an abstract location *)
val loc : Loc.t -> t

(** [list l] constructs a list expression with the given list of expressions *)
val list : t list -> t

(** [symbol sym] constructs a symbolic variable expression from the given
    symbol. *)
val symbol : Symbol.t -> t

(** [app sym args] constructs a function application expression with the given
    symbol and arguments. *)
val app : Symbol.t -> t list -> t

(** [binder ty bindings body] constructs a [ty] bidning expression with the
    given bindings and body. *)
val binder : Binder.t -> t list -> t -> t

(** [let_in bindings body] constructs a let-binding expression with the given
    bindings and body. *)
val let_in : t list -> t -> t

(** [forall bindings body] constructs a universal quantification expression with
    the given bindings and body. *)
val forall : t list -> t -> t

(** [exists bindings body] constructs an existential quantification expression
    with the given bindings and body. *)
val exists : t list -> t -> t

(** {1 Smart Constructors for Operations} *)

(** These constructors do NOT apply simplifications during construction. *)

(** [unop ty op expr] applies a raw unary. *)
val unop : Ty.t -> Ty.Unop.t -> t -> t

(** [binop ty op expr1 expr2] applies a raw binary. *)
val binop : Ty.t -> Ty.Binop.t -> t -> t -> t

(** [triop ty op expr1 expr2 expr3] applies a raw ternary operation. *)
val triop : Ty.t -> Ty.Triop.t -> t -> t -> t -> t

(** [relop ty op expr1 expr2] applies a raw relational operation. *)
val relop : Ty.t -> Ty.Relop.t -> t -> t -> t

(** [cvtop ty op expr] applies a raw conversion. *)
val cvtop : Ty.t -> Ty.Cvtop.t -> t -> t

(** [naryop ty op exprs] applies a raw N-ary operation. *)
val naryop : Ty.t -> Ty.Naryop.t -> t list -> t

(** [extract expr ~high ~low] extracts a bit range. *)
val extract : t -> high:int -> low:int -> t

(** [concat expr1 expr2] concatenates two expressions. *)
val concat : t -> t -> t

(** {1 Expression Simplification} *)

(** [simplify expr] The identity. Does nothing. *)
val simplify : t -> t

(** {1 Hash-Consing Module} *)

module Hc : sig
  (** [clear ()] clears the hash-consing table. *)
  val clear : unit -> unit

  (** [stats ()] returns statistics about the hash-consing table. *)
  val stats : unit -> Hashtbl.statistics

  (** [length ()] returns the number of entries in the hash-consing table. *)
  val length : unit -> int
end

(** {1 Boolean Expressions} *)

module Bool : sig
  (** The constant [true] expression. *)
  val true_ : t

  (** The constant [false] expression. *)
  val false_ : t

  (** [v b] constructs a Boolean expression from a boolean value. *)
  val v : bool -> t

  (** [not expr] constructs the logical negation of an expression. *)
  val not : t -> t

  (** [equal expr1 expr2] constructs an equality expression. *)
  val equal : t -> t -> t

  (** [distinct expr1 expr2] constructs a distinctness expression. *)
  val distinct : t -> t -> t

  (** [and_ expr1 expr2] constructs a logical AND expression. *)
  val and_ : t -> t -> t

  (** [or_ expr1 expr2] constructs a logical OR expression. *)
  val or_ : t -> t -> t

  (** [ite cond then_ else_] constructs an if-then-else expression. *)
  val ite : t -> t -> t -> t
end

(** {1 Set Module} *)

module Set : sig
  (** The type of elements of the set *)
  type elt = t

  (** Alias for the type of elements, for cross-compatibility with maps *)
  type key = elt

  (** The set type *)
  type t

  (** {1 Basic functions} *)

  (** The empty set *)
  val empty : t

  (** [is_empty st] is [true] if [st] contains no elements, [false] otherwise *)
  val is_empty : t -> bool

  (** [mem elt set] is [true] if [elt] is contained in [set], O(log(n))
      complexity. *)
  val mem : elt -> t -> bool

  (** [add elt set] adds element [elt] to the [set]. Preserves physical equality
      if [elt] was already present. O(log(n)) complexity. *)
  val add : elt -> t -> t

  (** [singleton elt] returns a set containing a single element: [elt] *)
  val singleton : elt -> t

  (** [cardinal set] is the size of the set (number of elements), O(n)
      complexity. *)
  val cardinal : t -> int

  (** [is_singleton set] is [Some (Any elt)] if [set] is [singleton elt] and
      [None] otherwise. *)
  val is_singleton : t -> elt option

  (** [remove elt set] returns a set containing all elements of [set] except
      [elt]. Returns a value physically equal to [set] if [elt] is not present.
  *)
  val remove : elt -> t -> t

  (** The minimal element (according to the unsigned order on {!to_int}) if non
      empty. raises Not_found *)
  val unsigned_min_elt : t -> elt

  (** The maximal element (according to the unsigned order on {!to_int}) if non
      empty. raises Not_found *)
  val unsigned_max_elt : t -> elt

  (** [pop_unsigned_minimum s] is [Some (elt, s')] where
      [elt = unsigned_min_elt s] and [s' = remove elt s] if [s] is non empty.
      Uses the unsigned order on {!to_int}. *)
  val pop_unsigned_minimum : t -> (elt * t) option

  (** [pop_unsigned_maximum s] is [Some (elt, s')] where
      [elt = unsigned_max_elt s] and [s' = remove elt s] if [s] is non empty.
      Uses the unsigned order on {!to_int}. *)
  val pop_unsigned_maximum : t -> (elt * t) option

  (** {1 Iterators} *)

  (** [iter f set] calls [f] on all elements of [set], in the unsigned order of
      {!to_int}. *)
  val iter : (elt -> unit) -> t -> unit

  (** [filter f set] is the subset of [set] that only contains the elements that
      satisfy [f]. [f] is called in the unsigned order of {!to_int}. *)
  val filter : (elt -> bool) -> t -> t

  (** [for_all f set] is [true] if [f] is [true] on all elements of [set].
      Short-circuits on first [false]. [f] is called in the unsigned order of
      {!to_int}. *)
  val for_all : (elt -> bool) -> t -> bool

  (** [fold f set acc] returns [f elt_n (... (f elt_1 acc) ...)], where
      [elt_1, ..., elt_n] are the elements of [set], in increasing unsigned
      order of {!to_int} *)
  val fold : (elt -> 'acc -> 'acc) -> t -> 'acc -> 'acc

  (** [split elt set] returns [s_lt, present, s_gt] where [s_lt] contains all
      elements of [set] smaller than [elt], [s_gt] all those greater than [elt],
      and [present] is [true] if [elt] is in [set]. Uses the unsigned order on
      {!to_int}.*)
  val split : elt -> t -> t * bool * t

  (** Pretty prints the set. *)
  val pp : Format.formatter -> t -> unit

  (** {1 Functions on pairs of sets} *)

  (** [union a b] is the set union of [a] and [b], i.e. the set containing all
      elements that are either in [a] or [b]. *)
  val union : t -> t -> t

  (** [inter a b] is the set intersection of [a] and [b], i.e. the set
      containing all elements that are in both [a] or [b]. *)
  val inter : t -> t -> t

  (** [disjoint a b] is [true] if [a] and [b] have no elements in common. *)
  val disjoint : t -> t -> bool

  (** [subset a b] is [true] if all elements of [a] are also in [b]. *)
  val subset : t -> t -> bool

  (** [diff s1 s2] is the set of all elements of [s1] that aren't in [s2].
      @since v0.11.0 *)
  val diff : t -> t -> t

  (** [min_elt_inter s1 s2] is {!unsigned_min_elt} of {{!inter}[inter s1 s2]},
      but faster as it does not require computing the whole intersection.
      Returns [None] when the intersection is empty.

      @since v0.11.0 *)
  val min_elt_inter : t -> t -> elt option

  (** [max_elt_inter s1 s2] is {!unsigned_max_elt} of {{!inter}[inter s1 s2]},
      but faster as it does not require computing the whole intersection.
      Returns [None] when the intersection is empty.

      @since v0.11.0 *)
  val max_elt_inter : t -> t -> elt option

  (** {1 Conversion functions} *)

  (** [to_seq st] iterates the whole set, in increasing unsigned order of
      {!to_int} *)
  val to_seq : t -> elt Seq.t

  (** [to_rev_seq st] iterates the whole set, in decreasing unsigned order of
      {!to_int} *)
  val to_rev_seq : t -> elt Seq.t

  (** [add_seq s st] adds all elements of the sequence [s] to [st] in order. *)
  val add_seq : elt Seq.t -> t -> t

  (** [of_seq s] creates a new set from the elements of [s]. *)
  val of_seq : elt Seq.t -> t

  (** [of_list l] creates a new set from the elements of [l]. *)
  val of_list : elt list -> t

  (** [to_list s] returns the elements of [s] as a list, in increasing unsigned
      order of {!to_int} *)
  val to_list : t -> elt list

  (** {1 Smtml Specific} *)

  (** [hash set] computes the hash of a set. *)
  val hash : t -> int

  (** [to_int set] converts a set to an integer. *)
  val to_int : t -> int

  (** [equal set1 set2] compares two sets for equality. *)
  val equal : t -> t -> bool

  (** [compare set1 set2] compares two sets lexicographically. *)
  val compare : t -> t -> int

  (** [get_symbols exprs] extracts all symbolic variables from a list of
      expressions. *)
  val get_symbols : t -> Symbol.t list
end

(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2025 formalsec *)
(* Written by the Smtml programmers *)

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
      { base : int32  (** Base address. *)
      ; offset : t  (** Offset from base. *)
      }
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
val negate_relop : t -> (t, string) Result.t

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

(** These constructors apply simplifications during construction. *)

(** [unop ty op expr] applies a unary operation with simplification. *)
val unop : Ty.t -> Ty.Unop.t -> t -> t

(** [binop ty op expr1 expr2] applies a binary operation with simplification. *)
val binop : Ty.t -> Ty.Binop.t -> t -> t -> t

(** [triop ty op expr1 expr2 expr3] applies a ternary operation with
    simplification. *)
val triop : Ty.t -> Ty.Triop.t -> t -> t -> t -> t

(** [relop ty op expr1 expr2] applies a relational operation with
    simplification. *)
val relop : Ty.t -> Ty.Relop.t -> t -> t -> t

(** [cvtop ty op expr] applies a conversion operation with simplification. *)
val cvtop : Ty.t -> Ty.Cvtop.t -> t -> t

(** [naryop ty op exprs] applies an N-ary operation with simplification. *)
val naryop : Ty.t -> Ty.Naryop.t -> t list -> t

(** [extract expr ~high ~low] extracts a bit range with simplification. *)
val extract : t -> high:int -> low:int -> t

(** [extract2 expr pos] extracts a bit range with simplification. *)
val extract2 : t -> int -> t

(** [concat expr1 expr2] concatenates two expressions with simplification. *)
val concat : t -> t -> t

(** [concat3 ~msb ~lsb size] concatenates two expressions with simplification,
    specifying the size of the result. *)
val concat3 : msb:t -> lsb:t -> int -> t

(** {1 Raw Constructors for Operations} *)

(** [raw_unop ty op expr] applies a unary operation, creating a node without
    immediate simplification.

    This function constructs the representation of a unary operation with the
    specified type [ty], operator [op], and operand [expr]. Unlike a "smart
    constructor" like [unop], it does not evaluate the expression if possible,
    but rather creates the AST node representing the unevaluated operation.

    For example:
    {@ocaml[
      raw_unop Ty_int Neg (value (Int 1))
    ]}

    returns the AST node:
    {@ocaml[
      Unop (Ty_int, Neg, Val (Int 1))
    ]}

    rather than the simplified value:
    {@ocaml[
      Val (Int (-1))
    ]}

    which would typically be the result of the smart constructor [unop]. *)
val raw_unop : Ty.t -> Ty.Unop.t -> t -> t

(** [raw_binop ty op expr1 expr2] applies a binary operation, creating a node
    without immediate simplification.

    This function constructs the representation of a binary operation with the
    specified type [ty], operator [op], and operands [expr1], [expr2]. Unlike a
    "smart constructor" like [binop], it does not evaluate the expression if
    possible, but rather creates the AST node representing the unevaluated
    operation.

    For example:
    {@ocaml[
      raw_binop Ty_int Add (value (Int 1)) (value (Int 2))
    ]}

    returns the AST node:
    {@ocaml[
      Binop (Ty_int, Add, Val (Int 1), Val (Int 2))
    ]}

    rather than the simplified value:
    {@ocaml[
      Val (Int 3)
    ]}

    which would typically be the result of the smart constructor [binop]. *)
val raw_binop : Ty.t -> Ty.Binop.t -> t -> t -> t

(** [raw_triop ty op expr1 expr2 expr3] applies a ternary operation, creating a
    node without immediate simplification.

    This function constructs the representation of a ternary operation with the
    specified type [ty], operator [op], and operands [expr1], [expr2], [expr3].
    Unlike a "smart constructor" like [triop], it does not evaluate the
    expression if possible, but rather creates the AST node representing the
    unevaluated operation.

    For example (using a if-then-else operator):
    {@ocaml[
      raw_triop Ty_bool Ite (value True) (value (Int 1)) (value (Int 2))
    ]}

    returns the AST node:
    {@ocaml[
      Triop (Ty_bool, Ite, Val True, Val (Int 1), Val (Int 2))
    ]}

    rather than the simplified value:
    {@ocaml[
      Val (Int 1)
    ]}

    which would typically be the result of the smart constructor [triop]. *)
val raw_triop : Ty.t -> Ty.Triop.t -> t -> t -> t -> t

(** [raw_relop ty op expr1 expr2] applies a relational operation, creating a
    node without immediate simplification.

    This function constructs the representation of a relational operation with
    the specified operand type [ty], operator [op], and operands [expr1],
    [expr2]. Unlike a "smart constructor" like [relop], it does not evaluate the
    expression if possible, but rather creates the AST node representing the
    unevaluated operation (which will have a boolean type).

    For example:
    {@ocaml[
      raw_relop Ty_bool Eq (value (Int 1)) (value (Int 2))
    ]}

    returns the AST node:
    {@ocaml[
      Relop (Ty_bool, Eq, Val (Int 1), Val (Int 2))
    ]}

    rather than the simplified value:
    {@ocaml[
      Val False
    ]}

    which would typically be the result of the smart constructor [relop]. *)
val raw_relop : Ty.t -> Ty.Relop.t -> t -> t -> t

(** [raw_cvtop ty op expr] applies a conversion operation, creating a node
    without immediate simplification.

    This function constructs the representation of a conversion operation with
    the specified target type [ty], operator [op], and operand [expr]. Unlike a
    "smart constructor" like [cvtop], it does not evaluate the conversion if
    possible, but rather creates the AST node representing the unevaluated
    operation.

    For example:
    {@ocaml[
      raw_cvtop Ty_real Reinterpret_int (value (Int 5))
    ]}

    returns the AST node:
    {@ocaml[
      Cvtop (Ty_real, Reinterpret_int, Val (Int 5))
    ]}

    rather than the simplified value:
    {@ocaml[
      Val (Real 5.0)
    ]}

    which would typically be the result of the smart constructor [cvtop]. *)
val raw_cvtop : Ty.t -> Ty.Cvtop.t -> t -> t

(** [raw_naryop ty op exprs] applies an N-ary operation without simplification.
*)
val raw_naryop : Ty.t -> Ty.Naryop.t -> t list -> t

(** [raw_extract expr ~high ~low] extracts a bit range without simplification.
*)
val raw_extract : t -> high:int -> low:int -> t

(** [raw_concat expr1 expr2] concatenates two expressions without
    simplification. *)
val raw_concat : t -> t -> t

(** {1 Expression Simplification} *)

(** [simplify expr] simplifies an expression until a fixpoint is reached. *)
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

  (** Pretty prints the set, [pp_sep] is called once between each element, it
      defaults to
      {{:https://v2.ocaml.org/api/Format.html#VALpp_print_cut}[Format.pp_print_cut]}
  *)
  val pretty :
       ?pp_sep:(Format.formatter -> unit -> unit)
    -> (Format.formatter -> elt -> unit)
    -> Format.formatter
    -> t
    -> unit

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

(** {1 Bitvectors} *)

module Bitv : sig
  (** Bitvector operations for 8-bit integers. *)
  module I8 : Constructors_intf.Infix with type elt := int and type t := t

  (** Bitvector operations for 32-bit integers. *)
  module I32 : Constructors_intf.Infix with type elt := int32 and type t := t

  (** Bitvector operations for 64-bit integers. *)
  module I64 : Constructors_intf.Infix with type elt := int64 and type t := t
end

(** {1 Floating-Points} *)

module Fpa : sig
  (** Floating-point operations for 32-bit floats. *)
  module F32 : Constructors_intf.Infix with type elt := float and type t := t

  (** Floating-point operations for 64-bit floats. *)
  module F64 : Constructors_intf.Infix with type elt := float and type t := t
end

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
and expr =
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

(** [make expr] creates a new term from the given expression. *)
val make : expr -> t

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

(** [symbol sym] constructs a symbolic variable expression from the given
    symbol. *)
val symbol : Symbol.t -> t

(** [app sym args] constructs a function application expression with the given
    symbol and arguments. *)
val app : Symbol.t -> t list -> t

(** [let_in bindings body] constructs a let-binding expression with the given
    bindings and body. *)
val let_in : t list -> t -> t

(** [forall bindings body] constructs a universal quantification expression
    with the given bindings and body. *)
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

(** {1 Dumb Constructors for Operations} *)

(** These constructors do not apply simplifications. *)

(** [unop' ty op expr] applies a unary operation without simplification. *)
val unop' : Ty.t -> Ty.Unop.t -> t -> t

(** [binop' ty op expr1 expr2] applies a binary operation without
    simplification. *)
val binop' : Ty.t -> Ty.Binop.t -> t -> t -> t

(** [triop' ty op expr1 expr2 expr3] applies a ternary operation without
    simplification. *)
val triop' : Ty.t -> Ty.Triop.t -> t -> t -> t -> t

(** [relop' ty op expr1 expr2] applies a relational operation without
    simplification. *)
val relop' : Ty.t -> Ty.Relop.t -> t -> t -> t

(** [cvtop' ty op expr] applies a conversion operation without simplification. *)
val cvtop' : Ty.t -> Ty.Cvtop.t -> t -> t

(** [naryop' ty op exprs] applies an N-ary operation without simplification. *)
val naryop' : Ty.t -> Ty.Naryop.t -> t list -> t

(** [extract' expr ~high ~low] extracts a bit range without simplification. *)
val extract' : t -> high:int -> low:int -> t

(** [concat' expr1 expr2] concatenates two expressions without simplification. *)
val concat' : t -> t -> t

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
  include PatriciaTree.SET with type elt = t

  (** [hash set] computes the hash of a set. *)
  val hash : t -> int

  (** [to_int set] converts a set to an integer. *)
  val to_int : t -> int

  (** [equal set1 set2] compares two sets for equality. *)
  val equal : t -> t -> bool

  (** [compare set1 set2] compares two sets lexicographically. *)
  val compare : t -> t -> int
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

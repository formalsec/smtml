(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2024 formalsec *)
(* Written by the Smtml programmers *)

(** Term definitions of the abstract syntax *)
type t = expr Hc.hash_consed

and expr =
  | Val of Value.t
  | Ptr of
      { base : int32
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

val make : expr -> t

val view : t -> expr

val hash : t -> int

val equal : t -> t -> bool

val compare : t -> t -> int

(** The return type of an expression *)
val ty : t -> Ty.t

val is_symbolic : t -> bool

val get_symbols : t list -> Symbol.t list

val negate_relop : t -> (t, string) Result.t

val pp : t Fmt.t

val pp_smt : t list Fmt.t

val pp_list : t list Fmt.t

val to_string : t -> string

val value : Value.t -> t

val ptr : int32 -> t -> t

val symbol : Symbol.t -> t

val app : Symbol.t -> t list -> t

val let_in : t list -> t -> t

val forall : t list -> t -> t

val exists : t list -> t -> t

(** Smart unop constructor, applies simplifications at constructor level *)
val unop : Ty.t -> Ty.Unop.t -> t -> t

(** Dumb unop constructor, no simplifications *)
val unop' : Ty.t -> Ty.Unop.t -> t -> t

(** Smart binop constructor, applies simplifications at constructor level *)
val binop : Ty.t -> Ty.Binop.t -> t -> t -> t

(** Dumb binop constructor, no simplifications *)
val binop' : Ty.t -> Ty.Binop.t -> t -> t -> t

(** Smart triop constructor, applies simplifications at constructor level *)
val triop : Ty.t -> Ty.Triop.t -> t -> t -> t -> t

(** Dumb triop constructor, no simplifications *)
val triop' : Ty.t -> Ty.Triop.t -> t -> t -> t -> t

(** Smart relop constructor, applies simplifications at constructor level *)
val relop : Ty.t -> Ty.Relop.t -> t -> t -> t

(** Dumb relop constructor, no simplifications *)
val relop' : Ty.t -> Ty.Relop.t -> t -> t -> t

(** Smart relop constructor, applies simplifications at constructor level *)
val cvtop : Ty.t -> Ty.Cvtop.t -> t -> t

(** Dumb cvtop constructor, no simplifications *)
val cvtop' : Ty.t -> Ty.Cvtop.t -> t -> t

(** Smart naryop constructor, applies simplifications at constructor level *)
val naryop : Ty.t -> Ty.Naryop.t -> t list -> t

(** Dumb naryop constructor, no simplifications *)
val naryop' : Ty.t -> Ty.Naryop.t -> t list -> t

(** Smart extract constructor, applies simplifications at constructor level *)
val extract : t -> high:int -> low:int -> t

(** Dumb extract constructor, no simplifications *)
val extract' : t -> high:int -> low:int -> t

val extract2 : t -> int -> t

(** Smart concat constructor, applies simplifications at constructor level *)
val concat : t -> t -> t

(** Dumb concat constructor, no simplifications *)
val concat' : t -> t -> t

val concat3 : msb:t -> lsb:t -> int -> t

(** Applies expression simplifications until a fixpoint *)
val simplify : t -> t

module Hc : sig
  val clear : unit -> unit

  val stats : unit -> Hashtbl.statistics

  val length : unit -> int
end

module Bool : sig
  val true_ : t

  val false_ : t

  val v : bool -> t

  val not : t -> t

  val equal : t -> t -> t

  val distinct : t -> t -> t

  val and_ : t -> t -> t

  val or_ : t -> t -> t

  val ite : t -> t -> t -> t
end

module Set : sig
  include PatriciaTree.SET with type elt = t

  val hash : t -> int

  val to_int : t -> int

  val equal : t -> t -> bool

  val compare : t -> t -> int
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

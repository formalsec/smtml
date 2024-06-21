(***************************************************************************)
(* This file is part of the third-party OCaml library `smtml`.             *)
(* Copyright (C) 2023-2024 formalsec                                       *)
(*                                                                         *)
(* This program is free software: you can redistribute it and/or modify    *)
(* it under the terms of the GNU General Public License as published by    *)
(* the Free Software Foundation, either version 3 of the License, or       *)
(* (at your option) any later version.                                     *)
(*                                                                         *)
(* This program is distributed in the hope that it will be useful,         *)
(* but WITHOUT ANY WARRANTY; without even the implied warranty of          *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the           *)
(* GNU General Public License for more details.                            *)
(*                                                                         *)
(* You should have received a copy of the GNU General Public License       *)
(* along with this program.  If not, see <https://www.gnu.org/licenses/>.  *)
(***************************************************************************)

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
  | App : [> `Op of string ] * t list -> expr
  | Unop of Ty.t * Ty.unop * t
  | Binop of Ty.t * Ty.binop * t * t
  | Triop of Ty.t * Ty.triop * t * t * t
  | Relop of Ty.t * Ty.relop * t * t
  | Cvtop of Ty.t * Ty.cvtop * t
  | Naryop of Ty.t * Ty.naryop * t list
  | Extract of t * int * int
  | Concat of t * t

val equal : t -> t -> bool

val hash : t -> int

val make : expr -> t

val ( @: ) : expr -> Ty.t -> t [@@deprecated "Please use 'make' instead"]

val view : t -> expr

val ty : t -> Ty.t

val mk_symbol : Symbol.t -> t

val is_symbolic : t -> bool

val get_symbols : t list -> Symbol.t list

val negate_relop : t -> (t, string) Result.t

val pp : Format.formatter -> t -> unit

val pp_smt : Format.formatter -> t list -> unit

val pp_list : Format.formatter -> t list -> unit

val to_string : t -> string

val value : Value.t -> t

val ptr : int32 -> t -> t

val symbol : Symbol.t -> t

(** Smart unop constructor, applies simplifications at constructor level *)
val unop : Ty.t -> Ty.unop -> t -> t

(** Dumb unop constructor, no simplifications *)
val unop' : Ty.t -> Ty.unop -> t -> t

(** Smart binop constructor, applies simplifications at constructor level *)
val binop : Ty.t -> Ty.binop -> t -> t -> t

(** Dumb binop constructor, no simplifications *)
val binop' : Ty.t -> Ty.binop -> t -> t -> t

(** Smart triop constructor, applies simplifications at constructor level *)
val triop : Ty.t -> Ty.triop -> t -> t -> t -> t

(** Dumb triop constructor, no simplifications *)
val triop' : Ty.t -> Ty.triop -> t -> t -> t -> t

(** Smart relop constructor, applies simplifications at constructor level *)
val relop : Ty.t -> Ty.relop -> t -> t -> t

(** Dumb relop constructor, no simplifications *)
val relop' : Ty.t -> Ty.relop -> t -> t -> t

(** Smart relop constructor, applies simplifications at constructor level *)
val cvtop : Ty.t -> Ty.cvtop -> t -> t

(** Dumb cvtop constructor, no simplifications *)
val cvtop' : Ty.t -> Ty.cvtop -> t -> t

(** Smart naryop constructor, applies simplifications at constructor level *)
val naryop : Ty.t -> Ty.naryop -> t list -> t

(** Dumb naryop constructor, no simplifications *)
val naryop' : Ty.t -> Ty.naryop -> t list -> t

(** Smart extract constructor, applies simplifications at constructor level *)
val extract : t -> high:int -> low:int -> t

(** Dumb extract constructor, no simplifications *)
val extract' : t -> high:int -> low:int -> t

(** Smart concat constructor, applies simplifications at constructor level *)
val concat : t -> t -> t

(** Dumb concat constructor, no simplifications *)
val concat' : t -> t -> t

(** Applies expression simplifications until a fixpoint *)
val simplify : t -> t

module Hc : sig
  val clear : unit -> unit

  val stats : unit -> Hashtbl.statistics

  val length : unit -> int
end

module Bool : sig
  val v : bool -> t

  val not : t -> t

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

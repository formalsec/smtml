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

type satisfiability =
  [ `Sat
  | `Unsat
  | `Unknown
  ]

module type M = sig
  type ty

  type term

  type interp

  type model

  type solver

  type handle

  type optimizer

  type func_decl

  val caches_consts : bool

  val true_ : term

  val false_ : term

  val int : int -> term

  val real : float -> term

  val const : string -> ty -> term

  val not_ : term -> term

  val and_ : term -> term -> term

  val or_ : term -> term -> term

  val logand : term list -> term

  val logor : term list -> term

  val xor : term -> term -> term

  val eq : term -> term -> term

  val distinct : term list -> term

  val ite : term -> term -> term -> term

  module Types : sig
    val int : ty

    val real : ty

    val bool : ty

    val string : ty

    val bitv : int -> ty

    val float : int -> int -> ty

    val ty : term -> ty

    val to_ety : ty -> Ty.t
  end

  module Interp : sig
    val to_int : interp -> int

    val to_real : interp -> float

    val to_bool : interp -> bool

    val to_string : interp -> string

    val to_bitv : interp -> int -> int64

    val to_float : interp -> int -> int -> float
  end

  module Int : sig
    val neg : term -> term

    val to_real : term -> term

    val add : term -> term -> term

    val sub : term -> term -> term

    val mul : term -> term -> term

    val div : term -> term -> term

    val rem : term -> term -> term

    val pow : term -> term -> term

    val lt : term -> term -> term

    val le : term -> term -> term

    val gt : term -> term -> term

    val ge : term -> term -> term
  end

  module Real : sig
    val neg : term -> term

    val to_int : term -> term

    val add : term -> term -> term

    val sub : term -> term -> term

    val mul : term -> term -> term

    val div : term -> term -> term

    val pow : term -> term -> term

    val lt : term -> term -> term

    val le : term -> term -> term

    val gt : term -> term -> term

    val ge : term -> term -> term
  end

  module String : sig
    val v : string -> term

    val length : term -> term

    val to_code : term -> term

    val of_code : term -> term

    val to_int : term -> term

    val of_int : term -> term

    val to_re : term -> term

    val at : term -> pos:term -> term

    val concat : term list -> term

    val contains : term -> sub:term -> term

    val is_prefix : term -> prefix:term -> term

    val is_suffix : term -> suffix:term -> term

    val in_re : term -> term -> term

    val lt : term -> term -> term

    val le : term -> term -> term

    val sub : term -> pos:term -> len:term -> term

    val index_of : term -> sub:term -> pos:term -> term

    val replace : term -> pattern:term -> with_:term -> term
  end

  module Re : sig
    val star : term -> term

    val plus : term -> term

    val opt : term -> term

    val comp : term -> term

    val range : term -> term -> term

    val loop : term -> int -> int -> term

    val union : term list -> term

    val concat : term list -> term
  end

  module Bitv : sig
    val v : string -> int -> term

    val neg : term -> term

    val lognot : term -> term

    val add : term -> term -> term

    val sub : term -> term -> term

    val mul : term -> term -> term

    val div : term -> term -> term

    val div_u : term -> term -> term

    val logor : term -> term -> term

    val logand : term -> term -> term

    val logxor : term -> term -> term

    val shl : term -> term -> term

    val ashr : term -> term -> term

    val lshr : term -> term -> term

    val rem : term -> term -> term

    val rem_u : term -> term -> term

    val rotate_left : term -> term -> term

    val rotate_right : term -> term -> term

    val lt : term -> term -> term

    val lt_u : term -> term -> term

    val le : term -> term -> term

    val le_u : term -> term -> term

    val gt : term -> term -> term

    val gt_u : term -> term -> term

    val ge : term -> term -> term

    val ge_u : term -> term -> term

    val concat : term -> term -> term

    val extract : term -> high:int -> low:int -> term

    val zero_extend : int -> term -> term

    val sign_extend : int -> term -> term
  end

  module Float : sig
    (* Rounding modes *)
    module Rounding_mode : sig
      (* Round nearest ties to even *)
      val rne : term

      (* Round nearest ties to away *)
      val rna : term

      (* Round toward positive *)
      val rtp : term

      (* Round toward negative *)
      val rtn : term

      (* Round toward zero *)
      val rtz : term
    end

    val v : float -> int -> int -> term

    val neg : term -> term

    val abs : term -> term

    (* [sqrt ~rm t] where [rm] is the rounding mode *)
    val sqrt : rm:term -> term -> term

    val is_nan : term -> term

    (* [round_to_integral ~rm t] where [rm] is the rounding mode *)
    val round_to_integral : rm:term -> term -> term

    (* [add ~rm t1 t2] where [rm] is the rounding mode *)
    val add : rm:term -> term -> term -> term

    (* [sub ~rm t1 t2] where [rm] is the rounding mode *)
    val sub : rm:term -> term -> term -> term

    (* [mul ~rm t1 t2] where [rm] is the rounding mode *)
    val mul : rm:term -> term -> term -> term

    (* [div ~rm t1 t2] where [rm] is the rounding mode *)
    val div : rm:term -> term -> term -> term

    val min : term -> term -> term

    val max : term -> term -> term

    val rem : term -> term -> term

    val eq : term -> term -> term

    val lt : term -> term -> term

    val le : term -> term -> term

    val gt : term -> term -> term

    val ge : term -> term -> term

    val to_fp : int -> int -> rm:term -> term -> term

    val sbv_to_fp : int -> int -> rm:term -> term -> term

    val ubv_to_fp : int -> int -> rm:term -> term -> term

    val to_ubv : int -> rm:term -> term -> term

    val to_sbv : int -> rm:term -> term -> term

    val of_ieee_bv : int -> int -> term -> term

    val to_ieee_bv : term -> term
  end

  module Func : sig
    val make : string -> ty list -> ty -> func_decl

    val apply : func_decl -> term list -> term
  end

  module Model : sig
    val get_symbols : model -> Symbol.t list

    val eval : ?completion:bool -> model -> term -> interp option
  end

  module Solver : sig
    val make : ?params:Params.t -> ?logic:Ty.logic -> unit -> solver

    val clone : solver -> solver

    val push : solver -> unit

    val pop : solver -> int -> unit

    val reset : solver -> unit

    val add : solver -> term list -> unit

    val check : solver -> assumptions:term list -> satisfiability

    val model : solver -> model option

    val add_simplifier : solver -> solver

    val interrupt : unit -> unit

    val get_statistics : solver -> Statistics.t

    val pp_statistics : solver Fmt.t
  end

  module Optimizer : sig
    val make : unit -> optimizer

    val push : optimizer -> unit

    val pop : optimizer -> unit

    val add : optimizer -> term list -> unit

    val check : optimizer -> satisfiability

    val model : optimizer -> model option

    val maximize : optimizer -> term -> handle

    val minimize : optimizer -> term -> handle

    val interrupt : unit -> unit

    val get_statistics : optimizer -> Statistics.t

    val pp_statistics : optimizer Fmt.t
  end

  module Smtlib : sig
    val pp :
         ?name:string
      -> ?logic:Ty.logic
      -> ?status:satisfiability
      -> term list Fmt.t
  end
end

module type M_with_make = sig
  module Make () : M

  val is_available : bool

  include M
end

module type S = sig
  type model

  type solver

  type optimize

  type handle

  val value : model -> Expr.t -> Value.t

  val values_of_model : ?symbols:Symbol.t list -> model -> Model.t

  val set_debug : bool -> unit

  module Smtlib : sig
    val pp :
         ?name:string
      -> ?logic:Ty.logic
      -> ?status:satisfiability
      -> Expr.t list Fmt.t
  end

  module Solver : sig
    val make : ?params:Params.t -> ?logic:Ty.logic -> unit -> solver

    val add_simplifier : solver -> solver

    val clone : solver -> solver

    val push : solver -> unit

    val pop : solver -> int -> unit

    val reset : solver -> unit

    val add : solver -> Expr.t list -> unit

    val check : solver -> assumptions:Expr.t list -> satisfiability

    val model : solver -> model option

    val interrupt : solver -> unit

    val get_statistics : solver -> Statistics.t
  end

  module Optimizer : sig
    val make : unit -> optimize

    val push : optimize -> unit

    val pop : optimize -> unit

    val add : optimize -> Expr.t list -> unit

    val check : optimize -> satisfiability

    val model : optimize -> model option

    val maximize : optimize -> Expr.t -> handle

    val minimize : optimize -> Expr.t -> handle

    val interrupt : optimize -> unit

    val get_statistics : optimize -> Statistics.t
  end
end

module type S_with_fresh = sig
  module Fresh : sig
    module Make () : S
  end

  val is_available : bool

  include S
end

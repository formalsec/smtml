(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2024 formalsec *)
(* Written by Hichem Rami Ait El Hara *)

module DExpr = Dolmen_std.Expr
module DTy = DExpr.Ty
module DTerm = DExpr.Term
module DM = Dolmen_model

module Builtin : sig
  val string_ty_cst : DExpr.ty_cst

  val string_ty : DExpr.ty

  val float32_ty : DExpr.ty

  val float64_ty : DExpr.ty

  val int_to_string : DExpr.term_cst

  val string_to_int : DExpr.term_cst

  val real_to_string : DExpr.term_cst

  val string_to_real : DExpr.term_cst

  val real_to_uint32 : DExpr.term_cst

  val trim_string : DExpr.term_cst

  val f32_to_string : DExpr.term_cst

  val string_to_f32 : DExpr.term_cst

  val f64_to_string : DExpr.term_cst

  val string_to_f64 : DExpr.term_cst
end

module DolmenIntf : sig
  module ConstMap : Map.S with type key = DExpr.term_cst

  type ty = DTy.t

  type term = DTerm.t

  type interp = DM.Value.t

  type model = interp ConstMap.t

  type func_decl = DTerm.Const.t

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

  val implies : term -> term -> term

  val eq : term -> term -> term

  val distinct : term list -> term

  val ite : term -> term -> term -> term

  val forall : term list -> term -> term

  val exists : term list -> term -> term

  module Types : sig
    val int : ty

    val real : ty

    val bool : ty

    val string : ty

    val bitv : int -> ty

    val float : int -> int -> ty

    val roundingMode : ty

    val regexp : ty

    val ty : term -> ty

    val to_ety : ty -> Ty.t
  end

  module Interp : sig
    val to_int : interp -> int

    val to_real : interp -> float

    val to_bool : interp -> bool

    val to_string : interp -> string

    val to_bitv : interp -> int -> Z.t

    val to_float : interp -> int -> int -> float
  end

  module Int : sig
    val neg : term -> term

    val to_real : term -> term

    val to_bv : int -> term -> term

    val add : term -> term -> term

    val sub : term -> term -> term

    val mul : term -> term -> term

    val div : term -> term -> term

    val rem : term -> term -> term

    val mod_ : term -> term -> term

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

    val replace_all : term -> pattern:term -> with_:term -> term

    val replace_re : term -> pattern:term -> with_:term -> term

    val replace_re_all : term -> pattern:term -> with_:term -> term
  end

  module Re : sig
    val allchar : unit -> term

    val all : unit -> term

    val none : unit -> term

    val star : term -> term

    val plus : term -> term

    val opt : term -> term

    val comp : term -> term

    val range : term -> term -> term

    val diff : term -> term -> term

    val inter : term -> term -> term

    val loop : term -> int -> int -> term

    val union : term list -> term

    val concat : term list -> term
  end

  module Bitv : sig
    val v : string -> int -> term

    val neg : term -> term

    val lognot : term -> term

    val to_int : signed:bool -> term -> term

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

    val smod : term -> term -> term

    val rem : term -> term -> term

    val rem_u : term -> term -> term

    val rotate_left : term -> term -> term

    val rotate_right : term -> term -> term

    val nego : term -> term

    val addo : signed:bool -> term -> term -> term

    val subo : signed:bool -> term -> term -> term

    val mulo : signed:bool -> term -> term -> term

    val divo : term -> term -> term

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
    module Rounding_mode : sig
      val rne : term

      val rna : term

      val rtp : term

      val rtn : term

      val rtz : term
    end

    val v : float -> int -> int -> term

    val neg : term -> term

    val abs : term -> term

    val sqrt : rm:term -> term -> term

    val is_normal : term -> term

    val is_subnormal : term -> term

    val is_negative : term -> term

    val is_positive : term -> term

    val is_infinite : term -> term

    val is_zero : term -> term

    val is_nan : term -> term

    val round_to_integral : rm:term -> term -> term

    val add : rm:term -> term -> term -> term

    val sub : rm:term -> term -> term -> term

    val mul : rm:term -> term -> term -> term

    val div : rm:term -> term -> term -> term

    val min : term -> term -> term

    val max : term -> term -> term

    val rem : term -> term -> term

    val fma : rm:term -> term -> term -> term -> term

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

    val to_ieee_bv : (term -> term) option
  end

  module Adt : sig
    module Cons : sig
      type t

      val make : string -> fields:(string * ty option) list -> t
    end

    type t = DExpr.ty_def * (func_decl * (ty * func_decl option) list) list

    val make : string -> Cons.t list -> t

    val ty : t -> ty

    val constructor : string -> t -> func_decl option

    val selector : string -> t -> func_decl option

    val tester : string -> t -> func_decl option
  end

  module Func : sig
    val make : string -> ty list -> ty -> func_decl

    val apply : func_decl -> term list -> term
  end

  module Model : sig
    (** [get_symbols model] retrieves the list of symbols in the model. *)
    val get_symbols : model -> Symbol.t list

    (** [eval ?completion model t] evaluates the term [t] in the given [model].
        If [completion] is true, missing values are completed. *)
    val eval :
         ?ctx:term Symbol.Map.t
      -> ?completion:bool
      -> model
      -> term
      -> interp option
  end

  module Smtlib : sig
    val pp :
         ?name:string
      -> ?logic:Logic.t
      -> ?status:[ `Sat | `Unknown | `Unsat ]
      -> term list Fmt.t
  end
end

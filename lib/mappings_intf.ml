type satisfiability =
  | Satisfiable
  | Unsatisfiable
  | Unknown

module type M = sig
  type ty

  type term

  type interp

  type model

  type solver

  type handle

  type optimizer

  type cont

  type 'a t

  val make_cont : unit -> cont

  module Cont : sig
    val return : 'a -> 'a t

    val bind : 'a t -> ('a -> 'b t) -> 'b t

    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t

    val map : 'a t -> ('a -> 'b) -> 'b t

    val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t

    val run : 'a t -> cont -> 'a
  end

  val true_ : term t

  val false_ : term t

  val int : int -> term t

  val real : float -> term t

  val const : string -> ty -> term t

  val not_ : term -> term t

  val and_ : term -> term -> term t

  val or_ : term -> term -> term t

  val xor : term -> term -> term t

  val eq : term -> term -> term t

  val distinct : term list -> term t

  val ite : term -> term -> term -> term t

  module Types : sig
    val int : ty t

    val real : ty t

    val bool : ty t

    val string : ty t

    val bitv : int -> ty t

    val float : int -> int -> ty t

    val ty : term -> ty

    val to_ety : ty -> Ty.t t
  end

  module Interp : sig
    val to_int : interp -> int

    val to_real : interp -> float

    val to_bool : interp -> bool

    val to_string : interp -> string t

    val to_bitv : interp -> int -> int64

    val to_float : interp -> int -> int -> float t
  end

  module Int : sig
    val neg : term -> term t

    val to_real : term -> term t

    val add : term -> term -> term t

    val sub : term -> term -> term t

    val mul : term -> term -> term t

    val div : term -> term -> term t

    val rem : term -> term -> term t

    val pow : term -> term -> term t

    val lt : term -> term -> term t

    val le : term -> term -> term t

    val gt : term -> term -> term t

    val ge : term -> term -> term t
  end

  module Real : sig
    val neg : term -> term t

    val to_int : term -> term t

    val add : term -> term -> term t

    val sub : term -> term -> term t

    val mul : term -> term -> term t

    val div : term -> term -> term t

    val pow : term -> term -> term t

    val lt : term -> term -> term t

    val le : term -> term -> term t

    val gt : term -> term -> term t

    val ge : term -> term -> term t
  end

  module String : sig
    val v : string -> term t

    val length : term -> term t

    val to_code : term -> term t

    val of_code : term -> term t

    val at : term -> pos:term -> term t

    val concat : term -> term -> term t

    val sub : term -> pos:term -> len:term -> term t
  end

  module Bitv : sig
    val v : string -> int -> term t

    val neg : term -> term t

    val lognot : term -> term t

    val add : term -> term -> term t

    val sub : term -> term -> term t

    val mul : term -> term -> term t

    val div : term -> term -> term t

    val div_u : term -> term -> term t

    val logor : term -> term -> term t

    val logand : term -> term -> term t

    val logxor : term -> term -> term t

    val shl : term -> term -> term t

    val ashr : term -> term -> term t

    val lshr : term -> term -> term t

    val rem : term -> term -> term t

    val rem_u : term -> term -> term t

    val rotate_left : term -> term -> term t

    val rotate_right : term -> term -> term t

    val lt : term -> term -> term t

    val lt_u : term -> term -> term t

    val le : term -> term -> term t

    val le_u : term -> term -> term t

    val gt : term -> term -> term t

    val gt_u : term -> term -> term t

    val ge : term -> term -> term t

    val ge_u : term -> term -> term t

    val concat : term -> term -> term t

    val extract : term -> high:int -> low:int -> term t

    val zero_extend : int -> term -> term t

    val sign_extend : int -> term -> term t
  end

  module Float : sig
    (* Rounding modes *)
    module Rounding_mode : sig
      (* Round nearest ties to even *)
      val rne : term t

      (* Round nearest ties to away *)
      val rna : term t

      (* Round toward positive *)
      val rtp : term t

      (* Round toward negative *)
      val rtn : term t

      (* Round toward zero *)
      val rtz : term t
    end

    val v : float -> int -> int -> term t

    val neg : term -> term t

    val abs : term -> term t

    (* [sqrt ~rm t] where [rm] is the rounding mode *)
    val sqrt : rm:term -> term -> term t

    val is_nan : term -> term t

    (* [round_to_integral ~rm t] where [rm] is the rounding mode *)
    val round_to_integral : rm:term -> term -> term t

    (* [add ~rm t1 t2] where [rm] is the rounding mode *)
    val add : rm:term -> term -> term -> term t

    (* [sub ~rm t1 t2] where [rm] is the rounding mode *)
    val sub : rm:term -> term -> term -> term t

    (* [mul ~rm t1 t2] where [rm] is the rounding mode *)
    val mul : rm:term -> term -> term -> term t

    (* [div ~rm t1 t2] where [rm] is the rounding mode *)
    val div : rm:term -> term -> term -> term t

    val min : term -> term -> term t

    val max : term -> term -> term t

    val rem : term -> term -> term t

    val eq : term -> term -> term t

    val lt : term -> term -> term t

    val le : term -> term -> term t

    val gt : term -> term -> term t

    val ge : term -> term -> term t

    val to_fp : int -> int -> rm:term -> term -> term t

    val sbv_to_fp : int -> int -> rm:term -> term -> term t

    val ubv_to_fp : int -> int -> rm:term -> term -> term t

    val to_ubv : int -> rm:term -> term -> term t

    val to_sbv : int -> rm:term -> term -> term t

    val of_ieee_bv : int -> int -> term -> term t

    val to_ieee_bv : term -> term t
  end

  module Model : sig
    val get_symbols : model -> Symbol.t list t

    val eval : ?completion:bool -> model -> term -> interp option
  end

  module Solver : sig
    val make : ?params:Params.t -> ?logic:Ty.logic -> unit -> solver t

    val clone : solver -> solver t

    val push : solver -> unit

    val pop : solver -> int -> unit

    val reset : solver -> unit

    val add : solver -> term list -> unit

    val check : solver -> assumptions:term list -> satisfiability

    val model : solver -> model option

    val add_simplifier : solver -> solver t

    val interrupt : unit -> unit t

    val pp_statistics : Format.formatter -> solver -> unit
  end

  module Optimizer : sig
    val make : unit -> optimizer t

    val push : optimizer -> unit

    val pop : optimizer -> unit

    val add : optimizer -> term list -> unit

    val check : optimizer -> satisfiability

    val model : optimizer -> model option

    val maximize : optimizer -> term -> handle

    val minimize : optimizer -> term -> handle

    val interrupt : unit -> unit t

    val pp_statistics : Format.formatter -> optimizer -> unit
  end
end

module type S = sig
  type model

  type solver

  type optimize

  type handle

  val value : model -> Expr.t -> Value.t

  val values_of_model : ?symbols:Symbol.t list -> model -> Model.t

  val pp_smt : ?status:bool -> Format.formatter -> Expr.t list -> unit

  val set_debug : bool -> unit

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

    val pp_statistics : Format.formatter -> solver -> unit
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

    val pp_statistics : Format.formatter -> optimize -> unit
  end
end

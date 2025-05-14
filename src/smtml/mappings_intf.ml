(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2025 formalsec *)
(* Written by the Smtml programmers *)

(** Mappings Module. This module defines interfaces for interacting with SMT
    solvers, including term construction, type handling, solver interaction, and
    optimization. It provides a generic interface for working with different SMT
    solvers and their functionalities. *)

(** {1 Module Types} *)

(** The [M] module type defines the core interface for interacting with SMT
    solvers, including term construction, type handling, and solver interaction.
*)
module type M = sig
  module Internals : sig
    (** [is_available] indicates whether the module is available for use. *)
    val is_available : bool

    (** [caches_consts] indicates whether the solver caches constants. *)
    val caches_consts : bool

    (** [has_to_ieee_bv] indicates whether the solver native support for the
        [to_ieee_bv]. *)
    val has_to_ieee_bv : bool
  end

  (** The type of SMT sorts (types). *)
  type ty

  (** The type of SMT terms. *)
  type term

  (** The type of interpretations (evaluated values). *)
  type interp

  (** The type of SMT models. *)
  type model

  (** The type of SMT solvers. *)
  type solver

  (** The type of optimization handles. *)
  type handle

  (** The type of SMT optimizers. *)
  type optimizer

  (** The type of function declarations. *)
  type func_decl

  (** [true_] represents the Boolean constant [true]. *)
  val true_ : term

  (** [false_] represents the Boolean constant [false]. *)
  val false_ : term

  (** [int n] constructs an integer term from the given integer [n]. *)
  val int : int -> term

  (** [real f] constructs a real number term from the given float [f]. *)
  val real : float -> term

  (** [const name ty] constructs a constant term with the given name and type.
  *)
  val const : string -> ty -> term

  (** [not_ t] constructs the logical negation of the term [t]. *)
  val not_ : term -> term

  (** [and_ t1 t2] constructs the logical AND of the terms [t1] and [t2]. *)
  val and_ : term -> term -> term

  (** [or_ t1 t2] constructs the logical OR of the terms [t1] and [t2]. *)
  val or_ : term -> term -> term

  (** [logand ts] constructs the logical AND of a list of terms [ts]. *)
  val logand : term list -> term

  (** [logor ts] constructs the logical OR of a list of terms [ts]. *)
  val logor : term list -> term

  (** [xor t1 t2] constructs the logical XOR of the terms [t1] and [t2]. *)
  val xor : term -> term -> term

  (** [implies t1 t2] constructs the logical implication of the terms [t1] and
      [t2]. *)
  val implies : term -> term -> term

  (** [eq t1 t2] constructs the equality of the terms [t1] and [t2]. *)
  val eq : term -> term -> term

  (** [distinct ts] constructs the distinctness of a list of terms [ts]. *)
  val distinct : term list -> term

  (** [ite cond then_ else_] constructs an if-then-else term. *)
  val ite : term -> term -> term -> term

  (** [forall vars body] constructs a universal quantification term. *)
  val forall : term list -> term -> term

  (** [exists vars body] constructs an existential quantification term. *)
  val exists : term list -> term -> term

  (** {2 Type Handling} *)

  module Types : sig
    (** [int] represents the integer type. *)
    val int : ty

    (** [real] represents the real number type. *)
    val real : ty

    (** [bool] represents the Boolean type. *)
    val bool : ty

    (** [string] represents the string type. *)
    val string : ty

    (** [bitv n] represents a bitvector type of width [n]. *)
    val bitv : int -> ty

    (** [float e s] represents a floating-point type with exponent width [e] and
        significand width [s]. *)
    val float : int -> int -> ty

    (** [ty t] retrieves the type of the term [t]. *)
    val ty : term -> ty

    (** [to_ety ty] converts the type [ty] to an smtml type representation. *)
    val to_ety : ty -> Ty.t
  end

  (** {2 Interpretation Handling} *)

  module Interp : sig
    (** [to_int interp] converts an interpretation to an integer. *)
    val to_int : interp -> int

    (** [to_real interp] converts an interpretation to a real number. *)
    val to_real : interp -> float

    (** [to_bool interp] converts an interpretation to a Boolean. *)
    val to_bool : interp -> bool

    (** [to_string interp] converts an interpretation to a string. *)
    val to_string : interp -> string

    (** [to_bitv interp n] converts an interpretation to a bitvector of width
        [n]. *)
    val to_bitv : interp -> int -> int64

    (** [to_float interp e s] converts an interpretation to a floating-point
        number with exponent width [e] and significand width [s]. *)
    val to_float : interp -> int -> int -> float
  end

  (** {2 Integer Operations} *)

  module Int : sig
    (** [neg t] constructs the negation of the integer term [t]. *)
    val neg : term -> term

    (** [to_real t] converts the integer term [t] to a real number term. *)
    val to_real : term -> term

    (** [add t1 t2] constructs the sum of the integer terms [t1] and [t2]. *)
    val add : term -> term -> term

    (** [sub t1 t2] constructs the difference of the integer terms [t1] and
        [t2]. *)
    val sub : term -> term -> term

    (** [mul t1 t2] constructs the product of the integer terms [t1] and [t2].
    *)
    val mul : term -> term -> term

    (** [div t1 t2] constructs the quotient of the integer terms [t1] and [t2].
    *)
    val div : term -> term -> term

    (** [rem t1 t2] constructs the remainder of the integer terms [t1] and [t2].
    *)
    val rem : term -> term -> term

    (** [pow t1 t2] constructs the power of the integer terms [t1] and [t2]. *)
    val pow : term -> term -> term

    (** [lt t1 t2] constructs the less-than relation between [t1] and [t2]. *)
    val lt : term -> term -> term

    (** [le t1 t2] constructs the less-than-or-equal relation between [t1] and
        [t2]. *)
    val le : term -> term -> term

    (** [gt t1 t2] constructs the greater-than relation between [t1] and [t2].
    *)
    val gt : term -> term -> term

    (** [ge t1 t2] constructs the greater-than-or-equal relation between [t1]
        and [t2]. *)
    val ge : term -> term -> term
  end

  (** {2 Real Number Operations} *)

  module Real : sig
    (** [neg t] constructs the negation of the real number term [t]. *)
    val neg : term -> term

    (** [to_int t] converts the real number term [t] to an integer term. *)
    val to_int : term -> term

    (** [add t1 t2] constructs the sum of the real number terms [t1] and [t2].
    *)
    val add : term -> term -> term

    (** [sub t1 t2] constructs the difference of the real number terms [t1] and
        [t2]. *)
    val sub : term -> term -> term

    (** [mul t1 t2] constructs the product of the real number terms [t1] and
        [t2]. *)
    val mul : term -> term -> term

    (** [div t1 t2] constructs the quotient of the real number terms [t1] and
        [t2]. *)
    val div : term -> term -> term

    (** [pow t1 t2] constructs the power of the real number terms [t1] and [t2].
    *)
    val pow : term -> term -> term

    (** [lt t1 t2] constructs the less-than relation between [t1] and [t2]. *)
    val lt : term -> term -> term

    (** [le t1 t2] constructs the less-than-or-equal relation between [t1] and
        [t2]. *)
    val le : term -> term -> term

    (** [gt t1 t2] constructs the greater-than relation between [t1] and [t2].
    *)
    val gt : term -> term -> term

    (** [ge t1 t2] constructs the greater-than-or-equal relation between [t1]
        and [t2]. *)
    val ge : term -> term -> term
  end

  (** {2 String Operations} *)

  module String : sig
    (** [v s] constructs a string term from the string [s]. *)
    val v : string -> term

    (** [length t] constructs the length of the string term [t]. *)
    val length : term -> term

    (** [to_code t] constructs the Unicode code point of the first character in
        the string term [t]. *)
    val to_code : term -> term

    (** [of_code t] constructs a string term from the Unicode code point [t]. *)
    val of_code : term -> term

    (** [to_int t] converts the string term [t] to an integer term. *)
    val to_int : term -> term

    (** [of_int t] converts the integer term [t] to a string term. *)
    val of_int : term -> term

    (** [to_re t] converts the string term [t] to a regular expression term. *)
    val to_re : term -> term

    (** [at t ~pos] constructs the character at position [pos] in the string
        term [t]. *)
    val at : term -> pos:term -> term

    (** [concat ts] constructs the concatenation of a list of string terms [ts].
    *)
    val concat : term list -> term

    (** [contains t ~sub] checks if the string term [t] contains the substring
        [sub]. *)
    val contains : term -> sub:term -> term

    (** [is_prefix t ~prefix] checks if the string term [t] starts with the
        prefix [prefix]. *)
    val is_prefix : term -> prefix:term -> term

    (** [is_suffix t ~suffix] checks if the string term [t] ends with the suffix
        [suffix]. *)
    val is_suffix : term -> suffix:term -> term

    (** [in_re t re] checks if the string term [t] matches the regular
        expression [re]. *)
    val in_re : term -> term -> term

    (** [lt t1 t2] constructs the less-than relation between string terms [t1]
        and [t2]. *)
    val lt : term -> term -> term

    (** [le t1 t2] constructs the less-than-or-equal relation between string
        terms [t1] and [t2]. *)
    val le : term -> term -> term

    (** [sub t ~pos ~len] constructs the substring of [t] starting at [pos] with
        length [len]. *)
    val sub : term -> pos:term -> len:term -> term

    (** [index_of t ~sub ~pos] constructs the index of the first occurrence of
        [sub] in [t] starting at [pos]. *)
    val index_of : term -> sub:term -> pos:term -> term

    (** [replace t ~pattern ~with_] constructs the string term resulting from
        replacing [pattern] with [with_] in [t]. *)
    val replace : term -> pattern:term -> with_:term -> term
  end

  (** {2 Regular Expression Operations} *)

  module Re : sig
    (** [star t] constructs the Kleene star of the regular expression term [t].
    *)
    val star : term -> term

    (** [plus t] constructs the Kleene plus of the regular expression term [t].
    *)
    val plus : term -> term

    (** [opt t] constructs the optional regular expression term [t]. *)
    val opt : term -> term

    (** [comp t] constructs the complement of the regular expression term [t].
    *)
    val comp : term -> term

    (** [range t1 t2] constructs a regular expression term matching characters
        in the range from [t1] to [t2]. *)
    val range : term -> term -> term

    (** [loop t min max] constructs a regular expression term matching [t]
        repeated between [min] and [max] times. *)
    val loop : term -> int -> int -> term

    (** [union ts] constructs the union of a list of regular expression terms
        [ts]. *)
    val union : term list -> term

    (** [concat ts] constructs the concatenation of a list of regular expression
        terms [ts]. *)
    val concat : term list -> term
  end

  (** {2 Bitvector Operations} *)

  module Bitv : sig
    (** [v s n] constructs a bitvector term from the string [s] of width [n]. *)
    val v : string -> int -> term

    (** [neg t] constructs the negation of the bitvector term [t]. *)
    val neg : term -> term

    (** [lognot t] constructs the bitwise NOT of the bitvector term [t]. *)
    val lognot : term -> term

    (** [add t1 t2] constructs the sum of the bitvector terms [t1] and [t2]. *)
    val add : term -> term -> term

    (** [sub t1 t2] constructs the difference of the bitvector terms [t1] and
        [t2]. *)
    val sub : term -> term -> term

    (** [mul t1 t2] constructs the product of the bitvector terms [t1] and [t2].
    *)
    val mul : term -> term -> term

    (** [div t1 t2] constructs the quotient of the bitvector terms [t1] and
        [t2]. *)
    val div : term -> term -> term

    (** [div_u t1 t2] constructs the unsigned quotient of the bitvector terms
        [t1] and [t2]. *)
    val div_u : term -> term -> term

    (** [logor t1 t2] constructs the bitwise OR of the bitvector terms [t1] and
        [t2]. *)
    val logor : term -> term -> term

    (** [logand t1 t2] constructs the bitwise AND of the bitvector terms [t1]
        and [t2]. *)
    val logand : term -> term -> term

    (** [logxor t1 t2] constructs the bitwise XOR of the bitvector terms [t1]
        and [t2]. *)
    val logxor : term -> term -> term

    (** [shl t1 t2] constructs the left shift of [t1] by [t2]. *)
    val shl : term -> term -> term

    (** [ashr t1 t2] constructs the arithmetic right shift of [t1] by [t2]. *)
    val ashr : term -> term -> term

    (** [lshr t1 t2] constructs the logical right shift of [t1] by [t2]. *)
    val lshr : term -> term -> term

    (** [rem t1 t2] constructs the remainder of the bitvector terms [t1] and
        [t2]. *)
    val rem : term -> term -> term

    (** [rem_u t1 t2] constructs the unsigned remainder of the bitvector terms
        [t1] and [t2]. *)
    val rem_u : term -> term -> term

    (** [rotate_left t1 t2] constructs the left rotation of [t1] by [t2]. *)
    val rotate_left : term -> term -> term

    (** [rotate_right t1 t2] constructs the right rotation of [t1] by [t2]. *)
    val rotate_right : term -> term -> term

    (** [lt t1 t2] constructs the less-than relation between bitvector terms
        [t1] and [t2]. *)
    val lt : term -> term -> term

    (** [lt_u t1 t2] constructs the unsigned less-than relation between
        bitvector terms [t1] and [t2]. *)
    val lt_u : term -> term -> term

    (** [le t1 t2] constructs the less-than-or-equal relation between bitvector
        terms [t1] and [t2]. *)
    val le : term -> term -> term

    (** [le_u t1 t2] constructs the unsigned less-than-or-equal relation between
        bitvector terms [t1] and [t2]. *)
    val le_u : term -> term -> term

    (** [gt t1 t2] constructs the greater-than relation between bitvector terms
        [t1] and [t2]. *)
    val gt : term -> term -> term

    (** [gt_u t1 t2] constructs the unsigned greater-than relation between
        bitvector terms [t1] and [t2]. *)
    val gt_u : term -> term -> term

    (** [ge t1 t2] constructs the greater-than-or-equal relation between
        bitvector terms [t1] and [t2]. *)
    val ge : term -> term -> term

    (** [ge_u t1 t2] constructs the unsigned greater-than-or-equal relation
        between bitvector terms [t1] and [t2]. *)
    val ge_u : term -> term -> term

    (** [concat t1 t2] constructs the concatenation of the bitvector terms [t1]
        and [t2]. *)
    val concat : term -> term -> term

    (** [extract t ~high ~low] extracts a bit range from [t] between [high] and
        [low]. *)
    val extract : term -> high:int -> low:int -> term

    (** [zero_extend n t] zero-extends the bitvector term [t] by [n] bits. *)
    val zero_extend : int -> term -> term

    (** [sign_extend n t] sign-extends the bitvector term [t] by [n] bits. *)
    val sign_extend : int -> term -> term
  end

  (** {2 Floating-Point Operations} *)

  module Float : sig
    (** Rounding modes for floating-point operations. *)
    module Rounding_mode : sig
      (** [rne] represents the "round nearest ties to even" rounding mode. *)
      val rne : term

      (** [rna] represents the "round nearest ties to away" rounding mode. *)
      val rna : term

      (** [rtp] represents the "round toward positive" rounding mode. *)
      val rtp : term

      (** [rtn] represents the "round toward negative" rounding mode. *)
      val rtn : term

      (** [rtz] represents the "round toward zero" rounding mode. *)
      val rtz : term
    end

    (** [v f e s] constructs a floating-point term from the float [f] with
        exponent width [e] and significand width [s]. *)
    val v : float -> int -> int -> term

    (** [neg t] constructs the negation of the floating-point term [t]. *)
    val neg : term -> term

    (** [abs t] constructs the absolute value of the floating-point term [t]. *)
    val abs : term -> term

    (** [sqrt ~rm t] constructs the square root of the floating-point term [t]
        using the rounding mode [rm]. *)
    val sqrt : rm:term -> term -> term

    (** [is_nan t] checks if the floating-point term [t] is NaN. *)
    val is_nan : term -> term

    (** [round_to_integral ~rm t] rounds the floating-point term [t] to an
        integral value using the rounding mode [rm]. *)
    val round_to_integral : rm:term -> term -> term

    (** [add ~rm t1 t2] constructs the sum of the floating-point terms [t1] and
        [t2] using the rounding mode [rm]. *)
    val add : rm:term -> term -> term -> term

    (** [sub ~rm t1 t2] constructs the difference of the floating-point terms
        [t1] and [t2] using the rounding mode [rm]. *)
    val sub : rm:term -> term -> term -> term

    (** [mul ~rm t1 t2] constructs the product of the floating-point terms [t1]
        and [t2] using the rounding mode [rm]. *)
    val mul : rm:term -> term -> term -> term

    (** [div ~rm t1 t2] constructs the quotient of the floating-point terms [t1]
        and [t2] using the rounding mode [rm]. *)
    val div : rm:term -> term -> term -> term

    (** [min t1 t2] constructs the minimum of the floating-point terms [t1] and
        [t2]. *)
    val min : term -> term -> term

    (** [max t1 t2] constructs the maximum of the floating-point terms [t1] and
        [t2]. *)
    val max : term -> term -> term

    (** [rem t1 t2] constructs the remainder of the floating-point terms [t1]
        and [t2]. *)
    val rem : term -> term -> term

    (** [eq t1 t2] constructs the equality of the floating-point terms [t1] and
        [t2]. *)
    val eq : term -> term -> term

    (** [lt t1 t2] constructs the less-than relation between floating-point
        terms [t1] and [t2]. *)
    val lt : term -> term -> term

    (** [le t1 t2] constructs the less-than-or-equal relation between
        floating-point terms [t1] and [t2]. *)
    val le : term -> term -> term

    (** [gt t1 t2] constructs the greater-than relation between floating-point
        terms [t1] and [t2]. *)
    val gt : term -> term -> term

    (** [ge t1 t2] constructs the greater-than-or-equal relation between
        floating-point terms [t1] and [t2]. *)
    val ge : term -> term -> term

    (** [to_fp e s ~rm t] converts the term [t] to a floating-point term with
        exponent width [e] and significand width [s] using the rounding mode
        [rm]. *)
    val to_fp : int -> int -> rm:term -> term -> term

    (** [sbv_to_fp e s ~rm t] converts the signed bitvector term [t] to a
        floating-point term with exponent width [e] and significand width [s]
        using the rounding mode [rm]. *)
    val sbv_to_fp : int -> int -> rm:term -> term -> term

    (** [ubv_to_fp e s ~rm t] converts the unsigned bitvector term [t] to a
        floating-point term with exponent width [e] and significand width [s]
        using the rounding mode [rm]. *)
    val ubv_to_fp : int -> int -> rm:term -> term -> term

    (** [to_ubv n ~rm t] converts the floating-point term [t] to an unsigned
        bitvector term of width [n] using the rounding mode [rm]. *)
    val to_ubv : int -> rm:term -> term -> term

    (** [to_sbv n ~rm t] converts the floating-point term [t] to a signed
        bitvector term of width [n] using the rounding mode [rm]. *)
    val to_sbv : int -> rm:term -> term -> term

    (** [of_ieee_bv e s t] constructs a floating-point term from the IEEE
        bitvector term [t] with exponent width [e] and significand width [s]. *)
    val of_ieee_bv : int -> int -> term -> term

    (** [to_ieee_bv t] converts the floating-point term [t] to an IEEE bitvector
        term. *)
    val to_ieee_bv : term -> term
  end

  (** {2 Function Handling} *)

  module Func : sig
    (** [make name arg_tys ret_ty] constructs a function declaration with the
        given name, argument types [arg_tys], and return type [ret_ty]. *)
    val make : string -> ty list -> ty -> func_decl

    (** [apply f args] applies the function declaration [f] to the arguments
        [args]. *)
    val apply : func_decl -> term list -> term
  end

  (** {2 Model Handling} *)

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

  (** {2 Solver Handling} *)

  module Solver : sig
    (** [make ?params ?logic ()] creates a new solver with optional parameters
        [params] and logic [logic]. *)
    val make : ?params:Params.t -> ?logic:Logic.t -> unit -> solver

    (** [clone solver] creates a copy of the solver [solver]. *)
    val clone : solver -> solver

    (** [push solver] pushes a new context level onto the solver [solver]. *)
    val push : solver -> unit

    (** [pop solver n] pops [n] context levels from the solver [solver]. *)
    val pop : solver -> int -> unit

    (** [reset solver] resets the solver [solver] to its initial state. *)
    val reset : solver -> unit

    (** [add solver ts] adds the terms [ts] to the solver [solver]. *)
    val add : ?ctx:term Symbol.Map.t -> solver -> term list -> unit

    (** [check solver ~assumptions] checks the satisfiability of the solver
        [solver] with the given [assumptions]. Returns [`Sat], [`Unsat], or
        [`Unknown]. *)
    val check :
         ?ctx:term Symbol.Map.t
      -> solver
      -> assumptions:term list
      -> [ `Sat | `Unsat | `Unknown ]

    (** [model solver] retrieves the model from the solver [solver], if one
        exists. *)
    val model : solver -> model option

    (** [add_simplifier solver] adds a simplifier to the solver [solver]. *)
    val add_simplifier : solver -> solver

    (** [interrupt ()] interrupts the current solver operation. *)
    val interrupt : unit -> unit

    (** [get_statistics solver] retrieves statistics from the solver [solver].
    *)
    val get_statistics : solver -> Statistics.t

    (** [pp_statistics fmt solver] pretty-prints the statistics of the solver
        [solver] using the formatter [fmt]. *)
    val pp_statistics : solver Fmt.t
  end

  (** {2 Optimizer Handling} *)

  module Optimizer : sig
    (** [make ()] creates a new optimizer. *)
    val make : unit -> optimizer

    (** [push optimizer] pushes a new context level onto the optimizer
        [optimizer]. *)
    val push : optimizer -> unit

    (** [pop optimizer] pops a context level from the optimizer [optimizer]. *)
    val pop : optimizer -> unit

    (** [add optimizer ts] adds the terms [ts] to the optimizer [optimizer]. *)
    val add : optimizer -> term list -> unit

    (** [check optimizer] checks the satisfiability of the optimizer
        [optimizer]. Returns [`Sat], [`Unsat], or [`Unknown]. *)
    val check : optimizer -> [ `Sat | `Unsat | `Unknown ]

    (** [model optimizer] retrieves the model from the optimizer [optimizer], if
        one exists. *)
    val model : optimizer -> model option

    (** [maximize optimizer t] maximizes the term [t] in the optimizer
        [optimizer]. *)
    val maximize : optimizer -> term -> handle

    (** [minimize optimizer t] minimizes the term [t] in the optimizer
        [optimizer]. *)
    val minimize : optimizer -> term -> handle

    (** [interrupt ()] interrupts the current optimizer operation. *)
    val interrupt : unit -> unit

    (** [get_statistics optimizer] retrieves statistics from the optimizer
        [optimizer]. *)
    val get_statistics : optimizer -> Statistics.t

    (** [pp_statistics fmt optimizer] pretty-prints the statistics of the
        optimizer [optimizer] using the formatter [fmt]. *)
    val pp_statistics : optimizer Fmt.t
  end

  (** {2 SMT-LIB Pretty Printing} *)

  module Smtlib : sig
    (** [pp ?name ?logic ?status fmt ts] pretty-prints the terms [ts] in SMT-LIB
        format using the formatter [fmt]. Optional parameters include [name] for
        the script name, [logic] for the logic, and [status] for the script
        status. *)
    val pp :
         ?name:string
      -> ?logic:Logic.t
      -> ?status:[ `Sat | `Unsat | `Unknown ]
      -> term list Fmt.t
  end
end

(** The [M_with_make] module type extends [M] with a functor for creating
    instances of [M] and a flag indicating availability. *)
module type M_with_make = sig
  (** [Make ()] creates a new instance of the [M] module type. *)
  module Make () : M

  (** [is_available] indicates whether the module is available for use.

      Will be deprecated in the future, please use Internals.is_available
      instead. *)
  val is_available : bool

  (** Include the [M] module type. *)
  include M
end

(** The [S] module type defines a simplified interface for interacting with SMT
    solvers, focusing on model evaluation, solver interaction, and optimization.
*)
module type S = sig
  (** The type of SMT models. *)
  type model

  (** The type of SMT solvers. *)
  type solver

  (** The type of optimizers. *)
  type optimize

  (** The type of optimization handles. *)
  type handle

  (** [value model expr] evaluates the expression [expr] in the given [model].
  *)
  val value : model -> Expr.t -> Value.t

  (** [values_of_model ?symbols model] retrieves the values of the given
      [symbols] (or all symbols if not provided) from the [model]. *)
  val values_of_model : ?symbols:Symbol.t list -> model -> Model.t

  (** [set_debug flag] enables or disables debug mode based on [flag]. *)
  val set_debug : bool -> unit

  (** {2 SMT-LIB Pretty Printing} *)

  module Smtlib : sig
    (** [pp ?name ?logic ?status fmt ts] pretty-prints the terms [ts] in SMT-LIB
        format using the formatter [fmt]. Optional parameters include [name] for
        the script name, [logic] for the logic, and [status] for the script
        status. *)
    val pp :
         ?name:string
      -> ?logic:Logic.t
      -> ?status:[ `Sat | `Unsat | `Unknown ]
      -> Expr.t list Fmt.t
  end

  (** {2 Solver Handling} *)

  module Solver : sig
    (** [make ?params ?logic ()] creates a new solver with optional parameters
        [params] and logic [logic]. *)
    val make : ?params:Params.t -> ?logic:Logic.t -> unit -> solver

    (** [add_simplifier solver] adds a simplifier to the solver [solver]. *)
    val add_simplifier : solver -> solver

    (** [clone solver] creates a copy of the solver [solver]. *)
    val clone : solver -> solver

    (** [push solver] pushes a new context level onto the solver [solver]. *)
    val push : solver -> unit

    (** [pop solver n] pops [n] context levels from the solver [solver]. *)
    val pop : solver -> int -> unit

    (** [reset solver] resets the solver [solver] to its initial state. *)
    val reset : solver -> unit

    (** [add solver exprs] adds the expressions [exprs] to the solver [solver].
    *)
    val add : solver -> Expr.t list -> unit

    (** [check solver ~assumptions] checks the satisfiability of the solver
        [solver] with the given [assumptions]. Returns [`Sat], [`Unsat], or
        [`Unknown]. *)
    val check :
      solver -> assumptions:Expr.t list -> [ `Sat | `Unsat | `Unknown ]

    (** [model solver] retrieves the model from the solver [solver], if one
        exists. *)
    val model : solver -> model option

    (** [interrupt solver] interrupts the current solver operation. *)
    val interrupt : solver -> unit

    (** [get_statistics solver] retrieves statistics from the solver [solver].
    *)
    val get_statistics : solver -> Statistics.t
  end

  (** {2 Optimizer Handling} *)

  module Optimizer : sig
    (** [make ()] creates a new optimizer. *)
    val make : unit -> optimize

    (** [push optimizer] pushes a new context level onto the optimizer
        [optimizer]. *)
    val push : optimize -> unit

    (** [pop optimizer] pops a context level from the optimizer [optimizer]. *)
    val pop : optimize -> unit

    (** [add optimizer exprs] adds the expressions [exprs] to the optimizer
        [optimizer]. *)
    val add : optimize -> Expr.t list -> unit

    (** [check optimizer] checks the satisfiability of the optimizer
        [optimizer]. Returns [`Sat], [`Unsat], or [`Unknown]. *)
    val check : optimize -> [ `Sat | `Unsat | `Unknown ]

    (** [model optimizer] retrieves the model from the optimizer [optimizer], if
        one exists. *)
    val model : optimize -> model option

    (** [maximize optimizer expr] maximizes the expression [expr] in the
        optimizer [optimizer]. *)
    val maximize : optimize -> Expr.t -> handle

    (** [minimize optimizer expr] minimizes the expression [expr] in the
        optimizer [optimizer]. *)
    val minimize : optimize -> Expr.t -> handle

    (** [interrupt optimizer] interrupts the current optimizer operation. *)
    val interrupt : optimize -> unit

    (** [get_statistics optimizer] retrieves statistics from the optimizer
        [optimizer]. *)
    val get_statistics : optimize -> Statistics.t
  end
end

(** The [S_with_fresh] module type extends [S] with a functor for creating fresh
    instances of [S] and a flag indicating availability of the mappings. *)
module type S_with_fresh = sig
  (** [Fresh.Make ()] creates a new instance of the [S] module type. *)
  module Fresh : sig
    module Make () : S
  end

  (** [is_available] indicates whether the module is available for use. *)
  val is_available : bool

  (** Include the [S] module type. *)
  include S
end

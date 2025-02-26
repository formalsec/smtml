(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2025 formalsec *)
(* Written by the Smtml programmers *)

(** Parameter Management Module. This module defines a type-safe interface for
    handling solver parameters, allowing configuration of options such as
    timeouts, model production, and parallel execution. *)

(** {1 Parameter Types} *)

(** The type [t] represents a set of solver parameters. *)
type t

(** The type [_ param] represents a type-safe parameter, where the type
    parameter indicates the type of the parameter's value. *)
type _ param =
  | Timeout : int param
    (** Specifies a timeout in milliseconds for each [check] call. *)
  | Model : bool param  (** Enable or disable model production. *)
  | Unsat_core : bool param
    (** Enable or disable unsatisfiable core computation. *)
  | Ematching : bool param  (** Enable or disable ematching. *)
  | Parallel : bool param  (** Enable or disable parallel execution. *)
  | Num_threads : int param
    (** Specifies the maximum number of threads to use in parallel mode. *)

(** The type [param'] is a wrapper for storing parameter-value pairs. *)
type param' = P : 'a param * 'a -> param'

(** {1 Default Values} *)

(** [default_value param] returns the default value associated with the given
    parameter. *)
val default_value : 'a param -> 'a

(** [default ()] returns a parameter set with default values for all parameters.
*)
val default : unit -> t

(** {1 Parameter Manipulation} *)

(** Infix operator for setting a parameter.

    [params $ (param, value)] updates the parameter set [params] by setting
    [param] to [value]. *)
val ( $ ) : t -> 'a param * 'a -> t

(** [set params param value] updates the parameter set [params] by setting
    [param] to [value]. *)
val set : t -> 'a param -> 'a -> t

(** [opt params param opt_value] conditionally updates the parameter set
    [params]. If [opt_value] is [Some v], it sets [param] to [v]. Otherwise,
    [params] remains unchanged. *)
val opt : t -> 'a param -> 'a option -> t

(** [get params param] retrieves the current value of [param] from the parameter
    set [params]. *)
val get : t -> 'a param -> 'a

(** {1 Conversion} *)

(** [to_list params] converts the parameter set [params] into a list of
    parameter-value pairs. *)
val to_list : t -> param' list

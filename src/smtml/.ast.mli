(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2025 formalsec *)
(* Written by the Smtml programmers *)

(** SMT-LIB Commands Representation. This module defines types and utilities for
    representing SMT-LIB commands, which are used to interact with SMT solvers.
    It includes commands for assertions, declarations, solver control, and
    metadata management. *)

(** {1 Command Types} *)

(** A type representing various SMT-LIB commands. *)
type t =
  | Assert of Expr.t
    (** [Assert expr] adds an assertion [expr] to the current set of
        constraints. *)
  | Check_sat of Expr.t list
    (** [Check_sat assumptions] checks the satisfiability of the current set of
        constraints, optionally using additional assumptions. *)
  | Declare_const of
      { id : Symbol.t  (** The identifier of the constant. *)
      ; sort : Symbol.t  (** The sort (type) of the declared constant. *)
      }
    (** [Declare_const { id; sort }] declares a new constant [id] of type
        [sort]. *)
  | Declare_fun of
      { id : Symbol.t  (** The identifier of the constant. *)
      ; args : Symbol.t list  (** The sorts (types) of the arguments. *)
      ; sort : Symbol.t  (** The sort (type) of the declared function. *)
      }
    (** [Declare_fun { id; args; sort }] declares a new constant [id] of type
        [args -> sort]. *)
  | Echo of string
    (** [Echo msg] prints the given message [msg] to the standard output. *)
  | Exit  (** [Exit] terminates the SMT solver session. *)
  | Get_assertions
    (** [Get_assertions] retrieves the current set of asserted formulas. *)
  | Get_assignment
    (** [Get_assignment] retrieves the current truth assignments for Boolean
        variables. *)
  | Get_info of string
    (** [Get_info key] retrieves metadata or configuration information
        associated with [key]. *)
  | Get_option of string
    (** [Get_option key] retrieves the value of the solver option identified by
        [key]. *)
  | Get_model
    (** [Get_model] requests a model for the current set of constraints if the
        problem is satisfiable. *)
  | Get_value of Expr.t list
    (** [Get_value exprs] retrieves the values of the given expressions [exprs]
        in the current model. *)
  | Pop of int
    (** [Pop n] removes the top [n] levels from the assertion stack. *)
  | Push of int  (** [Push n] creates [n] new levels on the assertion stack. *)
  | Reset  (** [Reset] clears all assertions and resets the solver state. *)
  | Reset_assertions
    (** [Reset_assertions] clears all assertions without modifying solver
        settings. *)
  | Set_info of Expr.t
    (** [Set_info expr] attaches metadata to the solver using the given [expr].
    *)
  | Set_logic of Logic.t
    (** [Set_logic logic] specifies the logic to be used by the solver. *)
  | Set_option of Expr.t
    (** [Set_option expr] sets a solver option with the given expression [expr].
    *)

(** {1 Script Type} *)

(** A type representing a sequence of SMT-LIB commands. *)
type script = t list

(** {1 Pretty Printing} *)

(** Pretty-printer for SMT-LIB commands. Formats a command for human-readable
    output. *)
val pp : t Fmt.t

(** {1 Serialization} *)

(** Converts an SMT-LIB command to its string representation. *)
val to_string : t -> string

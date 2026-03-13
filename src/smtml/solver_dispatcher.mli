(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2025 formalsec *)
(* Written by the Smtml programmers *)

(** Solver Query Module. This module provides functions for querying available
    solvers and obtaining mappings for specific solver types. It allows checking
    solver availability, listing available solvers, and retrieving solver
    mappings. *)

(** {1 Solver Availability} *)

(** [is_available solver] checks if the given solver is available.

    {b Note:} This function will be deprecated in favor of
    [Solver_type.is_available]. *)
val is_available : Solver_type.t -> bool

(** [available] returns a list of all available solvers.

    The list can be empty if no solvers are installed. *)
val available : Solver_type.t list

(** {1 Solver Retrieval} *)

(** [solver] returns the first available solver or an error if none exist.

    The returned solver is wrapped in a result type to handle cases where no
    solvers are available. *)
val solver : ((module Mappings.S_with_fresh), [> `Msg of string ]) result

(** [mappings_of_solver solver] returns the solver mappings for the given solver
    type. *)
val mappings_of_solver : Solver_type.t -> (module Mappings.S_with_fresh)

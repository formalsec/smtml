(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2024 formalsec *)
(* Written by the Smtml programmers *)

(** Will be deprecated in favour of Solver_type *)
val is_available : Solver_type.t -> bool

(** List of all available solvers. Can be empty if no solver installed. *)
val available : Solver_type.t list

(** Returns first available solver or errors when none exist *)
val solver : ((module Mappings.S_with_fresh), [> `Msg of string ]) result

val mappings_of_solver : Solver_type.t -> (module Mappings.S_with_fresh)

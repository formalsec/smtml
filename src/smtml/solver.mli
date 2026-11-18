(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2024 formalsec *)
(* Written by the Smtml programmers *)

(** @inline *)
include Solver_intf.Intf

val slice_algorithm : bool ref

val unsat_cache : bool ref

val unsat_plus : bool ref

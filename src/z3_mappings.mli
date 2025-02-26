(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2024 formalsec *)
(* Written by the Smtml programmers *)

(** Z3 Solver Mappings. This module defines types and utilities for mapping
    between internal representations and the format used by the Z3 solver. It
    provides functions to translate problems into solver-specific inputs and to
    interpret solver outputs. *)

(** @inline *)
include Mappings_intf.S_with_fresh

(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2024 formalsec *)
(* Written by Hichem Rami Ait El Hara *)

(** Colibri2 Solver Mappings. This module defines types and utilities for
    mapping between internal representations and the format used by the Colibri2
    solver. It provides functions to translate problems into solver-specific
    inputs and to interpret solver outputs. *)

(** @inline *)
include Mappings_intf.S_with_fresh

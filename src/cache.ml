(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2024 formalsec *)
(* Written by the Smtml programmers *)

include Cache_intf

module Strong : S = Hashtbl.Make (Expr.Set)

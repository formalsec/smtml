(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2024 formalsec *)
(* Written by the Smtml programmers *)

module StringMap : Map.S with type key = string

val extract_feats : Expr.t -> int StringMap.t

val read_marshalled_file : string -> (string * Expr.t list * bool * int64) list

val extract_feats_wtime : Expr.t list -> int64 -> int StringMap.t

val cmd : Fpath.t -> Fpath.t -> unit

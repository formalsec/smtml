(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2026 formalsec *)
(* Written by the Smtml programmers *)

open Regression_model

val read_marshalled_file :
     Fpath.t
  -> ( (string * Expr.t list * bool * int64 * [ `Sat | `Unsat | `Unknown ]) list
     , [ `Msg of string ] )
     result

val extract_feats : Expr.t list -> features

val extract_feats_wtime : Expr.t list -> int64 -> features

val cmd : Fpath.t -> Fpath.t -> unit

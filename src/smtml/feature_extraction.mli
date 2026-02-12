(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2024 formalsec *)
(* Written by the Smtml programmers *)

module FeatMap : sig
  include Map.S with type key = string

  val find_def0 : key -> int t -> int
end

type features = int FeatMap.t

val read_marshalled_file : string -> (string * Expr.t list * bool * int64) list

val extract_feats : Expr.t list -> features

val extract_feats_wtime : Expr.t list -> int64 -> features

val cmd : Fpath.t -> Fpath.t -> unit

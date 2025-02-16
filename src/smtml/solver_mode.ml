(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2024 formalsec *)
(* Written by the Smtml programmers *)

type t =
  | Batch
  | Cached
  | Incremental

let pp fmt = function
  | Batch -> Fmt.string fmt "batch"
  | Cached -> Fmt.string fmt "cached"
  | Incremental -> Fmt.string fmt "incremental"

let of_string = function
  | "batch" -> Ok Batch
  | "cached" -> Ok Cached
  | "incremental" -> Ok Incremental
  | _mode -> Error (`Msg (Fmt.str "unknown prover mode"))

let conv = Cmdliner.Arg.conv (of_string, pp)

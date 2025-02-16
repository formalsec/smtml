(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2024 formalsec *)
(* Written by the Smtml programmers *)

type t =
  | Batch
  | Cached
  | Incremental

val pp : t Fmt.t

val of_string : string -> (t, [> `Msg of string ]) result

val conv : t Cmdliner.Arg.conv

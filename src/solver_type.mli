(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2024 formalsec *)
(* Written by the Smtml programmers *)

type t =
  | Z3_solver
  | Bitwuzla_solver
  | Colibri2_solver
  | Cvc5_solver
  | Altergo_solver

val of_string : string -> (t, [> `Msg of string ]) result

val pp : t Fmt.t

val conv : t Cmdliner.Arg.conv

val is_available : t -> bool

val to_mappings : t -> (module Mappings.S_with_fresh)

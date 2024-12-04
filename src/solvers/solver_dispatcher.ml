(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2024 formalsec *)
(* Written by the Smtml programmers *)

open Solver_type

let is_available = function
  | Z3_solver -> Z3_mappings.is_available
  | Bitwuzla_solver -> Bitwuzla_mappings.is_available
  | Colibri2_solver -> Colibri2_mappings.is_available
  | Cvc5_solver -> Cvc5_mappings.is_available
  | Altergo_solver -> Altergo_mappings.is_available

let available =
  List.filter is_available
    [ Z3_solver; Bitwuzla_solver; Colibri2_solver; Cvc5_solver ]

let mappings_of_solver : Solver_type.t -> (module Mappings.S_with_fresh) =
  function
  | Z3_solver -> (module Z3_mappings)
  | Bitwuzla_solver -> (module Bitwuzla_mappings)
  | Colibri2_solver -> (module Colibri2_mappings)
  | Cvc5_solver -> (module Cvc5_mappings)
  | Altergo_solver -> (module Altergo_mappings)

let solver =
  match available with
  | [] -> Error (`Msg "no available solver")
  | solver :: _ -> Ok (mappings_of_solver solver)

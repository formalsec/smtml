(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2024 formalsec *)
(* Written by the Smtml programmers *)

(* TODO: put this in some other more appropriate module? *)
type solver_type =
  | Z3_solver
  | Bitwuzla_solver
  | Colibri2_solver
  | Cvc5_solver
  | Altergo_solver

let is_available = function
  | Z3_solver -> Z3_mappings.is_available
  | Bitwuzla_solver -> Bitwuzla_mappings.is_available
  | Colibri2_solver -> Colibri2_mappings.is_available
  | Cvc5_solver -> Cvc5_mappings.is_available
  | Altergo_solver -> Altergo_mappings.is_available

let available_solvers =
  List.filter is_available
    [ Z3_solver; Bitwuzla_solver; Colibri2_solver; Cvc5_solver ]

let mappings_of_solver : solver_type -> (module Mappings.S_with_fresh) =
  function
  | Z3_solver -> (module Z3_mappings)
  | Bitwuzla_solver -> (module Bitwuzla_mappings)
  | Colibri2_solver -> (module Colibri2_mappings)
  | Cvc5_solver -> (module Cvc5_mappings)
  | Altergo_solver -> (module Altergo_mappings)

let solver_type_of_string s =
  match String.map Char.lowercase_ascii s with
  | "z3" -> Ok Z3_solver
  | "bitwuzla" -> Ok Bitwuzla_solver
  | "colibri2" -> Ok Colibri2_solver
  | "cvc5" -> Ok Cvc5_solver
  | "alt-ergo" -> Ok Altergo_solver
  | s -> Error (`Msg (Fmt.str "unknown solver %s" s))

let solver =
  match available_solvers with
  | [] -> Error (`Msg "no available solver")
  | solver :: _ -> Ok (mappings_of_solver solver)

let pp_solver_type fmt = function
  | Z3_solver -> Fmt.pf fmt "Z3"
  | Bitwuzla_solver -> Fmt.pf fmt "Bitwuzla"
  | Colibri2_solver -> Fmt.pf fmt "Colibri2"
  | Cvc5_solver -> Fmt.pf fmt "cvc5"
  | Altergo_solver -> Fmt.pf fmt "Alt-Ergo"

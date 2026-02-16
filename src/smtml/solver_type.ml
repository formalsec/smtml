(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2024 formalsec *)
(* Written by the Smtml programmers *)

type t =
  | Z3_solver
  | Bitwuzla_solver
  | Colibri2_solver
  | Cvc5_solver
  | Altergo_solver
  | Smtzilla_solver

let of_string s =
  match String.map Char.lowercase_ascii s with
  | "z3" -> Ok Z3_solver
  | "bitwuzla" -> Ok Bitwuzla_solver
  | "colibri2" -> Ok Colibri2_solver
  | "cvc5" -> Ok Cvc5_solver
  | "alt-ergo" -> Ok Altergo_solver
  | "smtzilla" -> Ok Smtzilla_solver
  | s -> Error (`Msg (Fmt.str "unknown solver %s" s))

let pp fmt = function
  | Z3_solver -> Fmt.string fmt "Z3"
  | Bitwuzla_solver -> Fmt.string fmt "Bitwuzla"
  | Colibri2_solver -> Fmt.string fmt "Colibri2"
  | Cvc5_solver -> Fmt.string fmt "cvc5"
  | Altergo_solver -> Fmt.string fmt "Alt-Ergo"
  | Smtzilla_solver -> Fmt.string fmt "SMTZilla"

let conv = Cmdliner.Arg.conv (of_string, pp)

let is_available = function
  | Z3_solver -> Z3_mappings.is_available
  | Bitwuzla_solver -> Bitwuzla_mappings.is_available
  | Colibri2_solver -> Colibri2_mappings.is_available
  | Cvc5_solver -> Cvc5_mappings.is_available
  | Altergo_solver -> Altergo_mappings.is_available
  | Smtzilla_solver ->
    Z3_mappings.is_available || Bitwuzla_mappings.is_available

let to_mappings : t -> (module Mappings.S_with_fresh) = function
  | Z3_solver -> (module Z3_mappings)
  | Bitwuzla_solver -> (module Bitwuzla_mappings)
  | Colibri2_solver -> (module Colibri2_mappings)
  | Cvc5_solver -> (module Cvc5_mappings)
  | Altergo_solver -> (module Altergo_mappings)
  | Smtzilla_solver -> (module Smtzilla)

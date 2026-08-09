(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2026 formalsec *)
(* Written by the Smtml programmers *)

open Smtml

type availability =
  | Installed of string
  | Installed_no_version_info
  | Installed_via of string
  | Not_installed

let pp_availability fmt = function
  | Installed version -> Fmt.pf fmt "(installed: %s)" version
  | Installed_no_version_info -> Fmt.pf fmt "(installed: no version info)"
  | Installed_via deps -> Fmt.pf fmt "(installed via %s)" deps
  | Not_installed -> Fmt.pf fmt "(not installed)"

let package_name = function
  | Solver_type.Z3_solver -> Some "z3"
  | Bitwuzla_solver -> Some "bitwuzla-cxx"
  | Colibri2_solver -> Some "colibrilib"
  | Cvc5_solver -> Some "cvc5"
  | Altergo_solver -> Some "alt-ergo-lib"
  | Smtzilla_solver -> None

let get_availability solv_ty =
  match package_name solv_ty with
  | None ->
    if Solver_type.is_available solv_ty then Installed_via "Z3/Bitwuzla"
    else Not_installed
  | Some name ->
    begin match Build_info.V1.Statically_linked_libraries.find ~name with
    | None -> Not_installed
    | Some pkg ->
      begin match Build_info.V1.Statically_linked_library.version pkg with
      | None -> Installed_no_version_info
      | Some version -> Installed (Build_info.V1.Version.to_string version)
      end
    end

let run () =
  List.iter
    (fun t ->
      let availability = get_availability t in
      Fmt.pr "- %a %a@." Solver_type.pp t pp_availability availability )
    Solver_type.supported_solvers

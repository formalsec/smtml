(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2026 formalsec *)
(* Written by the Smtml programmers *)

open Smtml
open Smtml_test_solver

let is_available () =
  Alcotest.(check bool)
    "Alt-ergo is_available" true Altergo_mappings.is_available

let () =
  let module Alt_ergo = Test_solver.Make (Altergo_mappings) in
  Alcotest.run "Alt-ergo"
    [ ("is_available", [ Alcotest.test_case "is_available" `Quick is_available ])
    ; Alt_ergo.test_params
    ; Alt_ergo.test_cached
    ; Alt_ergo.test_lia
    ; Alt_ergo.test_bv
    ; Alt_ergo.test_extract
    ; Alt_ergo.test_typed_api_consistency
    ]

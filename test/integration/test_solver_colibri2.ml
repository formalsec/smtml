(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2026 formalsec *)
(* Written by the Smtml programmers *)

open Smtml
open Smtml_test_solver

let is_available () =
  Alcotest.(check bool)
    "Colibri2 is_available" true Colibri2_mappings.is_available

let () =
  let module C2 = Test_solver.Make (Colibri2_mappings) in
  Alcotest.run "Colibri2"
    [ ("is_available", [ Alcotest.test_case "is_available" `Quick is_available ])
    ; C2.test_params
    ; C2.test_cached
    ; C2.test_bv
    ; C2.test_fp
    ; C2.test_lia
    ; C2.test_extract
    ; C2.test_typed_api_consistency
    ]

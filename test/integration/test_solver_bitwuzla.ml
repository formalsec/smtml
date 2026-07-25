(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2026 formalsec *)
(* Written by the Smtml programmers *)

open Smtml
open Smtml_test_solver

let is_available () =
  Alcotest.(check bool)
    "Bitwuzla is_available" true Bitwuzla_mappings.is_available

let () =
  let module Bitwuzla = Test_solver.Make (Bitwuzla_mappings) in
  Alcotest.run "Bitwuzla"
    [ ("is_available", [ Alcotest.test_case "is_available" `Quick is_available ])
    ; Bitwuzla.test_params
    ; Bitwuzla.test_bv
    ; ( "test_bv_ext_rotate"
      , [ Alcotest.test_case "test_bv_ext_rotate" `Quick
            (Bitwuzla.with_solver Bitwuzla.test_bv_ext_rotate)
        ] )
    ; Bitwuzla.test_fp
    ; Bitwuzla.test_extract
    ; Bitwuzla.test_typed_api_consistency
    ]

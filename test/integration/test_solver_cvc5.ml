(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2026 formalsec *)
(* Written by the Smtml programmers *)

open Smtml
open Smtml_test_solver

let is_available () =
  Alcotest.(check bool) "cvc5 is_available" true Cvc5_mappings.is_available

let () =
  let module Cvc5_solv = Test_solver.Make (Cvc5_mappings) in
  Alcotest.run "cvc5"
    [ ("is_available", [ Alcotest.test_case "is_available" `Quick is_available ])
    ; Cvc5_solv.test_params
    ; Cvc5_solv.test_lia
    ; Cvc5_solv.test_bv
    ; Cvc5_solv.test_regexp
    ; Cvc5_solv.test_extract
    ; Cvc5_solv.test_typed_api_consistency
    ]

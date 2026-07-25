(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2026 formalsec *)
(* Written by the Smtml programmers *)

open Smtml
open Smtml_test_solver

let is_available () =
  Alcotest.(check bool) "Z3 is_available" true Z3_mappings.is_available

let () =
  let module Z3_opt = Test_optimizer.Make (Z3_mappings) in
  let module Z3_solv = Test_solver.Make (Z3_mappings) in
  let module Z3_bindings = Test_bindings.Make (Z3_mappings.M) in
  Alcotest.run "Z3"
    [ ("is_available", [ Alcotest.test_case "is_available" `Quick is_available ])
    ; Z3_opt.test
    ; Z3_solv.test_params
    ; Z3_solv.test_cached
    ; Z3_solv.test_lia
    ; Z3_solv.test_lra
    ; Z3_solv.test_bv
    ; ( "test_bv_ext_rotate"
      , [ Alcotest.test_case "test_bv_ext_rotate" `Quick
            (Z3_solv.with_solver Z3_solv.test_bv_ext_rotate)
        ] )
    ; Z3_solv.test_fp
    ; Z3_solv.test_regexp
    ; Z3_solv.test_uninterpreted
    ; Z3_bindings.test_adt
    ; Z3_solv.test_extract
    ; Z3_solv.test_typed_api_consistency
    ]

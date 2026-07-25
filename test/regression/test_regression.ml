(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2026 formalsec *)
(* Written by the Smtml programmers *)

(** Regression test suite runner. *)

let () =
  Alcotest.run "regression"
    [ ( "issue_655"
      , [ Alcotest.test_case "test_serialization" `Quick
            Test_issue_655.test_serialization
        ] )
    ; ( "issue_658"
      , [ Alcotest.test_case "test_exp_large" `Quick
            Test_issue_658.test_exp_large
        ; Alcotest.test_case "test_mul_large" `Quick
            Test_issue_658.test_mul_large
        ; Alcotest.test_case "test_add_large" `Quick
            Test_issue_658.test_add_large
        ; Alcotest.test_case "test_neg_large" `Quick
            Test_issue_658.test_neg_large
        ; Alcotest.test_case "test_abs_large" `Quick
            Test_issue_658.test_abs_large
        ; Alcotest.test_case "test_sub_large" `Quick
            Test_issue_658.test_sub_large
        ; Alcotest.test_case "test_div_large" `Quick
            Test_issue_658.test_div_large
        ; Alcotest.test_case "test_relop_large" `Quick
            Test_issue_658.test_relop_large
        ; Alcotest.test_case "test_pow_negative_exponent" `Quick
            Test_issue_658.test_pow_negative_exponent
        ; Alcotest.test_case "test_pow_overflow_exponent" `Quick
            Test_issue_658.test_pow_overflow_exponent
        ] )
    ; ( "issue_ext_rotate"
      , [ Alcotest.test_case "test_ext_rotl_huge_int_8" `Quick
            Test_issue_ext_rotate.test_ext_rotl_huge_int_8
        ; Alcotest.test_case "test_ext_rotl_neg_int_8" `Quick
            Test_issue_ext_rotate.test_ext_rotl_neg_int_8
        ; Alcotest.test_case "test_ext_rotr_huge_int_8" `Quick
            Test_issue_ext_rotate.test_ext_rotr_huge_int_8
        ; Alcotest.test_case "test_ext_rotr_neg_int_8" `Quick
            Test_issue_ext_rotate.test_ext_rotr_neg_int_8
        ; Alcotest.test_case "test_ext_rotl_eleven_int_8" `Quick
            Test_issue_ext_rotate.test_ext_rotl_eleven_int_8
        ; Alcotest.test_case "test_ext_rotr_eleven_int_8" `Quick
            Test_issue_ext_rotate.test_ext_rotr_eleven_int_8
        ; Alcotest.test_case "test_ext_rotl_huge_int_32" `Quick
            Test_issue_ext_rotate.test_ext_rotl_huge_int_32
        ; Alcotest.test_case "test_ext_rotr_huge_int_32" `Quick
            Test_issue_ext_rotate.test_ext_rotr_huge_int_32
        ; Alcotest.test_case "test_ext_rotl_bv_huge_8" `Quick
            Test_issue_ext_rotate.test_ext_rotl_bv_huge_8
        ; Alcotest.test_case "test_ext_rotr_bv_huge_8" `Quick
            Test_issue_ext_rotate.test_ext_rotr_bv_huge_8
        ; Alcotest.test_case "test_ext_rotl_matches_rotl" `Quick
            Test_issue_ext_rotate.test_ext_rotl_matches_rotl
        ; Alcotest.test_case "test_ext_rotr_matches_rotr" `Quick
            Test_issue_ext_rotate.test_ext_rotr_matches_rotr
        ] )
    ]

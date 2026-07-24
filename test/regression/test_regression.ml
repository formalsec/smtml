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
    ]

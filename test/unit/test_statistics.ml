(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2026 formalsec *)
(* Written by the Smtml programmers *)

open Smtml
module Map = Statistics.Map

let entry_testable = Alcotest.testable Statistics.pp_entry ( = )

let test_merge () =
  let s1 = Map.empty |> Map.add "time" (`Float 10.0) in
  let s2 = Map.empty |> Map.add "time" (`Float 20.0) in
  let stat_testable = Alcotest.(option entry_testable) in
  Alcotest.check stat_testable "test_merge"
    (Some (`Float 30.0))
    (Statistics.merge s1 s2 |> Map.find_opt "time")

let () =
  Alcotest.run "Statistics"
    [ ("Statistics", [ Alcotest.test_case "test_merge" `Quick test_merge ]) ]

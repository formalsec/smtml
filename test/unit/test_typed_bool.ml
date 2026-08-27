(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2026 formalsec *)
(* Written by the Smtml programmers *)

open Smtml

(* Hashconsing allows us to test for physical equality *)
let bool_testable = Alcotest.testable Typed.Bool.pp ( == )

let test_or_folding () =
  let t = Typed.Bool.true_ in
  let f = Typed.Bool.false_ in
  let s = Typed.const Typed.Types.bool "x" in

  let res1 = Typed.Bool.or_ f s in
  Alcotest.check bool_testable "false || s == s" s res1;

  let res2 = Typed.Bool.or_ s f in
  Alcotest.check bool_testable "s || false == s" s res2;

  let res3 = Typed.Bool.or_ t s in
  Alcotest.check bool_testable "true || s == true" t res3;

  let res4 = Typed.Bool.or_ s t in
  Alcotest.check bool_testable "s || true == true" t res4

let test_and_folding () =
  let t = Typed.Bool.true_ in
  let f = Typed.Bool.false_ in
  let s = Typed.const Typed.Types.bool "x" in

  let res1 = Typed.Bool.and_ t s in
  Alcotest.check bool_testable "true && s == s" s res1;

  let res2 = Typed.Bool.and_ s t in
  Alcotest.check bool_testable "s && true == s" s res2;

  let res3 = Typed.Bool.and_ f s in
  Alcotest.check bool_testable "false && s == false" f res3;

  let res4 = Typed.Bool.and_ s f in
  Alcotest.check bool_testable "s && false == false" f res4

let () =
  Alcotest.run "Typed Bool Folding"
    [ ( "folding"
      , [ Alcotest.test_case "test_or_folding" `Quick test_or_folding
        ; Alcotest.test_case "test_and_folding" `Quick test_and_folding
        ] )
    ]

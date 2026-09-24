(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2026 formalsec *)
(* Written by the Smtml programmers *)

open Smtml
open Smtml_test.Test_harness

let ty_testable = Alcotest.testable Ty.pp Ty.equal

let arr = Ty.Ty_array (Ty_int, Ty_bool)

let test_ty () =
  Alcotest.(check bool)
    "array(int, bool) <> array(bool, int)" false
    (Ty.equal arr (Ty_array (Ty_bool, Ty_int)));
  Alcotest.(check int)
    "hash" (Ty.hash arr)
    (Ty.hash (Ty_array (Ty_int, Ty_bool)))

let test_expr () =
  let open Infix in
  let a = symbol "a" arr in
  let store = Expr.triop arr Store a (int 0) true_ in
  let select = Expr.binop Ty_bool Select store (int 0) in
  Alcotest.check ty_testable "store" arr (Expr.ty store);
  Alcotest.check ty_testable "select" Ty_bool (Expr.ty select)

let test_typed () =
  let module A =
    Typed.Arrays.Make
      (struct
        type s = int

        let ty = Typed.Types.int
      end)
      (struct
        type s = bool

        let ty = Typed.Types.bool
      end)
  in
  Alcotest.check ty_testable "ty" arr A.ty

let () =
  Alcotest.run "Array unit tests"
    [ ( "test_array"
      , [ Alcotest.test_case "test_ty" `Quick test_ty
        ; Alcotest.test_case "test_expr" `Quick test_expr
        ; Alcotest.test_case "test_typed" `Quick test_typed
        ] )
    ]

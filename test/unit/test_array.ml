(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2026 formalsec *)
(* Written by the Smtml programmers *)

open Smtml
open Smtml_test.Test_harness

let ty_testable = Alcotest.testable Ty.pp Ty.equal

let value_testable = Alcotest.testable Value.pp Value.equal

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

let test_value () =
  let v =
    Value.array arr ~default:False
      [ (Value.Int (Z.of_int 2), True)
      ; (Value.Int (Z.of_int 1), True)
      ; (Value.Int (Z.of_int 0), False)
      ; (Value.Int (Z.of_int 1), False)
      ]
  in
  Alcotest.check value_testable "normalised"
    (Array
       { ty = arr
       ; default = False
       ; entries =
           [ (Value.Int (Z.of_int 1), True); (Value.Int (Z.of_int 2), True) ]
       } )
    v;
  (* The second binding of index 1 and the binding of 0 to false (the default
     value) are dropped *)
  Alcotest.check ty_testable "type_of" arr (Value.type_of v)

let test_eval () =
  let open Infix in
  let arr_v = Value.array arr ~default:False [ (Value.Int Z.one, True) ] in
  (* [select] and [store] on array values are constant folded *)
  check (Expr.binop Ty_bool Select (Expr.value arr_v) (int 1)) true_;
  check (Expr.binop Ty_bool Select (Expr.value arr_v) (int 2)) false_;
  let arr_v' = Expr.triop arr Store (Expr.value arr_v) (int 2) true_ in
  check arr_v'
    (Expr.value
       (Value.array arr ~default:False
          [ (Value.Int Z.one, True); (Value.Int (Z.of_int 2), True) ] ) );
  check (Expr.relop arr Eq (Expr.value arr_v) arr_v') false_

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
  Alcotest.check ty_testable "ty" arr (Typed.Types.to_ty A.ty)

let () =
  Alcotest.run "Array unit tests"
    [ ( "test_array"
      , [ Alcotest.test_case "test_ty" `Quick test_ty
        ; Alcotest.test_case "test_expr" `Quick test_expr
        ; Alcotest.test_case "test_value" `Quick test_value
        ; Alcotest.test_case "test_eval" `Quick test_eval
        ; Alcotest.test_case "test_typed" `Quick test_typed
        ] )
    ]

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
  begin match v with
  | Array { entries; _ } ->
    Alcotest.(check (list (pair value_testable value_testable)))
      "normalised"
      [ (Value.Int (Z.of_int 1), True); (Value.Int (Z.of_int 2), True) ]
      entries
  | _ -> assert false
  end;
  (* The second binding of index 1 and the binding of 0 to false (the default
     value) are dropped *)
  Alcotest.check ty_testable "type_of" arr (Value.type_of v);
  (* The outer binding of 1 to false (the default value) still shadows the inner
     one *)
  Alcotest.check value_testable "shadowed by the default"
    (Value.array arr ~default:False [ (Value.Int Z.zero, True) ])
    (Value.array arr ~default:False
       [ (Value.Int Z.zero, True)
       ; (Value.Int Z.one, False)
       ; (Value.Int Z.one, True)
       ] )

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

let test_equal () =
  let bb_arr_ty = Ty.Ty_array (Ty_bool, Ty_bool) in
  (* Different types, same default and bindings *)
  Alcotest.(check bool)
    "different types" false
    (Value.equal
       (Value.array bb_arr_ty ~default:False [])
       (Value.array arr ~default:False []) );
  (* Same default: entries are compared one by one *)
  let int i = Value.Int (Z.of_int i) in
  let a entries = Value.array arr ~default:False entries in
  Alcotest.check value_testable "same entries"
    (a [ (int 1, True); (int 2, True) ])
    (a [ (int 2, True); (int 1, True) ]);
  Alcotest.(check bool)
    "different entries" false
    (Value.equal (a [ (int 1, True) ]) (a [ (int 2, True) ]))

let test_array_indices () =
  let open Infix in
  let bb_arr_ty = Ty.Ty_array (Ty_bool, Ty_bool) in
  let outer_ty = Ty.Ty_array (bb_arr_ty, Ty_int) in
  let id = Value.array bb_arr_ty ~default:False [ (True, True) ] in
  let neg = Value.array bb_arr_ty ~default:False [ (False, True) ] in
  let outer =
    Value.array outer_ty ~default:(Int Z.zero)
      [ (id, Int Z.one); (neg, Int (Z.of_int 2)) ]
  in
  check (Expr.binop Ty_int Select (Expr.value outer) (Expr.value id)) (int 1);
  check (Expr.binop Ty_int Select (Expr.value outer) (Expr.value neg)) (int 2)

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
        ; Alcotest.test_case "test_equal" `Quick test_equal
        ; Alcotest.test_case "test_array_indices" `Quick test_array_indices
        ; Alcotest.test_case "test_typed" `Quick test_typed
        ] )
    ]

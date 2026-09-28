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

let test_eval_eq_finite_index () =
  let open Infix in
  let eq ty a b = Expr.relop ty Eq (Expr.value a) (Expr.value b) in
  let bb_arr_ty = Ty.Ty_array (Ty_bool, Ty_bool) in
  let const_true = Value.array bb_arr_ty ~default:True [] in
  (* Every index is bound, so the default is unused *)
  let full_rewrite =
    Value.array bb_arr_ty ~default:False [ (True, True); (False, True) ]
  in
  check (eq bb_arr_ty full_rewrite const_true) true_;
  check (eq Ty_bool full_rewrite const_true) true_;
  check
    (Expr.relop bb_arr_ty Ne (Expr.value full_rewrite) (Expr.value const_true))
    false_;
  let partial = Value.array bb_arr_ty ~default:False [ (True, True) ] in
  check (eq bb_arr_ty partial const_true) false_;
  let bv2 i = Value.Bitv (Bitvector.make (Z.of_int i) 2) in
  let arr_bv2 = Ty.Ty_array (Ty_bitv 2, Ty_bool) in
  check
    (eq arr_bv2
       (Value.array arr_bv2 ~default:True
          [ (bv2 0, False); (bv2 1, False); (bv2 2, False); (bv2 3, False) ] )
       (Value.array arr_bv2 ~default:False []) )
    true_;
  (* Infinite index type: different defaults always differ somewhere *)
  check
    (eq arr
       (Value.array arr ~default:True [ (Value.Int Z.zero, False) ])
       (Value.array arr ~default:False []) )
    false_

let test_semantic_equal () =
  let bb_arr_ty = Ty.Ty_array (Ty_bool, Ty_bool) in
  (* Both are the identity *)
  let id1 = Value.array bb_arr_ty ~default:False [ (True, True) ] in
  let id2 = Value.array bb_arr_ty ~default:True [ (False, False) ] in
  Alcotest.check value_testable "same function" id1 id2;
  Alcotest.(check int) "same hash" (Value.hash id1) (Value.hash id2);
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
  let const b = Value.array bb_arr_ty ~default:b [] in
  let id1 = Value.array bb_arr_ty ~default:False [ (True, True) ] in
  let id2 = Value.array bb_arr_ty ~default:True [ (False, False) ] in
  let neg = Value.array bb_arr_ty ~default:False [ (False, True) ] in
  (* [id1] and [id2] are the same index, so the second binding is dropped *)
  let outer =
    Value.array outer_ty ~default:(Int Z.zero)
      [ (id1, Int Z.one); (id2, Int (Z.of_int 2)) ]
  in
  check (Expr.binop Ty_int Select (Expr.value outer) (Expr.value id2)) (int 1);
  (* All 4 indices are bound to 1 *)
  Alcotest.check value_testable "all indices bound"
    (Value.array outer_ty ~default:(Int Z.one) [])
    (Value.array outer_ty ~default:(Int Z.zero)
       [ (id1, Int Z.one)
       ; (neg, Int Z.one)
       ; (const True, Int Z.one)
       ; (const False, Int Z.one)
       ] )

let test_cardinality () =
  let card = Alcotest.(option (testable Z.pp_print Z.equal)) in
  Alcotest.check card "bool" (Some (Z.of_int 2)) (Ty.cardinality Ty_bool);
  Alcotest.check card "int" None (Ty.cardinality Ty_int);
  Alcotest.check card "bv16"
    (Some (Z.shift_left Z.one 16))
    (Ty.cardinality (Ty_bitv 16));
  Alcotest.check card "bv17 is treated as infinite" None
    (Ty.cardinality (Ty_bitv 17));
  let bb_arr_ty = Ty.Ty_array (Ty_bool, Ty_bool) in
  Alcotest.check card "array(bool, bool)"
    (Some (Z.of_int 4))
    (Ty.cardinality bb_arr_ty);
  Alcotest.check card "array(array(bool, bool), bool)"
    (Some (Z.of_int 16))
    (Ty.cardinality (Ty_array (bb_arr_ty, Ty_bool)));
  Alcotest.check card "array(int, bool)" None (Ty.cardinality arr);
  Alcotest.check card "array(bv32, bv8) is treated as infinite" None
    (Ty.cardinality (Ty_array (Ty_bitv 32, Ty_bitv 8)));
  Alcotest.check card "array(bv5, bool) is treated as infinite" None
    (Ty.cardinality (Ty_array (Ty_bitv 5, Ty_bool)));
  Alcotest.check card "array(bool, bv16) is treated as infinite" None
    (Ty.cardinality (Ty_array (Ty_bool, Ty_bitv 16)))

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
        ; Alcotest.test_case "test_eval_eq_finite_index" `Quick
            test_eval_eq_finite_index
        ; Alcotest.test_case "test_semantic_equal" `Quick test_semantic_equal
        ; Alcotest.test_case "test_array_indices" `Quick test_array_indices
        ; Alcotest.test_case "test_cardinality" `Quick test_cardinality
        ; Alcotest.test_case "test_typed" `Quick test_typed
        ] )
    ]

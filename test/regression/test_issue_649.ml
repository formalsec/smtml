(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2026 formalsec *)
(* Written by the Smtml programmers *)

open Smtml

let int z = Typed.Int.v (Z.of_int z)

let bv i32 = Typed.Bitv32.v (Bitvector.of_int32 i32)

let equal f a b =
  let result = Typed.Unsafe.unwrap (f a b) in
  match Expr.view result with
  | Expr.Val True -> true
  | Val False -> false
  | _ -> Alcotest.failf "Unexpected result from Bool.eq: %a" Expr.pp result

let int_testable = Alcotest.testable Typed.Int.pp (equal Typed.Int.eq)

let bv_testable = Alcotest.testable Typed.Bitv32.pp (equal Typed.Bitv32.eq)

let test_int_mod () =
  let mod_ = Typed.Int.mod_ (int 4) (int 2) in
  Alcotest.check int_testable "4 mod 2 == 0" (int 0) mod_;

  let mod_ = Typed.Int.mod_ (int ~-7) (int 3) in
  Alcotest.check int_testable "-7 mod 3 == 2" (int 2) mod_;

  let mod_ = Typed.Int.mod_ (int 7) (int ~-3) in
  Alcotest.check int_testable "7 mod -3 == 1" (int 1) mod_;

  let mod_ = Typed.Int.mod_ (int ~-7) (int ~-3) in
  Alcotest.check int_testable "-7 mod -3 == 2" (int 2) mod_

let test_i32_mod () =
  let mod_ = Typed.Bitv32.smod (bv (-7l)) (bv 3l) in
  Alcotest.check bv_testable "-7 mod 3 == 2" (bv 2l) mod_;

  let mod_ = Typed.Bitv32.smod (bv 7l) (bv (-3l)) in
  Alcotest.check bv_testable "7 mod -3 == -2" (bv (-2l)) mod_;

  let mod_ = Typed.Bitv32.smod (bv (-7l)) (bv (-3l)) in
  Alcotest.check bv_testable "-7 mod -3 == -1" (bv (-1l)) mod_;

  let mod_ = Typed.Bitv32.smod (bv 7l) (bv 0l) in
  Alcotest.check bv_testable "7 mod 0 == 7" (bv 7l) mod_

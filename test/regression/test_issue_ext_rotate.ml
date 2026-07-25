(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2026 formalsec *)
(* Written by the Smtml programmers *)

(** Regression test: ext_rotate_left/ext_rotate_right constant folding must
    safely normalize oversized rotation counts via
    Bitvector.normalize_shift_amount before Z.to_int, avoiding Z.Overflow. *)

open Smtml

let intz z = Typed.Unsafe.wrap (Expr.value (Int z))

let bitv8_equal (a : Typed.Bitv8.t) (b : Typed.Bitv8.t) =
  let a' = Typed.Unsafe.unwrap a in
  let b' = Typed.Unsafe.unwrap b in
  match Expr.view (Expr.relop (Ty_bitv 8) Eq a' b') with
  | Expr.Val True -> true
  | Expr.Val False -> false
  | _ -> Alcotest.fail "non-constant bitv8 comparison"

let bitv32_equal (a : Typed.Bitv32.t) (b : Typed.Bitv32.t) =
  let a' = Typed.Unsafe.unwrap a in
  let b' = Typed.Unsafe.unwrap b in
  match Expr.view (Expr.relop (Ty_bitv 32) Eq a' b') with
  | Expr.Val True -> true
  | Expr.Val False -> false
  | _ -> Alcotest.fail "non-constant bitv32 comparison"

let bitv8_testable = Alcotest.testable Typed.Bitv8.pp bitv8_equal

let bitv32_testable = Alcotest.testable Typed.Bitv32.pp bitv32_equal

(* 2^100 — far exceeds OCaml int range *)
let huge = Z.(pow (of_int 2) 100)

let neg3 = Z.neg (Z.of_int 3)

(* -- ext_rotate_left via the typed API (constant-folding path) --- *)

let test_ext_rotl_huge_int_8 () =
  let x = Typed.Bitv8.of_int 0x36 in
  let shift = intz huge in
  let result = Typed.Bitv8.ext_rotate_left x shift in
  Alcotest.check bitv8_testable "rotl huge masked to 0"
    (Typed.Bitv8.of_int 0x36) result

let test_ext_rotl_neg_int_8 () =
  let x = Typed.Bitv8.of_int 0x36 in
  let shift = intz neg3 in
  let result = Typed.Bitv8.ext_rotate_left x shift in
  Alcotest.check bitv8_testable "rotl -3 masked to 5" (Typed.Bitv8.of_int 0xC6)
    result

let test_ext_rotr_huge_int_8 () =
  let x = Typed.Bitv8.of_int 0x36 in
  let shift = intz huge in
  let result = Typed.Bitv8.ext_rotate_right x shift in
  Alcotest.check bitv8_testable "rotr huge masked to 0"
    (Typed.Bitv8.of_int 0x36) result

let test_ext_rotr_neg_int_8 () =
  let x = Typed.Bitv8.of_int 0x36 in
  let shift = intz neg3 in
  let result = Typed.Bitv8.ext_rotate_right x shift in
  Alcotest.check bitv8_testable "rotr -3 masked to 5" (Typed.Bitv8.of_int 0xB1)
    result

(* --- oversize shift that masks to non-zero --- *)

let test_ext_rotl_eleven_int_8 () =
  let x = Typed.Bitv8.of_int 0x36 in
  let shift = intz (Z.of_int 11) in
  let result = Typed.Bitv8.ext_rotate_left x shift in
  Alcotest.check bitv8_testable "rotl 11 masked to 3" (Typed.Bitv8.of_int 0xB1)
    result

let test_ext_rotr_eleven_int_8 () =
  let x = Typed.Bitv8.of_int 0x36 in
  let shift = intz (Z.of_int 11) in
  let result = Typed.Bitv8.ext_rotate_right x shift in
  Alcotest.check bitv8_testable "rotr 11 masked to 3" (Typed.Bitv8.of_int 0xC6)
    result

(* --- 32-bit width --- *)

let test_ext_rotl_huge_int_32 () =
  let x = Typed.Bitv32.of_int 42 in
  let shift = intz huge in
  let result = Typed.Bitv32.ext_rotate_left x shift in
  Alcotest.check bitv32_testable "rotl huge masked to 0 on i32"
    (Typed.Bitv32.of_int 42) result

let test_ext_rotr_huge_int_32 () =
  let x = Typed.Bitv32.of_int 42 in
  let shift = intz huge in
  let result = Typed.Bitv32.ext_rotate_right x shift in
  Alcotest.check bitv32_testable "rotr huge masked to 0 on i32"
    (Typed.Bitv32.of_int 42) result

(* --- Bitvector-constant rotation amount (Val (Bitv _) path) --- *)

let test_ext_rotl_bv_huge_8 () =
  let x = Typed.Bitv8.of_int 0x36 in
  let shift = Typed.Bitv8.v (Bitvector.make huge 8) in
  let result = Typed.Bitv8.ext_rotate_left x shift in
  Alcotest.check bitv8_testable "rotl bv huge masked to 0"
    (Typed.Bitv8.of_int 0x36) result

let test_ext_rotr_bv_huge_8 () =
  let x = Typed.Bitv8.of_int 0x36 in
  let shift = Typed.Bitv8.v (Bitvector.make huge 8) in
  let result = Typed.Bitv8.ext_rotate_right x shift in
  Alcotest.check bitv8_testable "rotr bv huge masked to 0"
    (Typed.Bitv8.of_int 0x36) result

(* --- ext_rotate equals rotate_left/rotate_right for in-range n --- *)

let test_ext_rotl_matches_rotl () =
  let x = Typed.Bitv8.of_int 0x36 in
  let ext = Typed.Bitv8.ext_rotate_left x (Typed.Bitv8.of_int 3) in
  let rot = Typed.Bitv8.rotate_left 3 x in
  Alcotest.check bitv8_testable "ext_rotl 3 = rotl 3" rot ext

let test_ext_rotr_matches_rotr () =
  let x = Typed.Bitv8.of_int 0x36 in
  let ext = Typed.Bitv8.ext_rotate_right x (Typed.Bitv8.of_int 3) in
  let rot = Typed.Bitv8.rotate_right 3 x in
  Alcotest.check bitv8_testable "ext_rotr 3 = rotr 3" rot ext

(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2026 formalsec *)
(* Written by the Smtml programmers *)

open Smtml.Bitvector

let bitvector_testable =
  Alcotest.testable
    (fun fmt bv ->
      Fmt.pf fmt "{ v = %a; width = %d }" Z.pp_print (view bv) (numbits bv) )
    equal

let check bv1 bv2 =
  Alcotest.check bitvector_testable "bitvector equality" bv2 bv1

let z = Z.of_int

let test_make () =
  let bv = make (z 5) 8 in
  Alcotest.(check bool) "view" true (Z.equal (view bv) (z 5));
  Alcotest.(check int) "numbits" 8 (numbits bv)

let test_equal () =
  Alcotest.(check bool) "equal" true (equal (make (z 42) 8) (make (z 42) 8));
  Alcotest.(check bool)
    "not equal" false
    (equal (make (z 42) 8) (make (z 42) 16))

let test_eqz () =
  Alcotest.(check bool) "0 == 0" true (eqz (make Z.zero 8));
  Alcotest.(check bool) "42 != 0" false (eqz (make (z 42) 8))

let test_eq_one () =
  Alcotest.(check bool) "1 == 1" true (eq_one (make Z.one 8));
  Alcotest.(check bool) "42 != 1" false (eq_one (make (z 42) 8))

let test_neg () =
  let bv = make (z 5) 8 in
  check (neg bv) (make (z (-5)) 8)

let test_clz () =
  let bv = make (z 1) 8 in
  check (clz bv) (make (z 7) 8)

let test_ctz () =
  let bv = make (z 128) 8 in
  check (ctz bv) (make (z 7) 8)

let test_popcnt () =
  let bv = make (z 0b1010_1010) 8 in
  check (popcnt bv) (make (z 4) 8)

let test_add () =
  let bv1 = make (z 3) 8 in
  let bv2 = make (z 5) 8 in
  check (add bv1 bv2) (make (z 8) 8)

let test_sub () =
  let bv1 = make (z 10) 8 in
  let bv2 = make (z 3) 8 in
  check (sub bv1 bv2) (make (z 7) 8)

let test_mul () =
  let bv1 = make (z 4) 8 in
  let bv2 = make (z 3) 8 in
  check (mul bv1 bv2) (make (z 12) 8)

let test_div () =
  let bv1 = make (z 10) 8 in
  let bv2 = make (z 2) 8 in
  check (div bv1 bv2) (make (z 5) 8)

let test_div_u () =
  let bv1 = make (z 10) 8 in
  let bv2 = make (z 3) 8 in
  check (div_u bv1 bv2) (make (z (10 / 3)) 8)

let test_logical_ops () =
  let bv1 = make (z 0b1100) 4 in
  let bv2 = make (z 0b1010) 4 in
  check (logand bv1 bv2) (make (z 0b1000) 4);
  check (logor bv1 bv2) (make (z 0b1110) 4);
  check (logxor bv1 bv2) (make (z 0b0110) 4)

let test_shl () =
  let bv = make (z 0b0011) 4 in
  check (shl bv (make (z 1) 4)) (make (z 0b0110) 4);
  let bv = make (z 65475) 64 in
  let shift_count = make (z (-127)) 64 in
  let expected = make (z 130950) 64 in
  check (shl bv shift_count) expected

let test_lshr () =
  let bv = make (z 0b0011) 4 in
  check (lshr bv (make (z 1) 4)) (make (z 0b0001) 4);
  let bv = make (z (-4294967295)) 64 in
  let shift_count = make (z (-4294967295)) 64 in
  let expected = make (Z.of_string "9223372034707292160") 64 in
  check (lshr bv shift_count) expected

let test_ashr () =
  let bv = make (z 0b0011) 4 in
  check (ashr bv (make (z 1) 4)) (make (z 0b0001) 4);
  let bv = make (z 0) 64 in
  let shift_count = make (z (-327699)) 64 in
  let expected = make (z 0) 64 in
  check (ashr bv shift_count) expected

let test_comparisons () =
  let bv1 = make (z 3) 4 in
  let bv2 = make (z 5) 4 in
  Alcotest.(check bool) "3 < 5" true (lt bv1 bv2);
  Alcotest.(check bool) "3 <= 5" true (le bv1 bv2);
  Alcotest.(check bool) "5 > 3" true (gt bv2 bv1);
  Alcotest.(check bool) "5 >= 3" true (ge bv2 bv1);
  Alcotest.(check bool) "3 <_u 5" true (lt_u bv1 bv2);
  Alcotest.(check bool) "5 >_u 3" true (gt_u bv2 bv1)

let test_rotate_left_one () =
  let bv = make (z 0b1101) 4 in
  let one = make (z 1) 4 in
  check (rotate_left bv one) (make (z 0b1011) 4)

let test_rotate_left_negative () =
  let bv = make (z 0) 64 in
  let shift_count = make (z (-109)) 64 in
  let expected = make Z.zero 64 in
  check (rotate_left bv shift_count) expected

let test_rotate_right_one () =
  let bv = make (z 0b1101) 4 in
  let one = make (z 1) 4 in
  check (rotate_right bv one) (make (z 0b1110) 4)

let test_rotate_right_negative () =
  let bv = make Z.zero 64 in
  let shift_count = make (z (-5957114)) 64 in
  let expected = make Z.zero 64 in
  check (rotate_right bv shift_count) expected

let test_extensions () =
  let bv = make (z 0b1010) 4 in
  Alcotest.(check int) "zero extend numbits" 8 (numbits (zero_extend 4 bv));
  Alcotest.(check int) "sign extend numbits" 8 (numbits (sign_extend 4 bv))

let test_extract_i8 () =
  let bv = make (z 0x01) 8 in
  let extracted = extract bv ~high:7 ~low:0 in
  let expected = make (z 0b0000_0001) 8 in
  check expected extracted

let test_extract_i16_from_start () =
  let bv = make (z 0xABCD) 16 in
  let extracted = extract bv ~high:7 ~low:0 in
  let expected = make (z 0xCD) 8 in
  check expected extracted

let test_extract_i16_different_size () =
  let bv = make (Z.of_int 0xABCD) 16 in
  let extracted = extract bv ~high:11 ~low:4 in
  let expected = make (Z.of_int 0xBC) 8 in
  check expected extracted

let test_extract_i32_to_end () =
  let bv = of_int32 0x12345678l in
  let extracted = extract bv ~high:31 ~low:16 in
  let expected = make (z 0x1234) 16 in
  check expected extracted

let test_extract_i64_entire () =
  let bv = of_int64 0x1122334455667788L in
  let extracted = extract bv ~high:63 ~low:0 in
  let expected = of_int64 0x1122334455667788L in
  check expected extracted

let test_extract_i32_single_bit () =
  let bv = of_int32 0x12345678l in
  let extracted = extract bv ~high:5 ~low:5 in
  let expected = make (Z.of_int 0b1) 1 in
  check expected extracted

let test_extract_i64_single_bit_one () =
  let bv = of_int64 0x1122334455667788L in
  let extracted = extract bv ~high:63 ~low:63 in
  let expected = make (Z.of_int 0b0) 1 in
  check expected extracted

let test_concat_i8_i8 () =
  let a = of_int8 0b10101010 in
  let b = of_int8 0b11110000 in
  let concatenated = concat a b in
  let expected = make (Z.of_int 0xAAF0) 16 in
  check expected concatenated

let test_concat_i16_i16 () =
  let a = make (Z.of_int 0xABCD) 16 in
  let b = make (Z.of_int 0xEF12) 16 in
  let concatenated = concat a b in
  let expected = make (Z.of_int32 0xABCDEF12l) 32 in
  check expected concatenated

let test_concat_i32_i32 () =
  let a = of_int32 0x12345678l in
  let b = of_int32 0x9ABCDEF0l in
  let concatenated = concat a b in
  let expected = make (Z.of_string "0x123456789ABCDEF0") 64 in
  check expected concatenated

let test_concat_i64_i64 () =
  let a = of_int64 0x1122334455667788L in
  let b = of_int64 0x99AABBCCDDEEFF00L in
  let concatenated = concat a b in
  let expected = make (Z.of_string "0x112233445566778899AABBCCDDEEFF00") 128 in
  check expected concatenated

let test_concat_mixed_sizes () =
  let a = of_int8 0xFF in
  let b = make (Z.of_int 0xABCD) 16 in
  let c = of_int32 0x12345678l in
  let concatenated = concat (concat a b) c in
  let expected = make (Z.of_string "0xFFABCD12345678") (8 + 16 + 32) in
  check expected concatenated

let test_to_int32 () =
  let bv = make (Z.of_string "2441254434") 32 in
  let actual = to_int32 bv in
  let expected = -1853712862l in
  Alcotest.(check int32) "to_int32" expected actual

let () =
  Alcotest.run "Bitvector"
    [ ( "bitvector"
      , [ Alcotest.test_case "test_make" `Quick test_make
        ; Alcotest.test_case "test_equal" `Quick test_equal
        ; Alcotest.test_case "test_eqz" `Quick test_eqz
        ; Alcotest.test_case "test_eq_one" `Quick test_eq_one
        ; Alcotest.test_case "test_neg" `Quick test_neg
        ; Alcotest.test_case "test_clz" `Quick test_clz
        ; Alcotest.test_case "test_ctz" `Quick test_ctz
        ; Alcotest.test_case "test_popcnt" `Quick test_popcnt
        ; Alcotest.test_case "test_add" `Quick test_add
        ; Alcotest.test_case "test_sub" `Quick test_sub
        ; Alcotest.test_case "test_mul" `Quick test_mul
        ; Alcotest.test_case "test_div" `Quick test_div
        ; Alcotest.test_case "test_div_u" `Quick test_div_u
        ; Alcotest.test_case "test_logical_ops" `Quick test_logical_ops
        ; Alcotest.test_case "test_shl" `Quick test_shl
        ; Alcotest.test_case "test_lshr" `Quick test_lshr
        ; Alcotest.test_case "test_ashr" `Quick test_ashr
        ; Alcotest.test_case "test_comparisons" `Quick test_comparisons
        ; Alcotest.test_case "test_rotate_left_one" `Quick test_rotate_left_one
        ; Alcotest.test_case "test_rotate_left_negative" `Quick
            test_rotate_left_negative
        ; Alcotest.test_case "test_rotate_right_one" `Quick
            test_rotate_right_one
        ; Alcotest.test_case "test_rotate_right_negative" `Quick
            test_rotate_right_negative
        ; Alcotest.test_case "test_extensions" `Quick test_extensions
        ; Alcotest.test_case "test_extract_i8" `Quick test_extract_i8
        ; Alcotest.test_case "test_extract_i16_from_start" `Quick
            test_extract_i16_from_start
        ; Alcotest.test_case "test_extract_i16_different_size" `Quick
            test_extract_i16_different_size
        ; Alcotest.test_case "test_extract_i32_to_end" `Quick
            test_extract_i32_to_end
        ; Alcotest.test_case "test_extract_i64_entire" `Quick
            test_extract_i64_entire
        ; Alcotest.test_case "test_extract_i32_single_bit" `Quick
            test_extract_i32_single_bit
        ; Alcotest.test_case "test_extract_i64_single_bit" `Quick
            test_extract_i64_single_bit_one
        ; Alcotest.test_case "test_concat_i8_i8" `Quick test_concat_i8_i8
        ; Alcotest.test_case "test_concat_i16_i16" `Quick test_concat_i16_i16
        ; Alcotest.test_case "test_concat_i32_i32" `Quick test_concat_i32_i32
        ; Alcotest.test_case "test_concat_i64_i64" `Quick test_concat_i64_i64
        ; Alcotest.test_case "test_concat_mixed_sizes" `Quick
            test_concat_mixed_sizes
        ; Alcotest.test_case "test_to_int32" `Quick test_to_int32
        ] )
    ]

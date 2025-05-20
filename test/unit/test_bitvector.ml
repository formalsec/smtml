open OUnit2
open Smtml.Bitvector

let check =
  let pp_diff fmt (a, b) =
    Fmt.pf fmt
      "Got: { v = %a; width = %d } but expected: { v = %a; width = %d }"
      Z.pp_print (view a) (numbits a) Z.pp_print (view b) (numbits b)
  in
  assert_equal ~cmp:equal ~pp_diff

let z n = Z.of_int n (* Helper to create Z.t values *)

let test_make _ =
  let bv = make (z 5) 8 in
  assert_equal (view bv) (z 5);
  assert_equal (numbits bv) 8

let test_equal _ =
  assert (equal (make (z 42) 8) (make (z 42) 8));
  assert (not (equal (make (z 42) 8) (make (z 42) 16)))

let test_eqz _ =
  assert_bool "0 == 0" (eqz (make Z.zero 8));
  assert_bool "42 != 0" (not (eqz (make (z 42) 8)))

let test_eq_one _ =
  assert_bool "1 == 1" (eq_one (make Z.one 8));
  assert_bool "42 != 1" (not (eq_one (make (z 42) 8)))

let test_neg _ =
  let bv = make (z 5) 8 in
  check (neg bv) (make (z (-5)) 8)

let test_clz _ =
  let bv = make (z 1) 8 in
  check (clz bv) (make (z 7) 8)

let test_ctz _ =
  let bv = make (z 128) 8 in
  check (ctz bv) (make (z 7) 8)

let test_popcnt _ =
  let bv = make (z 0b1010_1010) 8 in
  check (popcnt bv) (make (z 4) 8)

let test_add _ =
  let bv1 = make (z 3) 8 in
  let bv2 = make (z 5) 8 in
  check (add bv1 bv2) (make (z 8) 8)

let test_sub _ =
  let bv1 = make (z 10) 8 in
  let bv2 = make (z 3) 8 in
  check (sub bv1 bv2) (make (z 7) 8)

let test_mul _ =
  let bv1 = make (z 4) 8 in
  let bv2 = make (z 3) 8 in
  check (mul bv1 bv2) (make (z 12) 8)

let test_div _ =
  let bv1 = make (z 10) 8 in
  let bv2 = make (z 2) 8 in
  check (div bv1 bv2) (make (z 5) 8)

let test_div_u _ =
  let bv1 = make (z 10) 8 in
  let bv2 = make (z 3) 8 in
  check (div_u bv1 bv2) (make (z (10 / 3)) 8)

let test_logical_ops _ =
  let bv1 = make (z 0b1100) 4 in
  let bv2 = make (z 0b1010) 4 in
  check (logand bv1 bv2) (make (z 0b1000) 4);
  check (logor bv1 bv2) (make (z 0b1110) 4);
  check (logxor bv1 bv2) (make (z 0b0110) 4)

let test_shl _ =
  let bv = make (z 0b0011) 4 in
  check (shl bv (make (z 1) 4)) (make (z 0b0110) 4);
  let bv = make (z 65475) 64 in
  let shift_count = make (z (-127)) 64 in
  let expected = make (z 130950) 64 in
  check (shl bv shift_count) expected

let test_lshr _ =
  let bv = make (z 0b0011) 4 in
  check (lshr bv (make (z 1) 4)) (make (z 0b0001) 4);
  let bv = make (z (-4294967295)) 64 in
  let shift_count = make (z (-4294967295)) 64 in
  let expected = make (Z.of_string "9223372034707292160") 64 in
  check (lshr bv shift_count) expected

let test_ashr _ =
  let bv = make (z 0b0011) 4 in
  check (ashr bv (make (z 1) 4)) (make (z 0b0001) 4);
  let bv = make (z 0) 64 in
  let shift_count = make (z (-327699)) 64 in
  let expected = make (z 0) 64 in
  check (ashr bv shift_count) expected

let test_comparisons _ =
  let bv1 = make (z 3) 4 in
  let bv2 = make (z 5) 4 in
  assert_bool "3 < 5" (lt bv1 bv2);
  assert_bool "3 <= 5" (le bv1 bv2);
  assert_bool "5 > 3" (gt bv2 bv1);
  assert_bool "5 >= 3" (ge bv2 bv1);
  assert_bool "3 <_u 5" (lt_u bv1 bv2);
  assert_bool "5 >_u 3" (gt_u bv2 bv1)

let test_rotate_left_one _ =
  let bv = make (z 0b1101) 4 in
  let one = make (z 1) 4 in
  check (ext_rotate_left bv one) (make (z 0b1011) 4)

let test_rotate_left_negative _ =
  let bv = make (z 0) 64 in
  let shift_count = make (z (-109)) 64 in
  let expected = make Z.zero 64 in
  check (ext_rotate_left bv shift_count) expected

let test_rotate_right_one _ =
  let bv = make (z 0b1101) 4 in
  let one = make (z 1) 4 in
  check (ext_rotate_right bv one) (make (z 0b1110) 4)

let test_rotate_right_negative _ =
  let bv = make Z.zero 64 in
  let shift_count = make (z (-5957114)) 64 in
  let expected = make Z.zero 64 in
  check (ext_rotate_right bv shift_count) expected

let test_extensions _ =
  let bv = make (z 0b1010) 4 in
  assert_equal (numbits (zero_extend 4 bv)) 8;
  assert_equal (numbits (sign_extend 4 bv)) 8

let test_extract_i8 _ =
  let bv = make (z 0x01) 8 in
  let extracted = extract bv ~high:7 ~low:0 in
  let expected = make (z 0b0000_0001) 8 in
  check expected extracted

let test_extract_i16_from_start _ =
  let bv = make (z 0xABCD) 16 in
  let extracted = extract bv ~high:7 ~low:0 in
  let expected = make (z 0xCD) 8 in
  check expected extracted

let test_extract_i16_different_size _ =
  let bv = make (Z.of_int 0xABCD) 16 in
  let extracted = extract bv ~high:11 ~low:4 in
  let expected = make (Z.of_int 0xBC) 8 in
  check expected extracted

let test_extract_i32_to_end _ =
  let bv = of_int32 0x12345678l in
  let extracted = extract bv ~high:31 ~low:16 in
  let expected = make (z 0x1234) 16 in
  check expected extracted

let test_extract_i64_entire _ =
  let bv = of_int64 0x1122334455667788L in
  let extracted = extract bv ~high:63 ~low:0 in
  let expected = of_int64 0x1122334455667788L in
  check expected extracted

let test_extract_i32_single_bit _ =
  let bv = of_int32 0x12345678l in
  let extracted = extract bv ~high:5 ~low:5 in
  let expected = make (Z.of_int 0b1) 1 in
  check expected extracted

let test_extract_i64_single_bit_one _ =
  let bv = of_int64 0x1122334455667788L in
  let extracted = extract bv ~high:63 ~low:63 in
  let expected = make (Z.of_int 0b0) 1 in
  check expected extracted

let test_extract =
  [ "test_extract_i8" >:: test_extract_i8
  ; "test_extract_i16_from_start" >:: test_extract_i16_from_start
  ; "test_extract_i16_different_size" >:: test_extract_i16_different_size
  ; "test_extract_i32_to_end" >:: test_extract_i32_to_end
  ; "test_extract_i64_entire" >:: test_extract_i64_entire
  ; "test_extract_i32_single_bit" >:: test_extract_i32_single_bit
  ; "test_extract_i64_single_bit" >:: test_extract_i64_single_bit_one
  ]

let test_concat_i8_i8 _ =
  let a = of_int8 0b10101010 in
  let b = of_int8 0b11110000 in
  let concatenated = concat a b in
  let expected = make (Z.of_int 0xAAF0) 16 in
  check expected concatenated

let test_concat_i16_i16 _ =
  let a = make (Z.of_int 0xABCD) 16 in
  let b = make (Z.of_int 0xEF12) 16 in
  let concatenated = concat a b in
  let expected = make (Z.of_int32 0xABCDEF12l) 32 in
  check expected concatenated

let test_concat_i32_i32 _ =
  let a = of_int32 0x12345678l in
  let b = of_int32 0x9ABCDEF0l in
  let concatenated = concat a b in
  let expected = make (Z.of_string "0x123456789ABCDEF0") 64 in
  check expected concatenated

let test_concat_i64_i64 _ =
  let a = of_int64 0x1122334455667788L in
  let b = of_int64 0x99AABBCCDDEEFF00L in
  let concatenated = concat a b in
  let expected = make (Z.of_string "0x112233445566778899AABBCCDDEEFF00") 128 in
  check expected concatenated

let test_concat_mixed_sizes _ =
  let a = of_int8 0xFF in
  let b = make (Z.of_int 0xABCD) 16 in
  let c = of_int32 0x12345678l in
  let concatenated = concat (concat a b) c in
  let expected = make (Z.of_string "0xFFABCD12345678") (8 + 16 + 32) in
  check expected concatenated

let test_concat =
  [ "test_concat_i8_i8" >:: test_concat_i8_i8
  ; "test_concat_i16_i16" >:: test_concat_i16_i16
  ; "test_concat_i32_i32" >:: test_concat_i32_i32
  ; "test_concat_i64_i64" >:: test_concat_i64_i64
  ; "test_concat_mixed_sizes" >:: test_concat_mixed_sizes
  ]

let test_to_int32 _ =
  let bv = make (Z.of_string "2441254434") 32 in
  let actual = to_int32 bv in
  let expected = -1853712862l in
  assert_equal expected actual

let test_to_int64 _ =
  (* TODO *)
  ()

let test_suite =
  "Bitvector"
  >::: [ "test_make" >:: test_make
       ; "test_equal" >:: test_equal
       ; "test_eqz" >:: test_eqz
       ; "test_eq_one" >:: test_eq_one
       ; "test_neg" >:: test_neg
       ; "test_clz" >:: test_clz
       ; "test_ctz" >:: test_ctz
       ; "test_popcn" >:: test_popcnt
       ; "test_add" >:: test_add
       ; "test_sub" >:: test_sub
       ; "test_mul" >:: test_mul
       ; "test_div" >:: test_div
       ; "test_div_u" >:: test_div_u
       ; "test_logical_ops" >:: test_logical_ops
       ; "test_shl" >:: test_shl
       ; "test_lshr" >:: test_lshr
       ; "test_ashr" >:: test_ashr
       ; "test_comparisons" >:: test_comparisons
       ; "test_rotate_left_one" >:: test_rotate_left_one
       ; "test_rotate_left_negative" >:: test_rotate_left_negative
       ; "test_rotate_right_one" >:: test_rotate_right_one
       ; "test_rotate_right_negative" >:: test_rotate_right_negative
       ; "test_extensions" >:: test_extensions
       ; "test_extract" >::: test_extract
       ; "test_concat" >::: test_concat
       ; "test_to_int32" >:: test_to_int32
       ; "test_to_int64" >:: test_to_int64
       ]

let () = run_test_tt_main test_suite

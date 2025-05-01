open OUnit2
open Smtml.Bitvector

let check =
  let pp_diff fmt (a, b) =
    Fmt.pf fmt "{ v = %a; width = %d } != { v = %a; width = %d }" Z.pp_print
      (view a) (numbits a) Z.pp_print (view b) (numbits b)
  in
  assert_equal ~cmp:equal ~pp_diff

let z n = Z.of_int n (* Helper to create Z.t values *)

let test_make _ =
  let bv = make (z 5) 8 in
  assert_equal (view bv) (z 5);
  assert_equal (numbits bv) 8

let test_neg _ =
  let bv = make (z 5) 8 in
  assert (equal (neg bv) (make (z (-5)) 8))

let test_add _ =
  let bv1 = make (z 3) 8 in
  let bv2 = make (z 5) 8 in
  assert_equal (view (add bv1 bv2)) (z 8)

let test_sub _ =
  let bv1 = make (z 10) 8 in
  let bv2 = make (z 3) 8 in
  assert_equal (view (sub bv1 bv2)) (z 7)

let test_mul _ =
  let bv1 = make (z 4) 8 in
  let bv2 = make (z 3) 8 in
  assert_equal (view (mul bv1 bv2)) (z 12)

let test_div _ =
  let bv1 = make (z 10) 8 in
  let bv2 = make (z 2) 8 in
  assert_equal (view (div bv1 bv2)) (z 5)

let test_div_u _ =
  let bv1 = make (z 10) 8 in
  let bv2 = make (z 3) 8 in
  assert_equal (view (div_u bv1 bv2)) (z (10 / 3))

let test_logical_ops _ =
  let bv1 = make (z 0b1100) 4 in
  let bv2 = make (z 0b1010) 4 in
  assert_equal (view (logand bv1 bv2)) (z 0b1000);
  assert_equal (view (logor bv1 bv2)) (z 0b1110);
  assert_equal (view (logxor bv1 bv2)) (z 0b0110)

let test_shifts _ =
  let bv = make (z 0b0011) 4 in
  assert_equal (view (shl bv (make (z 1) 4))) (z 0b0110);
  assert_equal (view (lshr bv (make (z 1) 4))) (z 0b0001);
  assert_equal (view (ashr bv (make (z 1) 4))) (z 0b0001)

let test_comparisons _ =
  let bv1 = make (z 3) 4 in
  let bv2 = make (z 5) 4 in
  assert (lt bv1 bv2);
  assert (le bv1 bv2);
  assert (gt bv2 bv1);
  assert (ge bv2 bv1);
  assert (lt_u bv1 bv2);
  assert (gt_u bv2 bv1)

let test_rotate _ =
  let bv = make (z 0b1101) 4 in
  let one = make (z 1) 4 in
  assert_equal (view (rotate_left bv one)) (z 0b1011);
  assert_equal (view (rotate_right bv one)) (z 0b1110)

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

let test_suite =
  "Bitvector"
  >::: [ "test_make" >:: test_make
       ; "test_neg" >:: test_neg
       ; "test_add" >:: test_add
       ; "test_sub" >:: test_sub
       ; "test_mul" >:: test_mul
       ; "test_div" >:: test_div
       ; "test_div_u" >:: test_div_u
       ; "test_logical_ops" >:: test_logical_ops
       ; "test_shifts" >:: test_shifts
       ; "test_comparisons" >:: test_comparisons
       ; "test_rotate" >:: test_rotate
       ; "test_extensions" >:: test_extensions
       ; "test_extract" >::: test_extract
       ; "test_concat" >::: test_concat
       ]

let () = run_test_tt_main test_suite

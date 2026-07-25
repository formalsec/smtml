(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2026 formalsec *)
(* Written by the Smtml programmers *)

open Smtml

let true_ = Value.True

let false_ = Value.False

let int x = Value.Int x

let str x = Value.Str x

let real x = Value.Real x

let int8 x = Value.Bitv (Bitvector.of_int8 x)

let int32 x = Value.Bitv (Bitvector.of_int32 x)

let int64 x = Value.Bitv (Bitvector.of_int64 x)

let float32 x = Value.Num (F32 (Int32.bits_of_float x))

let float64 x = Value.Num (F64 (Int64.bits_of_float x))

let value_testable = Alcotest.testable Value.pp Value.equal

let check_value = Alcotest.check value_testable

let assert_type_error f =
  try
    f ();
    Alcotest.fail
      "Expected Eval_error(`Type_error _) but function returned normally"
  with Eval.Eval_error (`Type_error _) -> ()

module Int_test = struct
  let unop =
    let test_neg () =
      let result = Eval.unop Ty_int Neg (int 5) in
      check_value "test_neg" (int (-5)) result
    in
    let test_lognot () =
      let result = Eval.unop Ty_int Not (int ~-1) in
      check_value "test_lognot" (int 0) result
    in
    let test_abs () =
      let result = Eval.unop Ty_int Abs (int (-7)) in
      check_value "test_abs" (int 7) result
    in
    let test_type_error () =
      assert_type_error @@ fun () -> ignore @@ Eval.unop Ty_int Neg (str "hi")
    in
    [ Alcotest.test_case "test_neg" `Quick test_neg
    ; Alcotest.test_case "test_not" `Quick test_lognot
    ; Alcotest.test_case "test_abs" `Quick test_abs
    ; Alcotest.test_case "test_unop_type_error" `Quick test_type_error
    ]

  let binop =
    let test_add () =
      check_value "test_add" (int 5) (Eval.binop Ty_int Add (int 2) (int 3))
    in
    let test_sub () =
      check_value "test_sub" (int 1) (Eval.binop Ty_int Sub (int 3) (int 2))
    in
    let test_mul () =
      check_value "test_mul" (int 9) (Eval.binop Ty_int Mul (int 3) (int 3))
    in
    let test_div () =
      check_value "test_div" (int 2) (Eval.binop Ty_int Div (int 6) (int 3))
    in
    let test_divide_by_zero () =
      Alcotest.check_raises "test_divide_by_zero" Division_by_zero (fun () ->
        let _ = Eval.binop Ty_int Div (int 1) (int 0) in
        () )
    in
    let test_rem () =
      check_value "test_rem" (int 0) (Eval.binop Ty_int Rem (int 6) (int 3))
    in
    let test_pow () =
      check_value "test_pow" (int 8) (Eval.binop Ty_int Pow (int 2) (int 3))
    in
    let test_min_max () =
      let a = int 42 in
      let b = int 1337 in
      check_value "test_max" (int 1337) (Eval.binop Ty_int Max a b);
      check_value "test_min" (int 42) (Eval.binop Ty_int Min a b)
    in
    let test_logical_ops () =
      let a = int 0b1100 in
      let b = int 0b1010 in
      check_value "test_and" (int 8) (Eval.binop Ty_int And a b);
      check_value "test_or" (int 14) (Eval.binop Ty_int Or a b);
      check_value "test_xor" (int 6) (Eval.binop Ty_int Xor a b)
    in
    let test_shifts () =
      let i = int 0b0011 in
      check_value "test_shl" (int 0b0110) (Eval.binop Ty_int Shl i (int 1));
      check_value "test_shrl" (int 0b0001) (Eval.binop Ty_int ShrL i (int 1));
      check_value "test_shra" (int 0b0001) (Eval.binop Ty_int ShrA i (int 1))
    in
    [ Alcotest.test_case "test_add" `Quick test_add
    ; Alcotest.test_case "test_sub" `Quick test_sub
    ; Alcotest.test_case "test_mul" `Quick test_mul
    ; Alcotest.test_case "test_div" `Quick test_div
    ; Alcotest.test_case "test_divide_by_zero" `Quick test_divide_by_zero
    ; Alcotest.test_case "test_rem" `Quick test_rem
    ; Alcotest.test_case "test_pow" `Quick test_pow
    ; Alcotest.test_case "test_min_max" `Quick test_min_max
    ; Alcotest.test_case "test_logical_ops" `Quick test_logical_ops
    ; Alcotest.test_case "test_shifts" `Quick test_shifts
    ]

  let relop =
    let test_lt () =
      Alcotest.(check bool) "2 < 3" true (Eval.relop Ty_int Lt (int 2) (int 3))
    in
    let test_le () =
      Alcotest.(check bool) "2 <= 3" true (Eval.relop Ty_int Le (int 3) (int 3))
    in
    let test_gt () =
      Alcotest.(check bool) "3 < 4" true (Eval.relop Ty_int Lt (int 3) (int 4))
    in
    let test_ge () =
      Alcotest.(check bool) "4 <= 4" true (Eval.relop Ty_int Le (int 4) (int 4))
    in
    [ Alcotest.test_case "test_lt" `Quick test_lt
    ; Alcotest.test_case "test_le" `Quick test_le
    ; Alcotest.test_case "test_gt" `Quick test_gt
    ; Alcotest.test_case "test_ge" `Quick test_ge
    ]

  let cvtop =
    let test_of_bool () =
      check_value "test_of_bool" (int 1) (Eval.cvtop Ty_int OfBool True)
    in
    let test_reinterpret_float () =
      check_value "test_reinterpret_float" (int 42)
        (Eval.cvtop Ty_int Reinterpret_float (real 42.0))
    in
    [ Alcotest.test_case "test_of_bool" `Quick test_of_bool
    ; Alcotest.test_case "test_reinterpret_float" `Quick test_reinterpret_float
    ]
end

module Real_test = struct
  let unop =
    let test_neg () =
      check_value "test_neg" (real (-5.)) (Eval.unop Ty_real Neg (real 5.))
    in
    let test_abs () =
      check_value "test_abs" (real 7.) (Eval.unop Ty_real Abs (real (-7.)))
    in
    let test_sqrt () =
      check_value "test_sqrt" (real 3.) (Eval.unop Ty_real Sqrt (real 9.))
    in
    let test_nearest () =
      check_value "test_nearest_1" (real 4.)
        (Eval.unop Ty_real Nearest (real 4.2));
      check_value "test_nearest_2" (real 5.)
        (Eval.unop Ty_real Nearest (real 4.6))
    in
    let test_ceil () =
      check_value "test_ceil" (real 5.) (Eval.unop Ty_real Ceil (real 4.2))
    in
    let test_floor () =
      check_value "test_floor" (real 4.) (Eval.unop Ty_real Floor (real 4.2))
    in
    let test_trunc () =
      check_value "test_trunc" (real 3.)
        (Eval.unop Ty_real Trunc (real Float.pi))
    in
    let test_is_nan () =
      check_value "test_is_nan_nan" true_
        (Eval.unop Ty_real Is_nan (real Float.nan));
      check_value "test_is_nan_42" false_ (Eval.unop Ty_real Is_nan (real 42.))
    in
    let test_type_error () =
      assert_type_error @@ fun () -> ignore @@ Eval.unop Ty_real Neg (str "hi")
    in
    [ Alcotest.test_case "test_neg" `Quick test_neg
    ; Alcotest.test_case "test_abs" `Quick test_abs
    ; Alcotest.test_case "test_sqrt" `Quick test_sqrt
    ; Alcotest.test_case "test_nearest" `Quick test_nearest
    ; Alcotest.test_case "test_ceil" `Quick test_ceil
    ; Alcotest.test_case "test_floor" `Quick test_floor
    ; Alcotest.test_case "test_trunc" `Quick test_trunc
    ; Alcotest.test_case "test_is_nan" `Quick test_is_nan
    ; Alcotest.test_case "test_unop_type_error" `Quick test_type_error
    ]

  let binop =
    let test_add () =
      check_value "test_add" (real 5.)
        (Eval.binop Ty_real Add (real 2.) (real 3.))
    in
    let test_sub () =
      check_value "test_sub" (real 1.)
        (Eval.binop Ty_real Sub (real 3.) (real 2.))
    in
    let test_mul () =
      check_value "test_mul" (real 9.)
        (Eval.binop Ty_real Mul (real 3.) (real 3.))
    in
    let test_div () =
      check_value "test_div" (real 2.)
        (Eval.binop Ty_real Div (real 6.) (real 3.))
    in
    let test_divide_by_zero () =
      check_value "test_divide_by_zero" (real Float.infinity)
        (Eval.binop Ty_real Div (real 1.) (real 0.))
    in
    let test_rem () =
      check_value "test_rem" (real 0.)
        (Eval.binop Ty_real Rem (real 6.) (real 3.))
    in
    let test_pow () =
      check_value "test_pow" (real 8.)
        (Eval.binop Ty_real Pow (real 2.) (real 3.))
    in
    let test_min_max () =
      let a = real 42. in
      let b = real 1337. in
      check_value "test_max" (real 1337.) (Eval.binop Ty_real Max a b);
      check_value "test_min" (real 42.) (Eval.binop Ty_real Min a b)
    in
    [ Alcotest.test_case "test_add" `Quick test_add
    ; Alcotest.test_case "test_sub" `Quick test_sub
    ; Alcotest.test_case "test_mul" `Quick test_mul
    ; Alcotest.test_case "test_div" `Quick test_div
    ; Alcotest.test_case "test_divide_by_zero" `Quick test_divide_by_zero
    ; Alcotest.test_case "test_rem" `Quick test_rem
    ; Alcotest.test_case "test_pow" `Quick test_pow
    ; Alcotest.test_case "test_min_max" `Quick test_min_max
    ]

  let relop =
    let test_eq () =
      Alcotest.(check bool)
        "0 = 0" true
        (Eval.relop Ty_real Eq (real 0.0) (real 0.0));
      Alcotest.(check bool)
        "nan != nan" true
        (not (Eval.relop Ty_real Eq (real Float.nan) (real Float.nan)))
    in
    let test_ne () =
      Alcotest.(check bool)
        "0 != 0" true
        (not (Eval.relop Ty_real Ne (real 0.0) (real 0.0)));
      Alcotest.(check bool)
        "nan != nan" true
        (Eval.relop Ty_real Ne (real Float.nan) (real Float.nan))
    in
    let test_lt () =
      Alcotest.(check bool)
        "2 < 3" true
        (Eval.relop Ty_real Lt (real 2.) (real 3.))
    in
    let test_le () =
      Alcotest.(check bool)
        "3 <= 3" true
        (Eval.relop Ty_real Le (real 3.) (real 3.))
    in
    let test_gt () =
      Alcotest.(check bool)
        "3 < 4" true
        (Eval.relop Ty_real Lt (real 3.) (real 4.))
    in
    let test_ge () =
      Alcotest.(check bool)
        "4 <= 4" true
        (Eval.relop Ty_real Le (real 4.) (real 4.))
    in
    [ Alcotest.test_case "test_eq" `Quick test_eq
    ; Alcotest.test_case "test_ne" `Quick test_ne
    ; Alcotest.test_case "test_lt" `Quick test_lt
    ; Alcotest.test_case "test_le" `Quick test_le
    ; Alcotest.test_case "test_gt" `Quick test_gt
    ; Alcotest.test_case "test_ge" `Quick test_ge
    ]

  let cvtop =
    let test_of_string () =
      check_value "test_of_string" (real 42.)
        (Eval.cvtop Ty_real OfString (str "42."))
    in
    let test_to_string () =
      check_value "test_to_string" (str "42.")
        (Eval.cvtop Ty_real ToString (real 42.))
    in
    let test_of_string_error () =
      Alcotest.check_raises "test_of_string_error"
        (Eval.Eval_error `Invalid_format_conversion) (fun () ->
        let _ = Eval.cvtop Ty_real OfString (str "not_a_real") in
        () )
    in
    let test_reinterpret_int () =
      check_value "test_reinterpret_int" (real 42.)
        (Eval.cvtop Ty_real Reinterpret_int (int 42))
    in
    let test_reinterpret_float () =
      check_value "test_reinterpret_float" (int 42)
        (Eval.cvtop Ty_real Reinterpret_float (real 42.))
    in
    [ Alcotest.test_case "test_to_string" `Quick test_to_string
    ; Alcotest.test_case "test_of_string" `Quick test_of_string
    ; Alcotest.test_case "test_of_string_error" `Quick test_of_string_error
    ; Alcotest.test_case "test_reinterpret_int" `Quick test_reinterpret_int
    ; Alcotest.test_case "test_reinterpret_float" `Quick test_reinterpret_float
    ]
end

module Bool_test = struct
  let unop =
    let test_not () =
      check_value "test_not" false_ (Eval.unop Ty_bool Not true_)
    in
    let test_type_error () =
      assert_type_error @@ fun () ->
      ignore @@ Eval.unop Ty_bool Not (str "false")
    in
    [ Alcotest.test_case "test_not" `Quick test_not
    ; Alcotest.test_case "test_type_error" `Quick test_type_error
    ]

  let binop =
    let test_and () =
      check_value "and_tt" true_ (Eval.binop Ty_bool And true_ true_);
      check_value "and_tf" false_ (Eval.binop Ty_bool And true_ false_);
      check_value "and_ft" false_ (Eval.binop Ty_bool And false_ true_);
      check_value "and_ff" false_ (Eval.binop Ty_bool And false_ false_)
    in
    let test_or () =
      check_value "or_tt" true_ (Eval.binop Ty_bool Or true_ true_);
      check_value "or_tf" true_ (Eval.binop Ty_bool Or true_ false_);
      check_value "or_ft" true_ (Eval.binop Ty_bool Or false_ true_);
      check_value "or_ff" false_ (Eval.binop Ty_bool Or false_ false_)
    in
    let test_xor () =
      check_value "xor_tt" false_ (Eval.binop Ty_bool Xor true_ true_);
      check_value "xor_tf" true_ (Eval.binop Ty_bool Xor true_ false_);
      check_value "xor_ft" true_ (Eval.binop Ty_bool Xor false_ true_);
      check_value "xor_ff" false_ (Eval.binop Ty_bool Xor false_ false_)
    in
    let test_implies () =
      check_value "impl_tt" true_ (Eval.binop Ty_bool Implies true_ true_);
      check_value "impl_tf" false_ (Eval.binop Ty_bool Implies true_ false_);
      check_value "impl_ft" true_ (Eval.binop Ty_bool Implies false_ true_);
      check_value "impl_ff" true_ (Eval.binop Ty_bool Implies false_ false_)
    in
    [ Alcotest.test_case "test_and" `Quick test_and
    ; Alcotest.test_case "test_or" `Quick test_or
    ; Alcotest.test_case "test_xor" `Quick test_xor
    ; Alcotest.test_case "test_implies" `Quick test_implies
    ]

  let triop =
    let test () =
      check_value "ite_true" (int 1)
        (Eval.triop Ty_bool Ite true_ (int 1) (int 0));
      check_value "ite_false" (int 0)
        (Eval.triop Ty_bool Ite false_ (int 1) (int 0))
    in
    Alcotest.test_case "test_ite" `Quick test

  let relop =
    let test_eq () =
      Alcotest.(check bool) "0 = 0" true (Eval.relop Ty_bool Eq (int 0) (int 0));
      Alcotest.(check bool)
        "\"abc\" = \"abc\"" true
        (Eval.relop Ty_bool Eq (str "abc") (str "abc"));
      Alcotest.(check bool)
        "True = True" true
        (Eval.relop Ty_bool Eq true_ true_);
      Alcotest.(check bool)
        "0l = 0l" true
        (Eval.relop (Ty_bitv 32) Eq (int32 0l) (int32 0l))
    in
    let test_ne () =
      Alcotest.(check bool)
        "0 != 1" true
        (Eval.relop Ty_bool Ne (int 0) (int 1));
      Alcotest.(check bool)
        "\"abc\" != \"cba\"" true
        (Eval.relop Ty_bool Ne (str "abc") (str "cba"));
      Alcotest.(check bool)
        "True != False" true
        (Eval.relop Ty_bool Ne true_ false_);
      Alcotest.(check bool)
        "0l != 1l" true
        (Eval.relop Ty_bool Ne (int32 0l) (int32 1l))
    in
    [ Alcotest.test_case "test_eq" `Quick test_eq
    ; Alcotest.test_case "test_ne" `Quick test_ne
    ]

  let naryop =
    let test () =
      let l = [ true_; false_; true_; false_ ] in
      check_value "logand" false_ (Eval.naryop Ty_bool Logand l);
      check_value "logor" true_ (Eval.naryop Ty_bool Logor l);
      check_value "distinct_1" false_ (Eval.naryop Ty_bool Distinct l);
      check_value "distinct_2" true_
        (Eval.naryop Ty_bool Distinct [ true_; false_ ]);
      check_value "distinct_3" true_
        (Eval.naryop Ty_bool Distinct [ int 0; int 1; int 2 ]);
      check_value "distinct_4" false_
        (Eval.naryop Ty_bool Distinct [ int 0; int 1; int 0 ])
    in
    Alcotest.test_case "test_logical_ops" `Quick test
end

module Str_test = struct
  let unop =
    let test_length () =
      check_value "test_length" (int 3) (Eval.unop Ty_str Length (str "abc"))
    in
    let test_trim () =
      check_value "test_trim" (str "abc") (Eval.unop Ty_str Trim (str "abc\n"))
    in
    let test_type_error () =
      assert_type_error @@ fun () -> ignore @@ Eval.unop Ty_str Length (int 42)
    in
    [ Alcotest.test_case "test_length" `Quick test_length
    ; Alcotest.test_case "test_trim" `Quick test_trim
    ; Alcotest.test_case "test_type_error" `Quick test_type_error
    ]

  let binop =
    let test_at () =
      check_value "test_at" (str "a") (Eval.binop Ty_str At (str "abc") (int 0))
    in
    let test_index_out_of_bounds_error () =
      Alcotest.check_raises "test_index_out_of_bounds"
        (Eval.Eval_error `Index_out_of_bounds) (fun () ->
        let result = Eval.binop Ty_str At (str "abc") (int 4) in
        check_value "unreachable" (str "a") result )
    in
    let test_string_prefix () =
      check_value "test_string_prefix" true_
        (Eval.binop Ty_str String_prefix (str "ab") (str "abc"))
    in
    let test_string_suffix () =
      check_value "test_string_suffix" true_
        (Eval.binop Ty_str String_suffix (str "bc") (str "abc"))
    in
    let test_string_contains () =
      check_value "test_string_contains" true_
        (Eval.binop Ty_str String_contains (str "abcd") (str "bc"))
    in
    [ Alcotest.test_case "test_at" `Quick test_at
    ; Alcotest.test_case "test_index_out_of_bounds_error" `Quick
        test_index_out_of_bounds_error
    ; Alcotest.test_case "test_string_prefix" `Quick test_string_prefix
    ; Alcotest.test_case "test_string_suffix" `Quick test_string_suffix
    ; Alcotest.test_case "test_string_contains" `Quick test_string_contains
    ]

  let triop =
    let test_string_extract () =
      check_value "test_string_extract" (str "ad")
        (Eval.triop Ty_str String_extract (str "aadd") (int 1) (int 2))
    in
    let test_string_replace () =
      check_value "test_string_replace" (str "abcd")
        (Eval.triop Ty_str String_replace (str "aadd") (str "ad") (str "bc"))
    in
    let test_string_index () =
      check_value "test_string_index" (int 1)
        (Eval.triop Ty_str String_index (str "abcd") (str "bc") (int 0))
    in
    [ Alcotest.test_case "test_string_extract" `Quick test_string_extract
    ; Alcotest.test_case "test_string_replace" `Quick test_string_replace
    ; Alcotest.test_case "test_string_index" `Quick test_string_index
    ]

  let relop =
    let test_lt () =
      Alcotest.(check bool)
        "a < b" true
        (Eval.relop Ty_str Lt (str "a") (str "b"))
    in
    let test_le () =
      Alcotest.(check bool)
        "a <= a" true
        (Eval.relop Ty_str Le (str "a") (str "a"))
    in
    let test_gt () =
      Alcotest.(check bool)
        "a < b" true
        (Eval.relop Ty_str Lt (str "a") (str "b"))
    in
    let test_ge () =
      Alcotest.(check bool)
        "a <= a" true
        (Eval.relop Ty_str Le (str "a") (str "a"))
    in
    let test_eq () =
      Alcotest.(check bool)
        "hello = hello" true
        (Eval.relop Ty_str Eq (str "hello") (str "hello"))
    in
    let test_ne () =
      Alcotest.(check bool)
        "foo != bar" true
        (Eval.relop Ty_str Ne (str "foo") (str "bar"))
    in
    [ Alcotest.test_case "test_lt" `Quick test_lt
    ; Alcotest.test_case "test_le" `Quick test_le
    ; Alcotest.test_case "test_gt" `Quick test_gt
    ; Alcotest.test_case "test_ge" `Quick test_ge
    ; Alcotest.test_case "test_eq" `Quick test_eq
    ; Alcotest.test_case "test_ne" `Quick test_ne
    ]

  let cvtop =
    let test_string_to_code () =
      check_value "test_string_to_code" (int 97)
        (Eval.cvtop Ty_str String_to_code (str "a"))
    in
    let test_string_from_code () =
      check_value "test_string_from_code" (str "b")
        (Eval.cvtop Ty_str String_from_code (int 98))
    in
    let test_string_to_int () =
      check_value "test_string_to_int" (int 98)
        (Eval.cvtop Ty_str String_to_int (str "98"))
    in
    let test_string_to_int_raises () =
      Alcotest.check_raises "test_string_to_int_raises"
        (Eval.Eval_error `Invalid_format_conversion) (fun () ->
        let _ = Eval.cvtop Ty_str String_to_int (str "not_an_int") in
        () )
    in
    let test_string_from_int () =
      check_value "test_string_from_int" (str "97")
        (Eval.cvtop Ty_str String_from_int (int 97))
    in
    let test_string_to_float () =
      check_value "test_string_to_float" (real 98.)
        (Eval.cvtop Ty_str String_to_float (str "98"))
    in
    let test_string_to_float_raises () =
      Alcotest.check_raises "test_string_to_float_raises"
        (Eval.Eval_error `Invalid_format_conversion) (fun () ->
        let _ = Eval.cvtop Ty_str String_to_float (str "not_a_real") in
        () )
    in
    [ Alcotest.test_case "test_string_to_code" `Quick test_string_to_code
    ; Alcotest.test_case "test_string_from_code" `Quick test_string_from_code
    ; Alcotest.test_case "test_string_to_int" `Quick test_string_to_int
    ; Alcotest.test_case "test_string_to_int_raises" `Quick
        test_string_to_int_raises
    ; Alcotest.test_case "test_string_from_int" `Quick test_string_from_int
    ; Alcotest.test_case "test_string_to_float" `Quick test_string_to_float
    ; Alcotest.test_case "test_string_to_float_raises" `Quick
        test_string_to_float_raises
    ]

  let naryop =
    let test () =
      let l = [ str "a"; str "b"; str "c"; str "d" ] in
      check_value "test_concat" (str "abcd") (Eval.naryop Ty_str Concat l)
    in
    Alcotest.test_case "test_string_concat" `Quick test
end

module Float_test (FXX : sig
  val ty : Ty.t

  val v : float -> Value.t
end) =
struct
  open FXX

  let unop =
    let test_neg () =
      check_value "test_neg" (v (-5.)) (Eval.unop ty Neg (v 5.))
    in
    let test_abs () =
      check_value "test_abs" (v 7.) (Eval.unop ty Abs (v (-7.)))
    in
    let test_sqrt () =
      check_value "test_sqrt" (v 3.) (Eval.unop ty Sqrt (v 9.))
    in
    let test_nearest () =
      check_value "test_nearest_1" (v 4.) (Eval.unop ty Nearest (v 4.2));
      check_value "test_nearest_2" (v 5.) (Eval.unop ty Nearest (v 4.6))
    in
    let test_ceil () =
      check_value "test_ceil" (v 5.) (Eval.unop ty Ceil (v 4.2))
    in
    let test_floor () =
      check_value "test_floor" (v 4.) (Eval.unop ty Floor (v 4.2))
    in
    let test_trunc () =
      check_value "test_trunc" (v 3.) (Eval.unop ty Trunc (v Float.pi))
    in
    let test_is_nan () =
      check_value "test_is_nan_nan" true_ (Eval.unop ty Is_nan (v Float.nan));
      check_value "test_is_nan_42" false_ (Eval.unop ty Is_nan (v 42.))
    in
    let test_type_error () =
      assert_type_error @@ fun () -> ignore @@ Eval.unop ty Neg (str "hi")
    in
    [ Alcotest.test_case "test_neg" `Quick test_neg
    ; Alcotest.test_case "test_abs" `Quick test_abs
    ; Alcotest.test_case "test_sqrt" `Quick test_sqrt
    ; Alcotest.test_case "test_nearest" `Quick test_nearest
    ; Alcotest.test_case "test_ceil" `Quick test_ceil
    ; Alcotest.test_case "test_floor" `Quick test_floor
    ; Alcotest.test_case "test_trunc" `Quick test_trunc
    ; Alcotest.test_case "test_is_nan" `Quick test_is_nan
    ; Alcotest.test_case "test_unop_type_error" `Quick test_type_error
    ]

  let binop =
    let test_add () =
      check_value "test_add" (v 5.) (Eval.binop ty Add (v 2.) (v 3.))
    in
    let test_sub () =
      check_value "test_sub" (v 1.) (Eval.binop ty Sub (v 3.) (v 2.))
    in
    let test_mul () =
      check_value "test_mul" (v 9.) (Eval.binop ty Mul (v 3.) (v 3.))
    in
    let test_div () =
      check_value "test_div" (v 2.) (Eval.binop ty Div (v 6.) (v 3.))
    in
    let test_divide_by_zero () =
      check_value "test_divide_by_zero" (v Float.infinity)
        (Eval.binop ty Div (v 1.) (v 0.))
    in
    let test_rem () =
      check_value "test_rem" (v 0.) (Eval.binop ty Rem (v 6.) (v 3.))
    in
    let test_min_max () =
      let a = v 42. in
      let b = v 1337. in
      check_value "test_max" (v 1337.) (Eval.binop ty Max a b);
      check_value "test_min" (v 42.) (Eval.binop ty Min a b)
    in
    let test_copysign () =
      check_value "test_copysign" (v 2.)
        (Eval.binop ty Copysign (v (-2.)) (v 3.))
    in
    [ Alcotest.test_case "test_add" `Quick test_add
    ; Alcotest.test_case "test_sub" `Quick test_sub
    ; Alcotest.test_case "test_mul" `Quick test_mul
    ; Alcotest.test_case "test_div" `Quick test_div
    ; Alcotest.test_case "test_divide_by_zero" `Quick test_divide_by_zero
    ; Alcotest.test_case "test_rem" `Quick test_rem
    ; Alcotest.test_case "test_min_max" `Quick test_min_max
    ; Alcotest.test_case "test_copysign" `Quick test_copysign
    ]

  let relop =
    let test_eq () =
      Alcotest.(check bool) "0 = 0" true (Eval.relop ty Eq (v 0.0) (v 0.0));
      Alcotest.(check bool)
        "nan != nan" true
        (not (Eval.relop ty Eq (v Float.nan) (v Float.nan)))
    in
    let test_ne () =
      Alcotest.(check bool)
        "0 != 0" true
        (not (Eval.relop ty Ne (v 0.0) (v 0.0)));
      Alcotest.(check bool)
        "nan != nan" true
        (Eval.relop ty Ne (v Float.nan) (v Float.nan))
    in
    let test_lt () =
      Alcotest.(check bool) "2 < 3" true (Eval.relop ty Lt (v 2.) (v 3.))
    in
    let test_le () =
      Alcotest.(check bool) "3 <= 3" true (Eval.relop ty Le (v 3.) (v 3.))
    in
    let test_gt () =
      Alcotest.(check bool) "3 < 4" true (Eval.relop ty Lt (v 3.) (v 4.))
    in
    let test_ge () =
      Alcotest.(check bool) "4 <= 4" true (Eval.relop ty Le (v 4.) (v 4.))
    in
    [ Alcotest.test_case "test_eq" `Quick test_eq
    ; Alcotest.test_case "test_ne" `Quick test_ne
    ; Alcotest.test_case "test_lt" `Quick test_lt
    ; Alcotest.test_case "test_le" `Quick test_le
    ; Alcotest.test_case "test_gt" `Quick test_gt
    ; Alcotest.test_case "test_ge" `Quick test_ge
    ]
end

module F32_test = struct
  include Float_test (struct
    let ty = Ty.Ty_fp 32

    let v = float32
  end)

  let regression () =
    let nan = Value.Num (F32 0xff8a1d2bl) in
    let i32 =
      Eval.cvtop (Ty_bitv 32) Reinterpret_float @@ Eval.unop (Ty_fp 32) Neg nan
    in
    let expected = Value.Bitv (Bitvector.of_int32 2139757867l) in
    check_value "test_neg_non_canonical_nan" expected i32

  let regression_case =
    Alcotest.test_case "test_neg_non_canonical_nan" `Quick regression
end

module F64_test = Float_test (struct
  let ty = Ty.Ty_fp 64

  let v = float64
end)

module I32Cvtop_test = struct
  let ty = Ty.Ty_bitv 32

  let cvtop =
    let test_wrap_i64 () =
      check_value "test_wrap_i64" (int32 0l)
        (Eval.cvtop ty WrapI64 (int64 0x1_0000_0000L))
    in
    let test_truncsf32 () =
      check_value "test_truncsf32" (int32 3l)
        (Eval.cvtop ty TruncSF32 (float32 3.7))
    in
    let test_truncuf32 () =
      check_value "test_truncuf32" (int32 4l)
        (Eval.cvtop ty TruncUF32 (float32 4.9))
    in
    let test_truncsf64 () =
      check_value "test_truncsf64" (int32 (-5l))
        (Eval.cvtop ty TruncSF64 (float64 (-5.2)))
    in
    let test_truncuf64 () =
      check_value "test_truncuf64" (int32 6l)
        (Eval.cvtop ty TruncUF64 (float64 6.99))
    in
    let test_trunc_sat_f32_s () =
      check_value "test_trunc_sat_f32_s" (int32 7l)
        (Eval.cvtop ty Trunc_sat_f32_s (float32 7.5))
    in
    let test_trunc_sat_f32_u () =
      check_value "test_trunc_sat_f32_u" (int32 8l)
        (Eval.cvtop ty Trunc_sat_f32_u (float32 8.5))
    in
    let test_trunc_sat_f64_s () =
      check_value "test_trunc_sat_f64_s" (int32 (-9l))
        (Eval.cvtop ty Trunc_sat_f64_s (float64 (-9.9)))
    in
    let test_trunc_sat_f64_u () =
      check_value "test_trunc_sat_f64_u" (int32 10l)
        (Eval.cvtop ty Trunc_sat_f64_u (float64 10.9))
    in
    let test_reinterpret_float () =
      check_value "test_reinterpret_float"
        (int32 (Int32.bits_of_float 1.5))
        (Eval.cvtop ty Reinterpret_float (float32 1.5))
    in
    let test_sign_extend () =
      check_value "test_sign_extend" (int32 (-1l))
        (Eval.cvtop ty (Sign_extend 24) (int8 0xff))
    in
    let test_zero_extend () =
      check_value "test_zero_extend" (int32 0xffl)
        (Eval.cvtop ty (Zero_extend 24) (int8 0xff))
    in
    [ Alcotest.test_case "test_wrap_i64" `Quick test_wrap_i64
    ; Alcotest.test_case "test_truncsf32" `Quick test_truncsf32
    ; Alcotest.test_case "test_truncuf32" `Quick test_truncuf32
    ; Alcotest.test_case "test_truncsf64" `Quick test_truncsf64
    ; Alcotest.test_case "test_truncuf64" `Quick test_truncuf64
    ; Alcotest.test_case "test_trunc_sat_f32_s" `Quick test_trunc_sat_f32_s
    ; Alcotest.test_case "test_trunc_sat_f32_u" `Quick test_trunc_sat_f32_u
    ; Alcotest.test_case "test_trunc_sat_f64_s" `Quick test_trunc_sat_f64_s
    ; Alcotest.test_case "test_trunc_sat_f64_u" `Quick test_trunc_sat_f64_u
    ; Alcotest.test_case "test_reinterpret_float" `Quick test_reinterpret_float
    ; Alcotest.test_case "test_sign_extend" `Quick test_sign_extend
    ; Alcotest.test_case "test_zero_extend" `Quick test_zero_extend
    ]
end

module I64Cvtop_test = struct
  let ty = Ty.Ty_bitv 64

  let cvtop =
    let test_wrap_i64_error () =
      assert_type_error @@ fun () ->
      let _ = Eval.cvtop ty WrapI64 (int64 0x1_0000_0000L) in
      ()
    in
    let test_truncsf32 () =
      check_value "test_truncsf32" (int64 3L)
        (Eval.cvtop ty TruncSF32 (float32 3.7))
    in
    let test_truncuf32 () =
      check_value "test_truncuf32" (int64 4L)
        (Eval.cvtop ty TruncUF32 (float32 4.9))
    in
    let test_truncsf64 () =
      check_value "test_truncsf64" (int64 (-5L))
        (Eval.cvtop ty TruncSF64 (float64 (-5.2)))
    in
    let test_truncuf64 () =
      check_value "test_truncuf64" (int64 6L)
        (Eval.cvtop ty TruncUF64 (float64 6.99))
    in
    let test_trunc_sat_f32_s () =
      check_value "test_trunc_sat_f32_s" (int64 7L)
        (Eval.cvtop ty Trunc_sat_f32_s (float32 7.5))
    in
    let test_trunc_sat_f32_u () =
      check_value "test_trunc_sat_f32_u" (int64 8L)
        (Eval.cvtop ty Trunc_sat_f32_u (float32 8.5))
    in
    let test_trunc_sat_f64_s () =
      check_value "test_trunc_sat_f64_s" (int64 (-9L))
        (Eval.cvtop ty Trunc_sat_f64_s (float64 (-9.9)))
    in
    let test_trunc_sat_f64_u () =
      check_value "test_trunc_sat_f64_u" (int64 10L)
        (Eval.cvtop ty Trunc_sat_f64_u (float64 10.9))
    in
    let test_reinterpret_float () =
      check_value "test_reinterpret_float"
        (int64 (Int64.bits_of_float 1.5))
        (Eval.cvtop ty Reinterpret_float (float64 1.5))
    in
    let test_sign_extend_i8 () =
      check_value "test_sign_extend_i8" (int64 (-1L))
        (Eval.cvtop ty (Sign_extend 56) (int8 (-1)))
    in
    let test_sign_extend_i32 () =
      check_value "test_sign_extend_i32" (int64 (-1L))
        (Eval.cvtop ty (Sign_extend 32) (int32 (-1l)))
    in
    let test_zero_extend () =
      check_value "test_zero_extend" (int64 0xffffffffL)
        (Eval.cvtop ty (Zero_extend 32) (int32 (-1l)))
    in
    [ Alcotest.test_case "test_wrap_i64_error" `Quick test_wrap_i64_error
    ; Alcotest.test_case "test_truncsf32" `Quick test_truncsf32
    ; Alcotest.test_case "test_truncuf32" `Quick test_truncuf32
    ; Alcotest.test_case "test_truncsf64" `Quick test_truncsf64
    ; Alcotest.test_case "test_truncuf64" `Quick test_truncuf64
    ; Alcotest.test_case "test_trunc_sat_f32_s" `Quick test_trunc_sat_f32_s
    ; Alcotest.test_case "test_trunc_sat_f32_u" `Quick test_trunc_sat_f32_u
    ; Alcotest.test_case "test_trunc_sat_f64_s" `Quick test_trunc_sat_f64_s
    ; Alcotest.test_case "test_trunc_sat_f64_u" `Quick test_trunc_sat_f64_u
    ; Alcotest.test_case "test_reinterpret_float" `Quick test_reinterpret_float
    ; Alcotest.test_case "test_sign_extend_i8" `Quick test_sign_extend_i8
    ; Alcotest.test_case "test_sign_extend_i32" `Quick test_sign_extend_i32
    ; Alcotest.test_case "test_zero_extend" `Quick test_zero_extend
    ]
end

module F32Cvtop_test = struct
  let ty = Ty.Ty_fp 32

  let cvtop =
    let test_demote_f64 () =
      check_value "test_demote_f64" (float32 3.14)
        (Eval.cvtop ty DemoteF64 (float64 3.14))
    in
    let test_demote_f64_nan () =
      check_value "test_demote_f64_nan" (float32 nan)
        (Eval.cvtop ty DemoteF64 (float64 nan))
    in
    let test_convert_si32 () =
      check_value "test_convert_si32" (float32 (-42.))
        (Eval.cvtop ty ConvertSI32 (int32 (-42l)))
    in
    let test_convert_ui32 () =
      check_value "test_convert_ui32_1" (float32 42.)
        (Eval.cvtop ty ConvertUI32 (int32 42l));
      check_value "test_convert_ui32_2" (float32 4294967294.)
        (Eval.cvtop ty ConvertUI32 (int32 (-1l)))
    in
    let test_convert_si64 () =
      check_value "test_convert_si64_1" (float32 (-42.))
        (Eval.cvtop ty ConvertSI64 (int64 (-42L)));
      check_value "test_convert_si64_2"
        (float32 4503599627370512.)
        (Eval.cvtop ty ConvertSI64 (int64 0x10_0000_0000_0100L))
    in
    let test_convert_ui64 () =
      check_value "test_convert_ui64_1" (float32 42.)
        (Eval.cvtop ty ConvertUI64 (int64 42L));
      check_value "test_convert_ui64_2"
        (float32 4503599627370512.)
        (Eval.cvtop ty ConvertUI64 (int64 0x10_0000_0000_0100L))
    in
    let test_reinterpret_int () =
      check_value "test_reinterpret_int" (float32 1.)
        (Eval.cvtop ty Reinterpret_int (int32 1065353216l))
    in
    let test_promote_f32_error () =
      assert_type_error @@ fun () ->
      let _ = Eval.cvtop ty PromoteF32 (float32 42.0) in
      ()
    in
    [ Alcotest.test_case "test_demote_f64" `Quick test_demote_f64
    ; Alcotest.test_case "test_demote_f64_nan" `Quick test_demote_f64_nan
    ; Alcotest.test_case "test_convert_si32" `Quick test_convert_si32
    ; Alcotest.test_case "test_convert_ui32" `Quick test_convert_ui32
    ; Alcotest.test_case "test_convert_si64" `Quick test_convert_si64
    ; Alcotest.test_case "test_convert_ui64" `Quick test_convert_ui64
    ; Alcotest.test_case "test_reinterpret_int" `Quick test_reinterpret_int
    ; Alcotest.test_case "test_promote_f32_error" `Quick test_promote_f32_error
    ]
end

module F64Cvtop_test = struct
  let ty = Ty.Ty_fp 64

  let cvtop =
    let test_promote_f32 () =
      check_value "test_promote_f32" (float64 42.0)
        (Eval.cvtop ty PromoteF32 (float32 42.0))
    in
    let test_promote_f32_nan () =
      check_value "test_promote_f32_nan" (float64 nan)
        (Eval.cvtop ty PromoteF32 (float32 nan))
    in
    let test_convert_si32 () =
      check_value "test_convert_si32" (float64 (-42.))
        (Eval.cvtop ty ConvertSI32 (int32 (-42l)))
    in
    let test_convert_ui32 () =
      check_value "test_convert_ui32" (float64 42.)
        (Eval.cvtop ty ConvertUI32 (int32 42l))
    in
    let test_convert_si64 () =
      check_value "test_convert_si64" (float64 (-42.))
        (Eval.cvtop ty ConvertSI64 (int64 (-42L)))
    in
    let test_convert_ui64 () =
      check_value "test_convert_ui64_1" (float64 42.)
        (Eval.cvtop ty ConvertUI64 (int64 42L));
      check_value "test_convert_ui64_2"
        (float64 (9223372036854775807. *. 2.))
        (Eval.cvtop ty ConvertUI64 (int64 (-1L)))
    in
    let test_reinterpret_int () =
      check_value "test_reinterpret_int" (float64 1.)
        (Eval.cvtop ty Reinterpret_int (int64 4607182418800017408L))
    in
    let test_demote_f64_error () =
      assert_type_error @@ fun () ->
      let _ = Eval.cvtop ty DemoteF64 (float64 3.14) in
      ()
    in
    [ Alcotest.test_case "test_promote_f32" `Quick test_promote_f32
    ; Alcotest.test_case "test_promote_f32_nan" `Quick test_promote_f32_nan
    ; Alcotest.test_case "test_convert_si32" `Quick test_convert_si32
    ; Alcotest.test_case "test_convert_ui32" `Quick test_convert_ui32
    ; Alcotest.test_case "test_convert_si64" `Quick test_convert_si64
    ; Alcotest.test_case "test_convert_ui64" `Quick test_convert_ui64
    ; Alcotest.test_case "test_reinterpret_int" `Quick test_reinterpret_int
    ; Alcotest.test_case "test_demote_f64_error" `Quick test_demote_f64_error
    ]
end

module Bitv_test = struct
  let ty = Ty.Ty_bitv 8

  let unop =
    let test_rotate_left () =
      check_value "test_rotate_left" (int8 0xB1)
        (Eval.unop ty (Rotl 3) (int8 0x36))
    in
    let test_rotate_right () =
      check_value "test_rotate_right" (int8 0xC6)
        (Eval.unop ty (Rotr 3) (int8 0x36))
    in
    [ Alcotest.test_case "test_rotate_left" `Quick test_rotate_left
    ; Alcotest.test_case "test_rotate_right" `Quick test_rotate_right
    ]

  let binop =
    let test_ext_rotate_left () =
      check_value "test_ext_rotate_left" (int8 0xB1)
        (Eval.binop ty Ext_rotl (int8 0x36) (int8 3))
    in
    let test_ext_rotate_right () =
      check_value "test_ext_rotate_right" (int8 0xC6)
        (Eval.binop ty Ext_rotr (int8 0x36) (int8 3))
    in
    [ Alcotest.test_case "test_ext_rotate_left" `Quick test_ext_rotate_left
    ; Alcotest.test_case "test_ext_rotate_right" `Quick test_ext_rotate_right
    ]
end

let () =
  Alcotest.run "Eval tests"
    [ (* Unops *)
      ("Int_test.unop", Int_test.unop)
    ; ("Real_test.unop", Real_test.unop)
    ; ("Bool_test.unop", Bool_test.unop)
    ; ("Str_test.unop", Str_test.unop)
    ; ("Bitv_test.unop", Bitv_test.unop)
    ; ("F32_test.unop", F32_test.unop)
    ; ("F64_test.unop", F64_test.unop)
    ; ("test_binop", [])
    ; ("Int_test.binop", Int_test.binop)
    ; ("Real_test.binop", Real_test.binop)
    ; ("Bool_test.binop", Bool_test.binop)
    ; ("Str_test.binop", Str_test.binop)
    ; ("Bitv_test.binop", Bitv_test.binop)
    ; ("F32_test.binop", F32_test.binop)
    ; ("F64_test.binop", F64_test.binop)
    ; ("Bool_test.triop", [ Bool_test.triop ])
    ; ("Str_test.triop", Str_test.triop)
    ; ("test_relop", [])
    ; ("Int_test.relop", Int_test.relop)
    ; ("Real_test.relop", Real_test.relop)
    ; ("Bool_test.relop", Bool_test.relop)
    ; ("Str_test.relop", Str_test.relop)
    ; ("F32_test.relop", F32_test.relop)
    ; ("F64_test.relop", F64_test.relop)
    ; ("test_cvtop", [])
    ; ("Int_test.cvtop", Int_test.cvtop)
    ; ("Real_test.cvtop", Real_test.cvtop)
    ; ("Str_test.cvtop", Str_test.cvtop)
    ; ("I32Cvtop_test.cvtop", I32Cvtop_test.cvtop)
    ; ("I64Cvtop_test.cvtop", I64Cvtop_test.cvtop)
    ; ("F32Cvtop_test.cvtop", F32Cvtop_test.cvtop)
    ; ("F64Cvtop_test.cvtop", F64Cvtop_test.cvtop)
    ; ("Bool_test.naryop", [ Bool_test.naryop ])
    ; ("Str_test.naryop", [ Str_test.naryop ])
    ; ("F32_test.regression", [ F32_test.regression_case ])
    ]

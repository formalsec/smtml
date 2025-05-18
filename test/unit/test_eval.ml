open OUnit2
open Smtml

(* Helpers *)
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

let assert_equal =
  let pp_diff fmt (a, b) =
    Fmt.pf fmt "Expected '%a' but got '%a'" Value.pp a Value.pp b
  in
  assert_equal ~cmp:Value.equal ~pp_diff

let assert_type_error f =
  try
    f ();
    assert_bool "raise TypeError" false
  with Eval.TypeError _ -> ()

let assert_parse_error f =
  try
    f ();
    assert_bool "raise Invalid_argument" false
  with Invalid_argument _ -> ()

module Int_test = struct
  (* Unary operators *)
  let unop =
    let test_neg _ =
      let result = Eval.unop Ty_int Neg (int 5) in
      assert_equal (int (-5)) result
    in
    let test_lognot _ =
      let result = Eval.unop Ty_int Not (int ~-1) in
      assert_equal (int 0) result
    in
    let test_abs _ =
      let result = Eval.unop Ty_int Abs (int (-7)) in
      assert_equal (int 7) result
    in
    let test_type_error _ =
      assert_type_error @@ fun () -> ignore @@ Eval.unop Ty_int Neg (str "hi")
    in
    [ "test_neg" >:: test_neg
    ; "test_not" >:: test_lognot
    ; "test_abs" >:: test_abs
    ; "test_unop_type_error" >:: test_type_error
    ]

  (* Binary operators *)
  let binop =
    let test_add _ =
      let result = Eval.binop Ty_int Add (int 2) (int 3) in
      assert_equal (int 5) result
    in
    let test_sub _ =
      let result = Eval.binop Ty_int Sub (int 3) (int 2) in
      assert_equal (int 1) result
    in
    let test_mul _ =
      let result = Eval.binop Ty_int Mul (int 3) (int 3) in
      assert_equal (int 9) result
    in
    let test_div _ =
      let result = Eval.binop Ty_int Div (int 6) (int 3) in
      assert_equal (int 2) result
    in
    let test_divide_by_zero _ =
      assert_raises Division_by_zero @@ fun () ->
      let _ = Eval.binop Ty_int Div (int 1) (int 0) in
      ()
    in
    let test_rem _ =
      let result = Eval.binop Ty_int Rem (int 6) (int 3) in
      assert_equal (int 0) result
    in
    let test_pow _ =
      let result = Eval.binop Ty_int Pow (int 2) (int 3) in
      assert_equal (int 8) result
    in
    let test_min_max _ =
      let a = int 42 in
      let b = int 1337 in
      let result = Eval.binop Ty_int Max a b in
      assert_equal (int 1337) result;
      let result = Eval.binop Ty_int Min a b in
      assert_equal (int 42) result
    in
    let test_logical_ops _ =
      let a = int 0b1100 in
      let b = int 0b1010 in
      assert_equal (Eval.binop Ty_int And a b) (int 8);
      assert_equal (Eval.binop Ty_int Or a b) (int 14);
      assert_equal (Eval.binop Ty_int Xor a b) (int 6)
    in
    let test_shifts _ =
      let i = int 0b0011 in
      assert_equal (Eval.binop Ty_int Shl i (int 1)) (int 0b0110);
      assert_equal (Eval.binop Ty_int ShrL i (int 1)) (int 0b0001);
      assert_equal (Eval.binop Ty_int ShrA i (int 1)) (int 0b0001)
    in
    [ "test_add" >:: test_add
    ; "test_sub" >:: test_sub
    ; "test_mul" >:: test_mul
    ; "test_div" >:: test_div
    ; "test_divide_by_zero" >:: test_divide_by_zero
    ; "test_rem" >:: test_rem
    ; "test_pow" >:: test_pow
    ; "test_min_max" >:: test_min_max
    ; "test_logical_ops" >:: test_logical_ops
    ; "test_shifts" >:: test_shifts
    ]

  (* Relational operators *)
  let relop =
    let test_lt _ =
      assert_bool "2 < 3" (Eval.relop Ty_int Lt (int 2) (int 3))
    in
    let test_le _ =
      assert_bool "2 <= 3" (Eval.relop Ty_int Le (int 3) (int 3))
    in
    let test_gt _ =
      assert_bool "4 > 4" (Eval.relop Ty_int Gt (int 4) (int 3))
    in
    let test_ge _ =
      assert_bool "4 >= 4" (Eval.relop Ty_int Ge (int 4) (int 4))
    in

    [ "test_lt" >:: test_lt
    ; "test_le" >:: test_le
    ; "test_gt" >:: test_gt
    ; "test_ge" >:: test_ge
    ]

  (* Conversion operators *)
  let cvtop =
    let test_of_bool _ =
      let result = Eval.cvtop Ty_int OfBool True in
      assert_equal (int 1) result
    in
    let test_to_string _ =
      let result = Eval.cvtop Ty_int ToString (int 42) in
      assert_equal (str "42") result
    in
    let test_of_string _ =
      let result = Eval.cvtop Ty_int OfString (str "123") in
      assert_equal (int 123) result
    in
    let test_of_string_error _ =
      assert_parse_error @@ fun () ->
      let _ = Eval.cvtop Ty_int OfString (str "not_an_int") in
      ()
    in
    let test_reinterpret_float _ =
      let result = Eval.cvtop Ty_int Reinterpret_float (real 42.0) in
      assert_equal (int 42) result
    in
    [ "test_of_bool" >:: test_of_bool
    ; "test_to_string" >:: test_to_string
    ; "test_of_string" >:: test_of_string
    ; "test_of_string_error" >:: test_of_string_error
    ; "test_reinterpret_float" >:: test_reinterpret_float
    ]
end

module Real_test = struct
  (* Unary operators *)
  let unop =
    let test_neg _ =
      let result = Eval.unop Ty_real Neg (real 5.) in
      assert_equal (real (-5.)) result
    in
    let test_abs _ =
      let result = Eval.unop Ty_real Abs (real (-7.)) in
      assert_equal (real 7.) result
    in
    let test_sqrt _ =
      let result = Eval.unop Ty_real Sqrt (real 9.) in
      assert_equal (real 3.) result
    in
    let test_nearest _ =
      assert_equal (real 4.) (Eval.unop Ty_real Nearest (real 4.2));
      assert_equal (real 5.) (Eval.unop Ty_real Nearest (real 4.6))
    in
    let test_ceil _ =
      let result = Eval.unop Ty_real Ceil (real 4.2) in
      assert_equal (real 5.) result
    in
    let test_floor _ =
      let result = Eval.unop Ty_real Floor (real 4.2) in
      assert_equal (real 4.) result
    in
    let test_trunc _ =
      let result = Eval.unop Ty_real Trunc (real Float.pi) in
      assert_equal (real 3.) result
    in
    let test_is_nan _ =
      assert_equal (Eval.unop Ty_real Is_nan (real Float.nan)) true_;
      assert_equal (Eval.unop Ty_real Is_nan (real 42.)) false_
    in
    let test_type_error _ =
      assert_type_error @@ fun () -> ignore @@ Eval.unop Ty_real Neg (str "hi")
    in
    [ "test_neg" >:: test_neg
    ; "test_abs" >:: test_abs
    ; "test_sqrt" >:: test_sqrt
    ; "test_nearest" >:: test_nearest
    ; "test_ceil" >:: test_ceil
    ; "test_floor" >:: test_floor
    ; "test_trunc" >:: test_trunc
    ; "test_is_nan" >:: test_is_nan
    ; "test_unop_type_error" >:: test_type_error
    ]

  (* Binary operators *)
  let binop =
    let test_add _ =
      let result = Eval.binop Ty_real Add (real 2.) (real 3.) in
      assert_equal (real 5.) result
    in
    let test_sub _ =
      let result = Eval.binop Ty_real Sub (real 3.) (real 2.) in
      assert_equal (real 1.) result
    in
    let test_mul _ =
      let result = Eval.binop Ty_real Mul (real 3.) (real 3.) in
      assert_equal (real 9.) result
    in
    let test_div _ =
      let result = Eval.binop Ty_real Div (real 6.) (real 3.) in
      assert_equal (real 2.) result
    in
    let test_divide_by_zero _ =
      let result = Eval.binop Ty_real Div (real 1.) (real 0.) in
      assert_equal (real Float.infinity) result
    in
    let test_rem _ =
      let result = Eval.binop Ty_real Rem (real 6.) (real 3.) in
      assert_equal (real 0.) result
    in
    let test_pow _ =
      let result = Eval.binop Ty_real Pow (real 2.) (real 3.) in
      assert_equal (real 8.) result
    in
    let test_min_max _ =
      let a = real 42. in
      let b = real 1337. in
      let result = Eval.binop Ty_real Max a b in
      assert_equal (real 1337.) result;
      let result = Eval.binop Ty_real Min a b in
      assert_equal (real 42.) result
    in
    [ "test_add" >:: test_add
    ; "test_sub" >:: test_sub
    ; "test_mul" >:: test_mul
    ; "test_div" >:: test_div
    ; "test_divide_by_zero" >:: test_divide_by_zero
    ; "test_rem" >:: test_rem
    ; "test_pow" >:: test_pow
    ; "test_min_max" >:: test_min_max
    ]

  (* Relational operators *)
  let relop =
    let test_eq _ =
      assert_bool "0 = 0" (Eval.relop Ty_real Eq (real 0.0) (real 0.0));
      assert_bool "nan != nan"
        (not (Eval.relop Ty_real Eq (real Float.nan) (real Float.nan)))
    in
    let test_ne _ =
      assert_bool "0 = 0" (not (Eval.relop Ty_real Ne (real 0.0) (real 0.0)));
      assert_bool "nan != nan"
        (Eval.relop Ty_real Ne (real Float.nan) (real Float.nan))
    in
    let test_lt _ =
      assert_bool "2 < 3" (Eval.relop Ty_real Lt (real 2.) (real 3.))
    in
    let test_le _ =
      assert_bool "2 <= 3" (Eval.relop Ty_real Le (real 3.) (real 3.))
    in
    let test_gt _ =
      assert_bool "4 > 4" (Eval.relop Ty_real Gt (real 4.) (real 3.))
    in
    let test_ge _ =
      assert_bool "4 >= 4" (Eval.relop Ty_real Ge (real 4.) (real 4.))
    in

    [ "test_eq" >:: test_eq
    ; "test_ne" >:: test_ne
    ; "test_lt" >:: test_lt
    ; "test_le" >:: test_le
    ; "test_gt" >:: test_gt
    ; "test_ge" >:: test_ge
    ]

  (* Conversion operators *)
  let cvtop =
    let test_of_string _ =
      let result = Eval.cvtop Ty_real OfString (str "42.") in
      assert_equal (real 42.) result
    in
    let test_to_string _ =
      let result = Eval.cvtop Ty_real ToString (real 42.) in
      assert_equal (str "42.") result
    in
    let test_of_string_error _ =
      assert_parse_error @@ fun () ->
      let _ = Eval.cvtop Ty_real OfString (str "not_a_real") in
      ()
    in
    let test_reinterpret_int _ =
      let result = Eval.cvtop Ty_real Reinterpret_int (int 42) in
      assert_equal (real 42.) result
    in
    let test_reinterpret_float _ =
      let result = Eval.cvtop Ty_real Reinterpret_float (real 42.) in
      assert_equal (int 42) result
    in
    [ "test_to_string" >:: test_to_string
    ; "test_of_string" >:: test_of_string
    ; "test_of_string_error" >:: test_of_string_error
    ; "test_reinterpret_int" >:: test_reinterpret_int
    ; "test_reinterpret_float" >:: test_reinterpret_float
    ]
end

module Bool_test = struct
  (* Unary operators *)
  let unop =
    [ ( "test_not" >:: fun _ ->
        let result = Eval.unop Ty_bool Not true_ in
        assert_equal false_ result )
    ; ( "test_type_error" >:: fun _ ->
        assert_type_error @@ fun () ->
        ignore @@ Eval.unop Ty_bool Not (str "false") )
    ]

  (* Binary operators *)
  let binop =
    [ ( "test_and" >:: fun _ ->
        assert_equal true_ (Eval.binop Ty_bool And true_ true_);
        assert_equal false_ (Eval.binop Ty_bool And true_ false_);
        assert_equal false_ (Eval.binop Ty_bool And false_ true_);
        assert_equal false_ (Eval.binop Ty_bool And false_ false_) )
    ; ( "test_or" >:: fun _ ->
        assert_equal true_ (Eval.binop Ty_bool Or true_ true_);
        assert_equal true_ (Eval.binop Ty_bool Or true_ false_);
        assert_equal true_ (Eval.binop Ty_bool Or false_ true_);
        assert_equal false_ (Eval.binop Ty_bool Or false_ false_) )
    ; ( "test_xor" >:: fun _ ->
        assert_equal false_ (Eval.binop Ty_bool Xor true_ true_);
        assert_equal true_ (Eval.binop Ty_bool Xor true_ false_);
        assert_equal true_ (Eval.binop Ty_bool Xor false_ true_);
        assert_equal false_ (Eval.binop Ty_bool Xor false_ false_) )
    ]

  let triop =
    "test_ite" >:: fun _ ->
    let result = Eval.triop Ty_bool Ite true_ (int 1) (int 0) in
    assert_equal (int 1) result;
    let result = Eval.triop Ty_bool Ite false_ (int 1) (int 0) in
    assert_equal (int 0) result

  (* Relational operators *)
  let relop =
    [ ( "test_eq" >:: fun _ ->
        assert_bool "0 = 0" (Eval.relop Ty_bool Eq (int 0) (int 0));
        assert_bool "\"abc\" = \"abc\""
          (Eval.relop Ty_bool Eq (str "abc") (str "abc"));
        assert_bool "True = True" (Eval.relop Ty_bool Eq true_ true_);
        assert_bool "0l = 0l" (Eval.relop (Ty_bitv 32) Eq (int32 0l) (int32 0l))
      )
    ; ( "test_ne" >:: fun _ ->
        assert_bool "0 != 1" (Eval.relop Ty_bool Ne (int 0) (int 1));
        assert_bool "\"abc\" != \"cba\""
          (Eval.relop Ty_bool Ne (str "abc") (str "cba"));
        assert_bool "True != False" (Eval.relop Ty_bool Ne true_ false_);
        assert_bool "0l != 1l" (Eval.relop Ty_bool Ne (int32 0l) (int32 1l)) )
    ]

  let naryop =
    "test_logical_ops" >:: fun _ ->
    let l = [ true_; false_; true_; false_ ] in
    assert_equal false_ (Eval.naryop Ty_bool Logand l);
    assert_equal true_ (Eval.naryop Ty_bool Logor l)
end

module Str_test = struct
  (* Unary operators *)
  let unop =
    [ ( "test_length" >:: fun _ ->
        let result = Eval.unop Ty_str Length (str "abc") in
        assert_equal (int 3) result )
    ; ( "test_trim" >:: fun _ ->
        let result = Eval.unop Ty_str Trim (str "abc\n") in
        assert_equal (str "abc") result )
    ; ( "test_type_error" >:: fun _ ->
        assert_type_error @@ fun () ->
        ignore @@ Eval.unop Ty_str Length (int 42) )
    ]

  (* Binary operators *)
  let binop =
    [ ( "test_at" >:: fun _ ->
        let result = Eval.binop Ty_str At (str "abc") (int 0) in
        assert_equal (str "a") result )
    ; ( "test_index_out_of_bounds_error" >:: fun _ ->
        assert_raises Eval.Index_out_of_bounds @@ fun () ->
        let result = Eval.binop Ty_str At (str "abc") (int 4) in
        assert_equal (str "a") result )
    ; ( "test_string_prefix" >:: fun _ ->
        let result = Eval.binop Ty_str String_prefix (str "ab") (str "abc") in
        assert_equal true_ result )
    ; ( "test_string_suffix" >:: fun _ ->
        let result = Eval.binop Ty_str String_suffix (str "bc") (str "abc") in
        assert_equal true_ result )
    ; ( "test_string_suffix" >:: fun _ ->
        let result =
          Eval.binop Ty_str String_contains (str "abcd") (str "bc")
        in
        assert_equal true_ result )
    ]

  let triop =
    [ ( "test_string_extract" >:: fun _ ->
        let result =
          Eval.triop Ty_str String_extract (str "aadd") (int 1) (int 2)
        in
        assert_equal (str "ad") result )
    ; ( "test_string_replace" >:: fun _ ->
        let result =
          Eval.triop Ty_str String_replace (str "aadd") (str "ad") (str "bc")
        in
        assert_equal (str "abcd") result )
    ; ( "test_string_index" >:: fun _ ->
        let result =
          Eval.triop Ty_str String_index (str "abcd") (str "bc") (int 0)
        in
        assert_equal (int 1) result )
    ]

  (* Relational operators *)
  let relop =
    [ ( "test_lt" >:: fun _ ->
        assert_bool "a < b" (Eval.relop Ty_str Lt (str "a") (str "b")) )
    ; ( "test_le" >:: fun _ ->
        assert_bool "a <= a" (Eval.relop Ty_str Le (str "a") (str "a")) )
    ; ( "test_gt" >:: fun _ ->
        assert_bool "b > a" (Eval.relop Ty_str Gt (str "b") (str "a")) )
    ; ( "test_ge" >:: fun _ ->
        assert_bool "a >= a" (Eval.relop Ty_str Ge (str "a") (str "a")) )
    ; ( "test_eq" >:: fun _ ->
        assert_bool "hello = hello"
          (Eval.relop Ty_str Eq (str "hello") (str "hello")) )
    ; ( "test_ne" >:: fun _ ->
        assert_bool "foo != bar" (Eval.relop Ty_str Ne (str "foo") (str "bar"))
      )
    ]

  let cvtop =
    [ ( "test_string_to_code" >:: fun _ ->
        let result = Eval.cvtop Ty_str String_to_code (str "a") in
        assert_equal (int 97) result )
    ; ( "test_string_from_code" >:: fun _ ->
        let result = Eval.cvtop Ty_str String_from_code (int 98) in
        assert_equal (str "b") result )
    ; ( "test_string_to_int" >:: fun _ ->
        let result = Eval.cvtop Ty_str String_to_int (str "98") in
        assert_equal (int 98) result )
    ; ( "test_string_to_int_raises" >:: fun _ ->
        assert_parse_error @@ fun () ->
        let _ = Eval.cvtop Ty_str String_to_int (str "not_an_int") in
        () )
    ; ( "test_string_from_int" >:: fun _ ->
        let result = Eval.cvtop Ty_str String_from_int (int 97) in
        assert_equal (str "97") result )
    ; ( "test_string_to_float" >:: fun _ ->
        let result = Eval.cvtop Ty_str String_to_float (str "98") in
        assert_equal (real 98.) result )
    ; ( "test_string_to_float_raises" >:: fun _ ->
        assert_parse_error @@ fun () ->
        let _ = Eval.cvtop Ty_str String_to_float (str "not_a_real") in
        () )
    ]

  let naryop =
    "test_logical_ops" >:: fun _ ->
    let l = [ str "a"; str "b"; str "c"; str "d" ] in
    assert_equal (str "abcd") (Eval.naryop Ty_str Concat l)
end

module Float_test (FXX : sig
  val ty : [ `Ty_fp ] Ty.ty

  val v : float -> Value.t
end) =
struct
  open FXX

  (* Unary operators *)
  let unop =
    let test_neg _ =
      let result = Eval.unop ty Neg (v 5.) in
      assert_equal (v (-5.)) result
    in
    let test_abs _ =
      let result = Eval.unop ty Abs (v (-7.)) in
      assert_equal (v 7.) result
    in
    let test_sqrt _ =
      let result = Eval.unop ty Sqrt (v 9.) in
      assert_equal (v 3.) result
    in
    let test_nearest _ =
      assert_equal (v 4.) (Eval.unop ty Nearest (v 4.2));
      assert_equal (v 5.) (Eval.unop ty Nearest (v 4.6))
    in
    let test_ceil _ =
      let result = Eval.unop ty Ceil (v 4.2) in
      assert_equal (v 5.) result
    in
    let test_floor _ =
      let result = Eval.unop ty Floor (v 4.2) in
      assert_equal (v 4.) result
    in
    let test_trunc _ =
      let result = Eval.unop ty Trunc (v Float.pi) in
      assert_equal (v 3.) result
    in
    let test_is_nan _ =
      assert_equal (Eval.unop ty Is_nan (v Float.nan)) true_;
      assert_equal (Eval.unop ty Is_nan (v 42.)) false_
    in
    let test_type_error _ =
      assert_type_error @@ fun () -> ignore @@ Eval.unop ty Neg (str "hi")
    in
    [ "test_neg" >:: test_neg
    ; "test_abs" >:: test_abs
    ; "test_sqrt" >:: test_sqrt
    ; "test_nearest" >:: test_nearest
    ; "test_ceil" >:: test_ceil
    ; "test_floor" >:: test_floor
    ; "test_trunc" >:: test_trunc
    ; "test_is_nan" >:: test_is_nan
    ; "test_unop_type_error" >:: test_type_error
    ]

  (* Binary operators *)
  let binop =
    let test_add _ =
      let result = Eval.binop ty Add (v 2.) (v 3.) in
      assert_equal (v 5.) result
    in
    let test_sub _ =
      let result = Eval.binop ty Sub (v 3.) (v 2.) in
      assert_equal (v 1.) result
    in
    let test_mul _ =
      let result = Eval.binop ty Mul (v 3.) (v 3.) in
      assert_equal (v 9.) result
    in
    let test_div _ =
      let result = Eval.binop ty Div (v 6.) (v 3.) in
      assert_equal (v 2.) result
    in
    let test_divide_by_zero _ =
      let result = Eval.binop ty Div (v 1.) (v 0.) in
      assert_equal (v Float.infinity) result
    in
    let test_rem _ =
      let result = Eval.binop ty Rem (v 6.) (v 3.) in
      assert_equal (v 0.) result
    in
    let test_min_max _ =
      let a = v 42. in
      let b = v 1337. in
      let result = Eval.binop ty Max a b in
      assert_equal (v 1337.) result;
      let result = Eval.binop ty Min a b in
      assert_equal (v 42.) result
    in
    let test_copysign _ =
      let result = Eval.binop ty Copysign (v (-2.)) (v 3.) in
      assert_equal (v 2.) result
    in
    [ "test_add" >:: test_add
    ; "test_sub" >:: test_sub
    ; "test_mul" >:: test_mul
    ; "test_div" >:: test_div
    ; "test_divide_by_zero" >:: test_divide_by_zero
    ; "test_rem" >:: test_rem
    ; "test_min_max" >:: test_min_max
    ; "test_copysign" >:: test_copysign
    ]

  (* Relational operators *)
  let relop =
    let test_eq _ =
      assert_bool "0 = 0" (Eval.relop ty Eq (v 0.0) (v 0.0));
      assert_bool "nan != nan"
        (not (Eval.relop ty Eq (v Float.nan) (v Float.nan)))
    in
    let test_ne _ =
      assert_bool "0 = 0" (not (Eval.relop ty Ne (v 0.0) (v 0.0)));
      assert_bool "nan != nan" (Eval.relop ty Ne (v Float.nan) (v Float.nan))
    in
    let test_lt _ = assert_bool "2 < 3" (Eval.relop ty Lt (v 2.) (v 3.)) in
    let test_le _ = assert_bool "2 <= 3" (Eval.relop ty Le (v 3.) (v 3.)) in
    let test_gt _ = assert_bool "4 > 4" (Eval.relop ty Gt (v 4.) (v 3.)) in
    let test_ge _ = assert_bool "4 >= 4" (Eval.relop ty Ge (v 4.) (v 4.)) in

    [ "test_eq" >:: test_eq
    ; "test_ne" >:: test_ne
    ; "test_lt" >:: test_lt
    ; "test_le" >:: test_le
    ; "test_gt" >:: test_gt
    ; "test_ge" >:: test_ge
    ]
end

module F32_test = struct
  include Float_test (struct
    let ty = Ty.Ty_fp 32

    let v = float32
  end)

  let regression =
    "test_neg_non_canonical_nan" >:: fun _ ->
    let nan = Value.Num (F32 0xff8a1d2bl) in
    let i32 =
      Eval.cvtop (Ty_bitv 32) Reinterpret_float @@ Eval.unop (Ty_fp 32) Neg nan
    in
    let expected = Value.Bitv (Bitvector.of_int32 2139757867l) in
    assert_equal i32 expected
end

module F64_test = Float_test (struct
  let ty = Ty.Ty_fp 64

  let v = float64
end)

module I32Cvtop_test = struct
  let ty = Ty.Ty_bitv 32

  let cvtop =
    [ ( "test_wrap_i64" >:: fun _ ->
        assert_equal (int32 0l) (Eval.cvtop ty WrapI64 (int64 0x1_0000_0000L))
      )
    ; ( "test_truncsf32" >:: fun _ ->
        assert_equal (int32 3l) (Eval.cvtop ty TruncSF32 (float32 3.7)) )
    ; ( "test_truncuf32" >:: fun _ ->
        assert_equal (int32 4l) (Eval.cvtop ty TruncUF32 (float32 4.9)) )
    ; ( "test_truncsf64" >:: fun _ ->
        assert_equal (int32 (-5l)) (Eval.cvtop ty TruncSF64 (float64 (-5.2))) )
    ; ( "test_truncuf64" >:: fun _ ->
        assert_equal (int32 6l) (Eval.cvtop ty TruncUF64 (float64 6.99)) )
    ; ( "test_trunc_sat_f32_s" >:: fun _ ->
        assert_equal (int32 7l) (Eval.cvtop ty Trunc_sat_f32_s (float32 7.5)) )
    ; ( "test_trunc_sat_f32_u" >:: fun _ ->
        assert_equal (int32 8l) (Eval.cvtop ty Trunc_sat_f32_u (float32 8.5)) )
    ; ( "test_trunc_sat_f64_s" >:: fun _ ->
        assert_equal (int32 (-9l))
          (Eval.cvtop ty Trunc_sat_f64_s (float64 (-9.9))) )
    ; ( "test_trunc_sat_f64_u" >:: fun _ ->
        assert_equal (int32 10l) (Eval.cvtop ty Trunc_sat_f64_u (float64 10.9))
      )
    ; ( "test_reinterpret_float" >:: fun _ ->
        assert_equal
          (int32 (Int32.bits_of_float 1.5))
          (Eval.cvtop ty Reinterpret_float (float32 1.5)) )
    ; ( "test_sign_extend" >:: fun _ ->
        assert_equal (int32 (-1l)) (Eval.cvtop ty (Sign_extend 24) (int8 0xff))
      )
    ; ( "test_zero_extend" >:: fun _ ->
        assert_equal (int32 0xffl) (Eval.cvtop ty (Zero_extend 24) (int8 0xff))
      )
    ]
end

module I64Cvtop_test = struct
  let ty = Ty.Ty_bitv 64

  let cvtop =
    [ ( "test_wrap_i64_error" >:: fun _ ->
        assert_type_error @@ fun () ->
        let _ = Eval.cvtop ty WrapI64 (int64 0x1_0000_0000L) in
        () )
    ; ( "test_truncsf32" >:: fun _ ->
        assert_equal (int64 3L) (Eval.cvtop ty TruncSF32 (float32 3.7)) )
    ; ( "test_truncuf32" >:: fun _ ->
        assert_equal (int64 4L) (Eval.cvtop ty TruncUF32 (float32 4.9)) )
    ; ( "test_truncsf64" >:: fun _ ->
        assert_equal (int64 (-5L)) (Eval.cvtop ty TruncSF64 (float64 (-5.2))) )
    ; ( "test_truncuf64" >:: fun _ ->
        assert_equal (int64 6L) (Eval.cvtop ty TruncUF64 (float64 6.99)) )
    ; ( "test_trunc_sat_f32_s" >:: fun _ ->
        assert_equal (int64 7L) (Eval.cvtop ty Trunc_sat_f32_s (float32 7.5)) )
    ; ( "test_trunc_sat_f32_u" >:: fun _ ->
        assert_equal (int64 8L) (Eval.cvtop ty Trunc_sat_f32_u (float32 8.5)) )
    ; ( "test_trunc_sat_f64_s" >:: fun _ ->
        assert_equal (int64 (-9L))
          (Eval.cvtop ty Trunc_sat_f64_s (float64 (-9.9))) )
    ; ( "test_trunc_sat_f64_u" >:: fun _ ->
        assert_equal (int64 10L) (Eval.cvtop ty Trunc_sat_f64_u (float64 10.9))
      )
    ; ( "test_reinterpret_float" >:: fun _ ->
        assert_equal
          (int64 (Int64.bits_of_float 1.5))
          (Eval.cvtop ty Reinterpret_float (float64 1.5)) )
    ; ( "test_sign_extend_i8" >:: fun _ ->
        assert_equal (int64 (-1L)) (Eval.cvtop ty (Sign_extend 56) (int8 (-1)))
      )
    ; ( "test_sign_extend_i32" >:: fun _ ->
        assert_equal (int64 (-1L))
          (Eval.cvtop ty (Sign_extend 32) (int32 (-1l))) )
    ; ( "test_zero_extend" >:: fun _ ->
        assert_equal (int64 0xffffffffL)
          (Eval.cvtop ty (Zero_extend 32) (int32 (-1l))) )
    ]
end

module F32Cvtop_test = struct
  let ty = Ty.Ty_fp 32

  let cvtop =
    [ ( "test_demote_f64" >:: fun _ ->
        let result = Eval.cvtop ty DemoteF64 (float64 3.14) in
        assert_equal (float32 3.14) result )
    ; ( "test_demote_f64_nan" >:: fun _ ->
        let result = Eval.cvtop ty DemoteF64 (float64 nan) in
        assert_equal (float32 nan) result )
    ; ( "test_convert_si32" >:: fun _ ->
        let result = Eval.cvtop ty ConvertSI32 (int32 (-42l)) in
        assert_equal (float32 (-42.)) result )
    ; ( "test_convert_ui32" >:: fun _ ->
        let result = Eval.cvtop ty ConvertUI32 (int32 42l) in
        assert_equal (float32 42.) result;
        let result = Eval.cvtop ty ConvertUI32 (int32 (-1l)) in
        assert_equal (float32 4294967294.) result )
    ; ( "test_convert_si64" >:: fun _ ->
        let result = Eval.cvtop ty ConvertSI64 (int64 (-42L)) in
        assert_equal (float32 (-42.)) result;
        let result = Eval.cvtop ty ConvertSI64 (int64 0x10_0000_0000_0100L) in
        assert_equal (float32 4503599627370512.) result )
    ; ( "test_convert_ui64" >:: fun _ ->
        let result = Eval.cvtop ty ConvertUI64 (int64 42L) in
        assert_equal (float32 42.) result;
        let result = Eval.cvtop ty ConvertUI64 (int64 0x10_0000_0000_0100L) in
        assert_equal (float32 4503599627370512.) result )
    ; ( "test_reinterpret_int" >:: fun _ ->
        let result = Eval.cvtop ty Reinterpret_int (int32 1065353216l) in
        assert_equal (float32 1.) result )
    ; ( "test_promote_f32_error" >:: fun _ ->
        assert_type_error @@ fun () ->
        let _ = Eval.cvtop ty PromoteF32 (float32 42.0) in
        () )
    ]
end

module F64Cvtop_test = struct
  let ty = Ty.Ty_fp 64

  let cvtop =
    [ ( "test_promote_f32" >:: fun _ ->
        let result = Eval.cvtop ty PromoteF32 (float32 42.0) in
        assert_equal (float64 42.0) result )
    ; ( "test_promote_f32_nan" >:: fun _ ->
        let result = Eval.cvtop ty PromoteF32 (float32 nan) in
        assert_equal (float64 nan) result )
    ; ( "test_convert_si32" >:: fun _ ->
        let result = Eval.cvtop ty ConvertSI32 (int32 (-42l)) in
        assert_equal (float64 (-42.)) result )
    ; ( "test_convert_ui32" >:: fun _ ->
        let result = Eval.cvtop ty ConvertUI32 (int32 42l) in
        assert_equal (float64 42.) result )
    ; ( "test_convert_si64" >:: fun _ ->
        let result = Eval.cvtop ty ConvertSI64 (int64 (-42L)) in
        assert_equal (float64 (-42.)) result )
    ; ( "test_convert_ui64" >:: fun _ ->
        let result = Eval.cvtop ty ConvertUI64 (int64 42L) in
        assert_equal (float64 42.) result;
        let result = Eval.cvtop ty ConvertUI64 (int64 (-1L)) in
        assert_equal (float64 (9223372036854775807. *. 2.)) result )
    ; ( "test_reinterpret_int" >:: fun _ ->
        let result =
          Eval.cvtop ty Reinterpret_int (int64 4607182418800017408L)
        in
        assert_equal (float64 1.) result )
    ; ( "test_demote_f64_error" >:: fun _ ->
        assert_type_error @@ fun () ->
        let _ = Eval.cvtop ty DemoteF64 (float64 3.14) in
        () )
    ]
end

let test_unop =
  [ "Int_test.unop" >::: Int_test.unop
  ; "Real_test.unop" >::: Real_test.unop
  ; "Bool_test.unop" >::: Bool_test.unop
  ; "Str_test.unop" >::: Str_test.unop
  ; "F32_test.unop" >::: F32_test.unop
  ; "F64_test.unop" >::: F64_test.unop
  ]

let test_binop =
  [ "Int_test.binop" >::: Int_test.binop
  ; "Real_test.binop" >::: Real_test.binop
  ; "Bool_test.binop" >::: Bool_test.binop
  ; "Str_test.binop" >::: Str_test.binop
  ; "F32_test.binop" >::: F32_test.binop
  ; "F64_test.binop" >::: F64_test.binop
  ]

let test_triop =
  [ "Bool_test.triop" >: Bool_test.triop; "Str_test.triop" >::: Str_test.triop ]

let test_relop =
  [ "Int_test.relop" >::: Int_test.relop
  ; "Real_test.relop" >::: Real_test.relop
  ; "Bool_test.relop" >::: Bool_test.relop
  ; "Str_test.relop" >::: Str_test.relop
  ; "F32_test.relop" >::: F32_test.relop
  ; "F64_test.relop" >::: F64_test.relop
  ]

let test_cvtop =
  [ "Int_test.cvtop" >::: Int_test.cvtop
  ; "Real_test.cvtop" >::: Real_test.cvtop
  ; "Str_test.cvtop" >::: Str_test.cvtop
  ; "I32Cvtop_test.cvtop" >::: I32Cvtop_test.cvtop
  ; "I64Cvtop_test.cvtop" >::: I64Cvtop_test.cvtop
  ; "F32Cvtop_test.cvtop" >::: F32Cvtop_test.cvtop
  ; "F64Cvtop_test.cvtop" >::: F64Cvtop_test.cvtop
  ]

let test_naryop =
  [ "Bool_test.naryop" >: Bool_test.naryop
  ; "Str_test.cvtop" >: Str_test.naryop
  ]

let test_regression = "F32_test.regression" >: F32_test.regression

let test_suite =
  "Eval tests"
  >::: [ "test_unop" >::: test_unop
       ; "test_binop" >::: test_binop
       ; "test_triop" >::: test_triop
       ; "test_relop" >::: test_relop
       ; "test_cvtop" >::: test_cvtop
       ; "test_naryop" >::: test_naryop
       ; "regression" >: test_regression
       ]

let () = run_test_tt_main test_suite

(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2026 formalsec *)
(* Written by the Smtml programmers *)

(** Regression test: large integer values should not overflow 63-bit int. Before
    the Z.t migration, operations on values exceeding 2^62 would silently
    overflow to negative or incorrect results. *)

open Smtml

let int z = Typed.Int.v (Z.of_int z)

let intz z = Typed.Int.v z

let equal a b =
  let result = Typed.Unsafe.unwrap (Typed.Bool.eq a b) in
  match Expr.view result with
  | Expr.Val True -> true
  | Val False -> false
  | _ -> Alcotest.failf "Unexpected result from Bool.eq: %a" Expr.pp result

let int_testable = Alcotest.testable Typed.Int.pp equal

let bool_testable = Alcotest.testable Typed.Bool.pp equal

let big = Z.(pow (of_int 2) 100) (* 2^100 — far exceeds 63-bit int *)

let neg_big = Z.neg big

let huge = Z.(pow (of_int 10) 200) (* 10^200 *)

(* Exponentiation *)
let test_exp_large () =
  let pow = Typed.Int.pow (int 10) (int 100) in
  let z100 = Z.pow (Z.of_int 10) 100 in
  Alcotest.check int_testable "exp 10 100 matches precomputed" (intz z100) pow;
  let le_zero = Typed.Int.le pow (int 0) in
  Alcotest.check bool_testable "exp 10^100 <= 0 => false" Typed.Bool.false_
    le_zero

(* Multiplication *)
let test_mul_large () =
  let a = intz big in
  let two = int 2 in
  let mul = Typed.Int.mul a two in
  let expected = intz (Z.mul big (Z.of_int 2)) in
  Alcotest.check int_testable "2^100 * 2 == 2^101" expected mul;
  let gt_zero = Typed.Int.lt (int 0) mul in
  Alcotest.check bool_testable "2^100 * 2 > 0" Typed.Bool.true_ gt_zero

(* Addition *)
let test_add_large () =
  let a = intz big in
  let b = intz big in
  let add = Typed.Int.add a b in
  let expected = intz (Z.add big big) in
  Alcotest.check int_testable "2^100 + 2^100 == 2^101" expected add

(* Negation *)
let test_neg_large () =
  let neg = Typed.Int.neg (intz big) in
  Alcotest.check int_testable "neg 2^100" (intz neg_big) neg;
  let lt_zero = Typed.Int.lt neg (int 0) in
  Alcotest.check bool_testable "neg 2^100 < 0" Typed.Bool.true_ lt_zero

(* Absolute value *)
let test_abs_large () =
  let abs_pos = Typed.Int.abs (intz big) in
  Alcotest.check int_testable "abs 2^100" (intz big) abs_pos;
  let abs_neg = Typed.Int.abs (intz neg_big) in
  Alcotest.check int_testable "abs (-2^100) == 2^100" (intz big) abs_neg

(* Subtraction producing positive from large values *)
let test_sub_large () =
  let a = intz (Z.add big (Z.of_int 5)) in
  let b = intz big in
  let sub = Typed.Int.sub a b in
  Alcotest.check int_testable "(2^100 + 5) - 2^100 == 5" (int 5) sub

(* Division *)
let test_div_large () =
  let a = intz huge in
  let small = intz (Z.of_int 2) in
  let half = intz (Z.div huge (Z.of_int 2)) in
  let div = Typed.Int.div a small in
  Alcotest.check int_testable "10^200 / 2 == 5*10^199" half div

(* Relational operators on large values *)
let test_relop_large () =
  let a = intz big in
  let b = intz huge in
  Alcotest.check bool_testable "2^100 < 10^200" Typed.Bool.true_
    (Typed.Int.lt a b);
  Alcotest.check bool_testable "2^100 <= 10^200" Typed.Bool.true_
    (Typed.Int.le a b);
  Alcotest.check bool_testable "2^100 == 2^100" Typed.Bool.true_
    (Typed.Int.eq a (intz big));
  Alcotest.check bool_testable "2^100 != 10^200" Typed.Bool.false_
    (Typed.Int.eq a b)

(* Error: negative exponent *)
let test_pow_negative_exponent () =
  let exn =
    Eval.Eval_error (`Unsupported_operator (`Binop Ty.Binop.Pow, Ty_int))
  in
  Alcotest.check_raises "pow with negative exponent raises" exn @@ fun () ->
  let _ = Typed.Int.(pow (v Z.one) (v (Z.neg Z.one))) in
  ()

(* Error: exponent exceeds max_int *)
let test_pow_overflow_exponent () =
  let exn =
    Eval.Eval_error (`Unsupported_operator (`Binop Ty.Binop.Pow, Ty_int))
  in
  let too_large = Z.succ (Z.of_int64 Int64.max_int) in
  Alcotest.check_raises "pow with overflow exponent raises" exn @@ fun () ->
  let _ = Typed.Int.(pow (v (Z.of_int 2)) (v too_large)) in
  ()

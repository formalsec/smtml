open Smtml
open Smtml_test.Test_harness

let pp_op fmt = function
  | `Unop op -> Ty.Unop.pp fmt op
  | `Binop op -> Ty.Binop.pp fmt op
  | `Relop op -> Ty.Relop.pp fmt op
  | `Triop op -> Ty.Triop.pp fmt op
  | `Cvtop op -> Ty.Cvtop.pp fmt op
  | `Naryop op -> Ty.Naryop.pp fmt op

let with_type_error f =
  try f ()
  with Eval.TypeError { index; value; ty; op; _ } ->
    Fmt.failwith
      "type error: operator %a.%a argument %d got unexpected value %a" Ty.pp ty
      pp_op op index Value.pp value

let test_hc () =
  let open Infix in
  let length0 = Expr.Hc.length () in
  let ty = Ty.Ty_bitv 32 in
  assert (symbol "x" ty == symbol "x" ty);
  assert (symbol "x" ty != symbol "y" ty);
  let left_a = symbol "x" ty in
  let right_a = symbol "y" ty in
  let left_b = symbol "x" ty in
  let right_b = symbol "y" ty in
  let a = Expr.binop ty Add left_a right_a in
  let b = Expr.binop ty Add left_b right_b in
  assert (a == b);
  (* There should be only 3 elements added in the hashcons table: *)
  (*   1. x *)
  (*   2. y *)
  (*   3. x + y *)
  assert (Expr.Hc.length () - length0 == 3)

let test_unop_int () =
  let open Infix in
  let ty = Ty.Ty_int in
  assert_equal (Expr.unop ty Neg (int 1)) (int ~-1);
  assert_equal (Expr.unop ty Abs (int ~-1)) (int 1);
  let x = symbol "x" ty in
  assert_equal (Expr.unop ty Neg (Expr.unop ty Neg x)) x

let test_unop_real () =
  let open Infix in
  let ty = Ty.Ty_real in
  assert_equal (Expr.unop ty Neg (real 1.0)) (real (-1.0));
  assert_equal (Expr.unop ty Abs (real 1.0)) (real 1.0);
  assert_equal (Expr.unop ty Sqrt (real 4.0)) (real 2.0);
  assert_equal (Expr.unop ty Nearest (real 0.504)) (real 1.0);
  assert_equal (Expr.unop ty Ceil (real 0.3)) (real 1.0);
  assert_equal (Expr.unop ty Floor (real 0.7)) (real 0.0);
  assert_equal (Expr.unop ty Trunc (real 1.504)) (real 1.0);
  assert_equal (Expr.unop ty Is_nan (real Float.nan)) true_

let test_unop_string () =
  let open Infix in
  let ty = Ty.Ty_str in
  assert_equal (Expr.unop ty Length (string "abc")) (int 3);
  assert_equal (Expr.unop ty Trim (string " abc\t\n")) (string "abc")

let test_unop_bool () =
  let ty = Ty.Ty_bool in
  assert_equal (Expr.unop ty Not Expr.Bool.true_) Expr.Bool.false_;
  let x = Expr.symbol (Symbol.make ty "x") in
  assert_equal (Expr.unop ty Not (Expr.unop ty Not x)) x

let test_unop_list () =
  let open Infix in
  let ty = Ty.Ty_list in
  let vlist = list [ Int 1; Int 2; Int 3 ] in
  let x = symbol "x" Ty_int in
  let y = symbol "y" Ty_int in
  let slist = Expr.make (List [ x; y ]) in
  assert_equal (Expr.unop ty Head vlist) (int 1);
  assert_equal (Expr.unop ty Tail vlist) (list [ Int 2; Int 3 ]);
  assert_equal (Expr.unop ty Length vlist) (int 3);
  assert_equal (Expr.unop ty Reverse vlist) (list [ Int 3; Int 2; Int 1 ]);
  assert_equal (Expr.unop ty Head slist) x;
  assert_equal (Expr.unop ty Tail slist) (Expr.make (List [ y ]));
  assert_equal (Expr.unop ty Length slist) (int 2);
  assert_equal (Expr.unop ty Reverse (Expr.unop ty Reverse slist)) slist

let test_unop_i32 () =
  let open Infix in
  let ty = Ty.Ty_bitv 32 in
  assert_equal (Expr.unop ty Neg (int32 1l)) (int32 (-1l));
  assert_equal (Expr.unop ty Not (int32 (-1l))) (int32 0l)

let test_unop_i64 () =
  let open Infix in
  let ty = Ty.Ty_bitv 64 in
  assert_equal (Expr.unop ty Neg (int64 1L)) (int64 (-1L));
  assert_equal (Expr.unop ty Not (int64 (-1L))) (int64 0L)

(* f32 *)
let test_unop_f32 () =
  let open Infix in
  let ty = Ty.Ty_fp 32 in
  assert_equal (Expr.unop ty Trunc (float32 0.75)) (float32 0.0)

(* f64 *)
let test_unop_f64 () =
  let open Infix in
  let ty = Ty.Ty_fp 64 in
  assert_equal (Expr.unop ty Trunc (float64 0.75)) (float64 0.0)

let test_unop () =
  test_unop_int ();
  test_unop_real ();
  test_unop_string ();
  test_unop_bool ();
  test_unop_list ();
  test_unop_i32 ();
  test_unop_i64 ();
  test_unop_f32 ();
  test_unop_f64 ()

let test_binop_int () =
  let open Infix in
  let ty = Ty.Ty_int in
  assert_equal (Expr.binop ty Add (int 0) (int 42)) (int 42);
  assert_equal (Expr.binop ty Sub (int 0) (int 1)) (int (-1));
  assert_equal (Expr.binop ty Mul (int 2) (int 21)) (int 42);
  assert_equal (Expr.binop ty Div (int 84) (int 2)) (int 42);
  assert_equal (Expr.binop ty Rem (int 0) (int 1)) (int 0);
  assert_equal (Expr.binop ty Pow (int 2) (int 2)) (int 4);
  assert_equal (Expr.binop ty Min (int 2) (int 4)) (int 2);
  assert_equal (Expr.binop ty Max (int 2) (int 4)) (int 4);
  assert_equal (Expr.binop ty And (int 1) (int 0)) (int 0);
  assert_equal (Expr.binop ty Or (int 0) (int 1)) (int 1);
  assert_equal (Expr.binop ty Xor (int 1) (int 1)) (int 0);
  assert_equal (Expr.binop ty Shl (int 1) (int 2)) (int 4);
  assert_equal (Expr.binop ty ShrA (int 4) (int 2)) (int 1);
  assert_equal (Expr.binop ty ShrA (int (-4)) (int 2)) (int (-1))
(* assert_equal (Expr.binop ty ShrL (int (-4)) (int 2)) (int (-1)) *)

let test_binop_real () =
  let open Infix in
  let ty = Ty.Ty_real in
  assert_equal (Expr.binop ty Add (real 0.0) (real 42.0)) (real 42.0);
  assert_equal (Expr.binop ty Sub (real 0.0) (real 1.0)) (real (-1.0));
  assert_equal (Expr.binop ty Mul (real 2.0) (real 21.0)) (real 42.0);
  assert_equal (Expr.binop ty Div (real 84.0) (real 2.0)) (real 42.0);
  assert_equal (Expr.binop ty Rem (real 0.0) (real 1.0)) (real 0.0);
  assert_equal (Expr.binop ty Min (real 2.0) (real 4.0)) (real 2.0);
  assert_equal (Expr.binop ty Max (real 2.0) (real 4.0)) (real 4.0)

let test_binop_string () =
  let open Infix in
  let ty = Ty.Ty_str in
  assert_equal (Expr.binop ty At (string "abc") (int 0)) (string "a");
  assert_equal (Expr.binop ty String_prefix (string "ab") (string "abcd")) true_;
  assert_equal
    (Expr.binop ty String_suffix (string "ab") (string "abcd"))
    false_;
  assert_equal
    (Expr.binop ty String_contains (string "abcd") (string "bc"))
    true_

let test_binop_list () =
  let open Infix in
  let ty = Ty.Ty_list in
  let clist = list [ Int 0; Int 1; Int 2 ] in
  assert_equal (Expr.binop Ty_list At clist (int 0)) (int 0);
  assert_equal
    (Expr.binop Ty_list List_cons (int 0) (list [ Int 1; Int 2 ]))
    clist;
  assert_equal
    (Expr.binop Ty_list List_append (list [ Int 0; Int 1 ]) (list [ Int 2 ]))
    clist;
  let slist2 = Expr.make (List [ int 0; int 1 ]) in
  let slist3 = Expr.make (List [ int 0; int 1; int 2 ]) in
  assert_equal (Expr.binop ty At slist3 (int 0)) (int 0);
  assert_equal (Expr.binop ty List_append slist2 (list [ Int 2 ])) slist3;
  assert_equal
    (Expr.binop ty List_cons (int 0) (Expr.make (List [ int 1; int 2 ])))
    slist3

let test_binop_i32 () =
  let open Infix in
  let ptr = Expr.ptr 8390670l (int32 2l) in
  assert_equal (Expr.binop (Ty_bitv 32) Rem ptr (int32 1l)) (int32 0l);
  assert_equal (Expr.binop (Ty_bitv 32) Add (int32 0l) (int32 1l)) (int32 1l);
  assert_equal (Expr.binop (Ty_bitv 32) Sub (int32 1l) (int32 0l)) (int32 1l);
  let x = Expr.binop (Ty_bitv 32) Mul (int32 2l) (int32 2l) in
  assert_equal (Expr.binop (Ty_bitv 32) Div x (int32 2l)) (int32 2l);
  assert_equal (Expr.binop (Ty_bitv 32) Rem (int32 10l) (int32 7l)) (int32 3l);
  assert_equal (Expr.binop (Ty_bitv 32) And (int32 1l) (int32 0l)) (int32 0l);
  assert_equal (Expr.binop (Ty_bitv 32) Or (int32 0l) (int32 1l)) (int32 1l);
  assert_equal (Expr.binop (Ty_bitv 32) Xor (int32 1l) (int32 1l)) (int32 0l);
  assert_equal (Expr.binop (Ty_bitv 32) Shl (int32 1l) (int32 2l)) (int32 4l);
  assert_equal (Expr.binop (Ty_bitv 32) ShrA (int32 4l) (int32 2l)) (int32 1l);
  assert_equal
    (Expr.binop (Ty_bitv 32) Rotl (int32 Int32.min_int) (int32 2l))
    (int32 2l);
  assert_equal
    Expr.(binop (Ty_bitv 32) Rotr (int32 2l) (int32 2l))
    (int32 Int32.min_int)

let test_binop_i64 () =
  let open Infix in
  assert_equal (Expr.binop (Ty_bitv 64) Add (int64 0L) (int64 1L)) (int64 1L);
  assert_equal (Expr.binop (Ty_bitv 64) Sub (int64 1L) (int64 0L)) (int64 1L);
  let x = Expr.binop (Ty_bitv 64) Mul (int64 2L) (int64 2L) in
  assert_equal (Expr.binop (Ty_bitv 64) Div x (int64 2L)) (int64 2L);
  assert_equal (Expr.binop (Ty_bitv 64) Rem (int64 10L) (int64 7L)) (int64 3L);
  assert_equal (Expr.binop (Ty_bitv 64) And (int64 1L) (int64 0L)) (int64 0L);
  assert_equal (Expr.binop (Ty_bitv 64) Or (int64 0L) (int64 1L)) (int64 1L);
  assert_equal (Expr.binop (Ty_bitv 64) Xor (int64 1L) (int64 1L)) (int64 0L);
  assert_equal (Expr.binop (Ty_bitv 64) Shl (int64 1L) (int64 2L)) (int64 4L);
  assert_equal (Expr.binop (Ty_bitv 64) ShrA (int64 4L) (int64 2L)) (int64 1L);
  assert_equal
    (Expr.binop (Ty_bitv 64) Rotl (int64 Int64.min_int) (int64 2L))
    (int64 2L);
  assert_equal
    (Expr.binop (Ty_bitv 64) Rotr (int64 2L) (int64 2L))
    (int64 Int64.min_int)

let test_binop_f32 () =
  let open Infix in
  let ty = Ty.Ty_fp 32 in
  assert_equal
    (Expr.binop ty Copysign (float32 (-4.2)) (float32 2.0))
    (float32 4.2);
  assert_equal
    (Expr.binop ty Copysign (float32 4.2) (float32 (-2.0)))
    (float32 (-4.2));
  assert_equal
    (Expr.binop ty Copysign (float32 4.2) (float32 2.0))
    (float32 4.2);
  assert_equal
    (Expr.binop ty Copysign (float32 (-4.2)) (float32 (-2.0)))
    (float32 (-4.2))

let test_binop_f64 () =
  let open Infix in
  let ty = Ty.Ty_fp 64 in
  assert_equal
    (Expr.binop ty Copysign (float64 (-4.2)) (float64 2.0))
    (float64 4.2);
  assert_equal
    (Expr.binop ty Copysign (float64 4.2) (float64 (-2.0)))
    (float64 (-4.2));
  assert_equal
    (Expr.binop ty Copysign (float64 4.2) (float64 2.0))
    (float64 4.2);
  assert_equal
    (Expr.binop ty Copysign (float64 (-4.2)) (float64 (-2.0)))
    (float64 (-4.2))

let test_binop_ptr () =
  let open Infix in
  let p0 = Expr.ptr 0l (int32 0l) in
  let p1 = Expr.binop (Ty_bitv 32) Add p0 (int32 4l) in
  assert_equal p1 (Expr.ptr 0l (int32 4l));
  assert_equal (Expr.binop (Ty_bitv 32) Sub p1 p0) (int32 4l);
  assert_equal (Expr.binop (Ty_bitv 32) Sub p1 (int32 4l)) p0;
  assert_equal (Expr.binop (Ty_bitv 32) Add (int32 4l) p0) p1

let test_binop_simplifications () =
  let open Infix in
  let x = symbol "x" (Ty_bitv 32) in
  let zero = int32 0l in
  let binop32 = Expr.binop (Ty_bitv 32) in
  assert_equal (binop32 And x zero = zero && binop32 And zero x = zero) true_;
  assert_equal (binop32 Or zero x = x) (binop32 Or x zero = x);
  assert_equal
    (binop32 Sub (binop32 Sub x (int32 1l)) (int32 1l))
    (binop32 Sub x (int32 2l));
  assert_equal
    (binop32 Mul (binop32 Mul x (int32 2l)) (int32 2l))
    (binop32 Mul x (int32 4l));
  assert_equal
    (binop32 Mul (int32 2l) (binop32 Mul x (int32 2l)))
    (binop32 Mul (int32 4l) x)

let test_binop () =
  test_binop_int ();
  test_binop_real ();
  test_binop_string ();
  test_binop_list ();
  test_binop_i32 ();
  test_binop_i64 ();
  test_binop_f32 ();
  test_binop_f64 ();
  test_binop_ptr ();
  test_binop_simplifications ()

let test_relop_bool () =
  let open Infix in
  let ty = Ty.Ty_bool in
  assert_equal (Expr.relop ty Eq (int 0) (int 0)) true_;
  assert_equal (Expr.relop ty Ne (int 0) (int 0)) false_;
  assert_equal (Expr.relop ty Eq (real 0.0) (real 0.0)) true_;
  assert_equal (Expr.relop ty Ne (real 0.0) (real 0.0)) false_;
  assert_equal (Expr.relop ty Eq (int32 0l) (int32 0l)) true_;
  assert_equal (Expr.relop ty Ne (int32 0l) (int32 0l)) false_;
  assert_equal (Expr.relop ty Eq (int64 0L) (int64 0L)) true_;
  assert_equal (Expr.relop ty Ne (int64 0L) (int64 0L)) false_

let test_relop_int () =
  let open Infix in
  let ty = Ty.Ty_int in
  assert_equal (Expr.relop ty Lt (int 0) (int 1)) true_;
  assert_equal (Expr.relop ty Le (int 0) (int 1)) true_;
  assert_equal (Expr.relop ty Gt (int 0) (int 1)) false_;
  assert_equal (Expr.relop ty Ge (int 0) (int 1)) false_

let test_relop_real () =
  let open Infix in
  let ty = Ty.Ty_real in
  let x = symbol "x" ty in
  assert_equal (Expr.relop Ty_bool Ne (real Float.nan) x) true_;
  assert_equal (Expr.relop Ty_bool Eq x (real Float.nan)) false_;
  assert_equal (Expr.relop ty Lt (real 0.0) (real 1.0)) true_;
  assert_equal (Expr.relop ty Le (real 0.0) (real 1.0)) true_;
  assert_equal (Expr.relop ty Gt (real 0.0) (real 1.0)) false_;
  assert_equal (Expr.relop ty Ge (real 0.0) (real 1.0)) false_

let test_relop_string () =
  let open Infix in
  let ty = Ty.Ty_str in
  assert_equal (Expr.relop ty Lt (string "a") (string "b")) true_;
  assert_equal (Expr.relop ty Le (string "a") (string "b")) true_;
  assert_equal (Expr.relop ty Gt (string "a") (string "b")) false_;
  assert_equal (Expr.relop ty Ge (string "a") (string "b")) false_;
  assert_equal (Expr.relop ty Eq (string "a") (string "a")) true_;
  assert_equal (Expr.relop ty Ne (string "a") (string "a")) false_;
  assert_equal (Expr.relop ty Eq (string "a") (string "b")) false_;
  assert_equal (Expr.relop ty Ne (string "a") (string "b")) true_

let test_relop_i32 () =
  let open Infix in
  let ty = Ty.Ty_bitv 32 in
  assert_equal (Expr.relop ty Lt (int32 0l) (int32 1l)) true_;
  assert_equal (Expr.relop ty LtU (int32 (-1l)) (int32 0l)) false_;
  assert_equal (Expr.relop ty Le (int32 0l) (int32 1l)) true_;
  assert_equal (Expr.relop ty LeU (int32 (-1l)) (int32 0l)) false_;
  assert_equal (Expr.relop ty Gt (int32 1l) (int32 0l)) true_;
  assert_equal (Expr.relop ty GtU (int32 0l) (int32 (-1l))) false_;
  assert_equal (Expr.relop ty Ge (int32 1l) (int32 0l)) true_;
  assert_equal (Expr.relop ty GeU (int32 0l) (int32 (-1l))) false_

let test_relop_i64 () =
  let open Infix in
  let ty = Ty.Ty_bitv 64 in
  assert_equal (Expr.relop ty Lt (int64 0L) (int64 1L)) true_;
  assert_equal (Expr.relop ty LtU (int64 (-1L)) (int64 0L)) false_;
  assert_equal (Expr.relop ty Le (int64 0L) (int64 1L)) true_;
  assert_equal (Expr.relop ty LeU (int64 (-1L)) (int64 0L)) false_;
  assert_equal (Expr.relop ty Gt (int64 1L) (int64 0L)) true_;
  assert_equal (Expr.relop ty GtU (int64 0L) (int64 (-1L))) false_;
  assert_equal (Expr.relop ty Ge (int64 1L) (int64 0L)) true_;
  assert_equal (Expr.relop ty GeU (int64 0L) (int64 (-1L))) false_

let test_relop_f32 () =
  let open Infix in
  let ty = Ty.Ty_fp 32 in
  let nan0 = float32 Float.nan in
  let nan1 = float32 Float.nan in
  (* Structual equaility should say nan = nan *)
  assert_equal nan0 nan1;
  (* Concrete evaluation should say nan <> nan *)
  assert_equal (Expr.relop ty Eq nan0 nan1) false_;
  assert_equal (Expr.relop ty Lt (float32 0.0) (float32 1.0)) true_;
  assert_equal (Expr.relop ty Le (float32 0.0) (float32 1.0)) true_;
  assert_equal (Expr.relop ty Gt (float32 0.0) (float32 1.0)) false_;
  assert_equal (Expr.relop ty Ge (float32 0.0) (float32 1.0)) false_

let test_relop_f64 () =
  let open Infix in
  let ty = Ty.Ty_fp 64 in
  assert_equal (Expr.relop ty Lt (float64 0.0) (float64 1.0)) true_;
  assert_equal (Expr.relop ty Le (float64 0.0) (float64 1.0)) true_;
  assert_equal (Expr.relop ty Gt (float64 0.0) (float64 1.0)) false_;
  assert_equal (Expr.relop ty Ge (float64 0.0) (float64 1.0)) false_

let test_relop_app () =
  let open Infix in
  let ty = Ty.Ty_bool in
  assert_equal
    (Expr.relop ty Eq (app (`Op "undefined")) (app (`Op "undefined")))
    Expr.Bool.true_;
  assert_equal
    (Expr.relop ty Ne (app (`Op "undefined")) (app (`Op "undefined")))
    Expr.Bool.false_;
  assert_equal
    (Expr.relop ty Eq (app (`Op "undefined")) (int 1))
    Expr.Bool.false_;
  assert_equal
    (Expr.relop ty Ne (int 1) (app (`Op "undefined")))
    Expr.Bool.true_

let test_relop_ptr () =
  let open Infix in
  let ty = Ty.Ty_bitv 32 in
  let p0 = Expr.ptr 0l (int32 0l) in
  let p1 = Expr.ptr 4l (int32 0l) in
  assert_equal (Expr.relop Ty_bool Eq p0 p0) true_;
  assert_equal (Expr.relop Ty_bool Eq p0 p1) false_;
  assert_equal (Expr.relop Ty_bool Ne p0 p0) false_;
  assert_equal (Expr.relop Ty_bool Ne p0 p1) true_;
  assert_equal (Expr.relop ty LtU p0 p0) false_;
  assert_equal (Expr.relop ty LeU p0 p1) true_;
  assert_equal (Expr.relop ty GeU p0 p0) true_;
  assert_equal (Expr.relop ty GtU p0 p1) false_;
  assert_equal (Expr.relop ty Le p0 (int32 4l)) true_;
  assert_equal (Expr.relop ty Lt (int32 4l) p0) false_;
  assert_equal (Expr.relop ty Gt p1 (int32 4l)) false_;
  assert_equal (Expr.relop ty Ge (int32 4l) p1) true_

let test_relop () =
  test_relop_bool ();
  test_relop_int ();
  test_relop_real ();
  test_relop_string ();
  test_relop_i32 ();
  test_relop_i64 ();
  test_relop_f32 ();
  test_relop_f64 ();
  test_relop_app ();
  test_relop_ptr ()

let test_triop_bool () =
  let open Infix in
  let ty = Ty.Ty_bool in
  assert_equal (Expr.triop ty Ite true_ (int 1) (int 0)) (int 1);
  assert_equal (Expr.triop ty Ite false_ (int 1) (int 0)) (int 0)

let test_triop_string () =
  let open Infix in
  let ty = Ty.Ty_str in
  assert_equal
    (Expr.triop ty String_extract (string "abcd") (int 1) (int 2))
    (string "bc");
  assert_equal
    (Expr.triop ty String_index (string "abcd") (string "bc") (int 0))
    (int 1);
  assert_equal
    (Expr.triop ty String_replace (string "abcd") (string "bc") (string "ef"))
    (string "aefd")

let test_triop_list () =
  let open Infix in
  let ty = Ty.Ty_list in
  assert_equal
    (Expr.triop ty List_set (list [ Int 0; Int 1; Int 2 ]) (int 1) (int 3))
    (list [ Int 0; Int 3; Int 2 ])

let test_triop () =
  test_triop_bool ();
  test_triop_string ();
  test_triop_list ()

let test_cvtop_int () =
  let open Infix in
  let ty = Ty.Ty_int in
  assert_equal (Expr.cvtop ty OfBool true_) (int 1);
  assert_equal (Expr.cvtop ty OfBool false_) (int 0);
  assert_equal (Expr.cvtop ty Reinterpret_float (real 1.)) (int 1);
  assert_equal (Expr.cvtop ty ToString (int 1)) (string "1")

let test_cvtop_real () =
  let open Infix in
  let ty = Ty.Ty_real in
  assert_equal (Expr.cvtop ty ToString (real 1.)) (string "1.");
  assert_equal (Expr.cvtop ty OfString (string "1.")) (real 1.);
  assert_equal (Expr.cvtop ty Reinterpret_int (int 1)) (real 1.)

let test_cvtop_string () =
  let open Infix in
  let ty = Ty.Ty_str in
  assert_equal (Expr.cvtop ty String_to_code (string "a")) (int 97);
  assert_equal (Expr.cvtop ty String_from_code (int 97)) (string "a");
  assert_equal (Expr.cvtop ty String_to_int (string "42")) (int 42);
  assert_equal (Expr.cvtop ty String_from_int (int 42)) (string "42");
  assert_equal (Expr.cvtop ty String_to_float (string "1.")) (real 1.)

let test_cvtop_i32 () =
  let open Infix in
  assert_equal (Expr.cvtop (Ty_bitv 32) TruncSF32 (float32 8.5)) (int32 8l);
  assert_equal (Expr.cvtop (Ty_bitv 32) TruncSF64 (float64 8.5)) (int32 8l);
  let x = Expr.symbol (Symbol.make (Ty_bitv 32) "x") in
  let x = Expr.extract x ~high:2 ~low:0 in
  assert (Ty.equal (Expr.ty x) (Ty_bitv 16));
  let x = Expr.cvtop (Ty_bitv 32) (Sign_extend 16) x in
  assert (Ty.equal (Expr.ty x) (Ty_bitv 32))

let test_cvtop_i64 () =
  let open Infix in
  assert_equal (Expr.cvtop (Ty_bitv 64) TruncSF32 (float32 8.5)) (int64 8L);
  assert_equal (Expr.cvtop (Ty_bitv 64) TruncSF64 (float64 8.5)) (int64 8L)

let test_cvtop_f32 () =
  let open Infix in
  let ty = Ty.Ty_fp 32 in
  assert_equal (Expr.cvtop ty ConvertSI32 (int32 8l)) (float32 8.0);
  assert_equal (Expr.cvtop ty ConvertSI64 (int64 8L)) (float32 8.0)

let test_cvtop_f64 () =
  let open Infix in
  let ty = Ty.Ty_fp 64 in
  assert_equal (Expr.cvtop ty ConvertSI32 (int32 8l)) (float64 8.0);
  assert_equal (Expr.cvtop ty ConvertSI64 (int64 8L)) (float64 8.0)

let test_cvtop () =
  test_cvtop_int ();
  test_cvtop_real ();
  test_cvtop_string ();
  test_cvtop_i32 ();
  test_cvtop_i64 ();
  test_cvtop_f32 ();
  test_cvtop_f64 ()

let test_naryop_bool () =
  let open Infix in
  let ty = Ty.Ty_bool in
  assert_equal (Expr.naryop ty Logand [ true_; true_; true_ ]) true_;
  assert_equal (Expr.naryop ty Logor [ false_; false_; false_ ]) false_;
  assert_equal (Expr.naryop ty Logand [ true_; false_; true_ ]) false_;
  assert_equal (Expr.naryop ty Logor [ false_; true_; false_ ]) true_

let test_naryop_string () =
  let open Infix in
  let ty = Ty.Ty_str in
  assert_equal
    (Expr.naryop ty Concat [ string "a"; string "b"; string "c" ])
    (string "abc");
  assert_equal (Expr.naryop ty Concat [ string "abc" ]) (string "abc");
  assert_equal (Expr.naryop ty Concat [ string ""; string "" ]) (string "")

let test_naryop () =
  test_naryop_bool ();
  test_naryop_string ()

let test_simplify_assoc () =
  (* Test simplify of left- and righ- associative operators *)
  let open Infix in
  let ty = Ty.Ty_int in
  let x = symbol "x" ty in
  let binary = Expr.binop' Ty_int Add x (int 10) in
  let sym = Expr.binop' Ty_int Add binary (int 3) in
  assert_equal (Expr.simplify sym) (Expr.binop' Ty_int Add x (int 13));
  let binary = Expr.binop' Ty_int Add x (int 10) in
  let sym = Expr.binop' Ty_int Add (int 3) binary in
  assert_equal (Expr.simplify sym) (Expr.binop' Ty_int Add (int 13) x)

let test_simplify_concat () =
  (* Test Concat of Extracts simplifications *)
  let open Infix in
  let x = symbol "x" (Ty_bitv 32) in
  let b0 = Expr.extract' x ~high:1 ~low:0 in
  let b1 = Expr.extract' x ~high:2 ~low:1 in
  let b2 = Expr.extract' x ~high:3 ~low:2 in
  let b3 = Expr.extract' x ~high:4 ~low:3 in
  let b3210 = Expr.concat' b3 (Expr.concat' b2 (Expr.concat' b1 b0)) in
  assert_equal x (Expr.simplify b3210)

let test_simplify () =
  test_simplify_assoc ();
  test_simplify_concat ()

let () =
  with_type_error @@ fun () ->
  test_hc ();
  test_unop ();
  test_binop ();
  test_relop ();
  test_triop ();
  test_cvtop ();
  test_naryop ();
  test_simplify ()

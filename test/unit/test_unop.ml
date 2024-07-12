open Smtml
open Ty
open Expr
open Value

let ( = ) = Expr.equal

(* int *)
let () =
  let ty = Ty_int in
  let v i = value (Int i) in
  assert (unop ty Neg (v 1) = v ~-1);
  assert (unop ty Abs (v ~-1) = v 1);
  assert (
    let x = symbol (Symbol.make ty "x") in
    Expr.equal (unop ty Neg (unop ty Neg x)) x )

(* real *)
let () =
  let ty = Ty_real in
  let v f = value (Real f) in
  assert (unop ty Neg (v 1.0) = v (-1.0));
  assert (unop ty Abs (v 1.0) = v 1.0);
  assert (unop ty Sqrt (v 4.0) = v 2.0);
  assert (unop ty Nearest (v 0.504) = v 1.0);
  assert (unop ty Ceil (v 0.3) = v 1.0);
  assert (unop ty Floor (v 0.7) = v 0.0);
  assert (unop ty Trunc (v 1.504) = v 1.0);
  assert (unop ty Is_nan (v Float.nan) = value True)

(* string*)
let () =
  let ty = Ty_str in
  let v s = value (Str s) in
  assert (unop ty Length (v "abc") = value (Int 3));
  assert (unop ty String_trim (v " abc\t\n") = value (Str "abc"))

(* bool *)
let () =
  let ty = Ty_bool in
  assert (unop ty Not Bool.true_ = Bool.false_);
  assert (
    let x = Expr.symbol (Symbol.make ty "x") in
    Expr.equal (unop ty Not (unop ty Not x)) x )

(* list *)
let () =
  let ty = Ty_list in
  let v l = value (List l) in
  let vlist = v [ Int 1; Int 2; Int 3 ] in
  let x = Expr.symbol (Symbol.make Ty_int "x") in
  let y = Expr.symbol (Symbol.make Ty_int "y") in
  let slist = Expr.make (List [ x; y ]) in
  assert (unop ty List_head vlist = value (Int 1));
  assert (Expr.equal (unop ty List_tail vlist) (v [ Int 2; Int 3 ]));
  assert (Expr.equal (unop ty Length vlist) (value (Int 3)));
  assert (
    Expr.equal
      (unop ty List_reverse vlist)
      (value (List [ Int 3; Int 2; Int 1 ])) );
  assert (Expr.equal (unop ty List_head slist) x);
  assert (Expr.equal (unop ty List_tail slist) (Expr.make (List [ y ])));
  assert (Expr.equal (unop ty Length slist) (value (Int 2)));
  assert (
    Expr.equal (unop ty List_reverse (unop Ty_list List_reverse slist)) slist )

(* i32 *)
let () =
  let ty = Ty_bitv 32 in
  let v i = value (Num (I32 i)) in
  assert (unop ty Neg (v 1l) = v (-1l));
  assert (unop ty Not (v (-1l)) = v 0l)

(* i64 *)
let () =
  let ty = Ty_bitv 64 in
  let v i = value (Num (I64 i)) in
  assert (unop ty Neg (v 1L) = v (-1L));
  assert (unop ty Not (v (-1L)) = v 0L)

(* f32 *)
let () =
  let ty = Ty_fp 32 in
  let v f = value (Num (F32 (Int32.bits_of_float f))) in
  assert (unop ty Trunc (v 0.75) = v 0.0)

(* f64 *)
let () =
  let ty = Ty_fp 64 in
  let v f = value (Num (F64 (Int64.bits_of_float f))) in
  assert (unop ty Trunc (v 0.75) = v 0.0)

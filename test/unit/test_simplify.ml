open Smtml
open Expr
module I8 = Bitv.I8
module I32 = Bitv.I32
module I64 = Bitv.I64

(* Test concrete simplification *)
let () =
  assert (
    let unary = unop' (Ty_bitv 32) Neg (I32.v 1l) in
    simplify unary = I32.v (-1l) );
  assert (
    let binary = binop' (Ty_bitv 32) Add (I32.v 1l) (I32.v 1l) in
    simplify binary = I32.v 2l );
  assert (
    let triop = triop' Ty_bool Ite Bool.true_ (I32.v 1l) (I32.v 0l) in
    simplify triop = I32.v 1l );
  assert (
    let relop = relop' (Ty_bitv 32) Lt (I32.v 2l) (I32.v 1l) in
    simplify relop = make @@ Val False );
  assert (
    let cvtop = cvtop' (Ty_bitv 32) WrapI64 (I64.v 1L) in
    simplify cvtop = I32.v 1l )

(* Test simplify of left- and righ- associative operators *)
let () =
  let x = symbol (Symbol.make Ty_int "x") in
  let v i = value (Int i) in
  assert (
    let binary = binop' Ty_int Add x (v 10) in
    let sym = binop' Ty_int Add binary (v 3) in
    simplify sym = binop' Ty_int Add x (v 13) );
  assert (
    let binary = binop' Ty_int Add x (v 10) in
    let sym = binop' Ty_int Add (v 3) binary in
    simplify sym = binop' Ty_int Add (v 13) x )

(* Test Concat of Extracts simplifications *)
let () =
  assert (
    let x = I32.sym "x" in
    let b0 = extract' x ~high:1 ~low:0 in
    let b1 = extract' x ~high:2 ~low:1 in
    let b2 = extract' x ~high:3 ~low:2 in
    let b3 = extract' x ~high:4 ~low:3 in
    let b3210 = concat' b3 (concat' b2 (concat' b1 b0)) in
    x = simplify b3210 )

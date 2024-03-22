open Encoding
open Expr
module I8 = Bitv.I8
module I32 = Bitv.I32
module I64 = Bitv.I64

(* Test concrete simplification *)
let () =
  assert (
    let unary = make @@ Unop (Ty_bitv 32, Neg, I32.v 1l) in
    simplify unary = I32.v (Int32.neg 1l) );
  assert (
    let binary = make @@ Binop (Ty_bitv 32, Add, I32.v 1l, I32.v 1l) in
    simplify binary = I32.v 2l );
  assert (
    let triop = make @@ Triop (Ty_bool, Ite, Bool.v true, I32.v 1l, I32.v 0l) in
    simplify triop = I32.v 1l );
  assert (
    let relop = make @@ Relop (Ty_bitv 32, Lt, I32.v 2l, I32.v 1l) in
    simplify relop = make @@ Val False );
  assert (
    let cvtop = make @@ Cvtop (Ty_bitv 32, WrapI64, I64.v 1L) in
    simplify cvtop = I32.v 1l );
  assert (
    let x = mk_symbol (Symbol.make Ty_int "x") in
    let binary = make @@ Binop (Ty_int, Add, x, (make @@ Val (Int 10))) in
    let sym = make @@ Binop (Ty_int, Add, binary, (make @@ Val (Int 3))) in
    simplify sym = (make @@ Binop (Ty_int, Add, x, (make @@ Val (Int 13)))))

(* Test Concat of Extracts simplifications *)
let () =
  assert (
    let x = I32.sym "x" in
    let b0 = make @@ Extract (x, 1, 0) in
    let b1 = make @@ Extract (x, 2, 1) in
    let b2 = make @@ Extract (x, 3, 2) in
    let b3 = make @@ Extract (x, 4, 3) in
    let b3210 = Concat (b3, make @@ Concat (b2, make @@ Concat (b1, b0))) in
    x = simplify (make b3210) )

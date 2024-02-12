open Encoding
open Expr
module I8 = Bitv.I8
module I32 = Bitv.I32
module I64 = Bitv.I64

(* Test concrete simplification *)
let () =
  assert (
    let unary = mk @@ Unop (Ty_bitv 32, Neg, I32.v 1l) in
    simplify unary = I32.v (Int32.neg 1l) );
  assert (
    let binary = mk @@ Binop (Ty_bitv 32, Add, I32.v 1l, I32.v 1l) in
    simplify binary = I32.v 2l );
  assert (
    let triop = mk @@ Triop (Ty_bool, Ite, Bool.v true, I32.v 1l, I32.v 0l) in
    simplify triop = I32.v 1l );
  assert (
    let relop = mk @@ Relop (Ty_bitv 32, Lt, I32.v 2l, I32.v 1l) in
    simplify relop = mk @@ Val False );
  assert (
    let cvtop = mk @@ Cvtop (Ty_bitv 32, WrapI64, I64.v 1L) in
    simplify cvtop = I32.v 1l )

(* Test Concat of Extracts simplifications *)
let () =
  assert (
    let x = I32.sym "x" in
    let b0 = mk @@ Extract (x, 1, 0) in
    let b1 = mk @@ Extract (x, 2, 1) in
    let b2 = mk @@ Extract (x, 3, 2) in
    let b3 = mk @@ Extract (x, 4, 3) in
    x = simplify @@ mk @@ Concat [ b3; b2; b1; b0 ] );
  assert (
    let v0 = I8.v 0xbe in
    let v1 = I8.v 0xba in
    let v2 = I8.v 0xef in
    let v3 = I8.v 0xbe in
    I32.v 0xbeefbabel = simplify @@ mk @@ Concat [ v3; v2; v1; v0 ])

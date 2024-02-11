open Encoding
open Expr
open Ty

let () =
  let module I32 = Bitv.I32 in
  assert (I32.sym "x" == I32.sym "x");
  assert (I32.sym "x" != I32.sym "y");
  let left_a = I32.sym "x" in
  let right_a = I32.sym "y" in
  let left_b = I32.sym "x" in
  let right_b = I32.sym "y" in
  let a = mk @@ Binop (Ty_bitv S32, Add, left_a, right_a) in
  let b = mk @@ Binop (Ty_bitv S32, Add, left_b, right_b) in
  assert (a == b);
  (*
     There should be only 3 elements in the hashcons table:
       1. x
       2. y
       3. x + y
  *)
  assert (Expr.Hc.length () = 3)

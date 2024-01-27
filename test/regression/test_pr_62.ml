open Encoding
open Expr
module I32 = Bitv.I32

let () =
  let ptr = Expr.mk @@ Ptr (8390670l, I32.v 2l) in
  let rem = Expr.mk @@ Binop (Ty_bitv S32, Rem, ptr, I32.v 1l) in
  let result = Expr.simplify rem in
  assert (result = I32.v 0l)

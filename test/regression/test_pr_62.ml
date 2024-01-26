open Encoding
open Expr
module I32 = Bitv.I32

let () =
  let ptr = Ptr (8390670l, I32.v 2l) @: Ty_bitv S32 in
  let rem = Binop (Rem, ptr, I32.v 1l) @: Ty_bitv S32 in
  let result = Expr.simplify rem in
  assert (result = I32.v 0l)

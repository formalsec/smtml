open Encoding
open Ty
open Encoding.Expr

let () =
  let ptr = make @@ Ptr (8390670l, Expr.Bitv.I32.v 2l) in
  let rem = make @@ Binop (Ty_bitv 32, Rem, ptr, Expr.Bitv.I32.v 1l) in
  let result = Expr.simplify rem in
  assert (view result = Val (Num (I32 0l)))

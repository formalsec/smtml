open Encoding

let () =
  let module I32 = Expr.Bitv.I32 in
  assert (I32.sym "x" == I32.sym "x")

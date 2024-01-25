open Encoding
open Expr

let () =
  let x = Bitv.I32.sym "x" in
  let y = Bitv.I32.sym "y" in
  let e1 = Bitv.I32.(x > y) in
  let e2 = Bitv.I32.(y < x) in
  assert (e1 == e2)

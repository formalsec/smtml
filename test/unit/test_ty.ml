open Smtml

let () =
  let x = Expr.symbol (Symbol.make (Ty_bitv 32) "x") in
  let x = Expr.extract x ~high:2 ~low:0 in
  assert (Ty.equal (Expr.ty x) (Ty_bitv 16));
  let x = Expr.cvtop (Ty_bitv 32) (Sign_extend 16) x in
  assert (Ty.equal (Expr.ty x) (Ty_bitv 32))

open Encoding
module Solver = Solver.Z3_batch

let solver = Solver.create ()

let () =
  assert (
    let x = Expr.mk_symbol Symbol.("x" @: Ty_real) in
    let y = Expr.mk_symbol Symbol.("y" @: Ty_real) in
    let c0 = Expr.relop Ty_bool Eq x y in
    let c1 =
      Expr.relop Ty_bool Eq
        (Expr.cvtop Ty_real ToString x)
        (Expr.cvtop Ty_real ToString y)
    in
    Solver.check solver [ c0; c1 ] )

open Smtml

module Make (M : Mappings_intf.S) = struct
  module Solver = Solver.Batch (M)

  let () =
    let solver = Solver.create () in
    assert (
      let x = Expr.mk_symbol Symbol.("x" @: Ty_real) in
      let y = Expr.mk_symbol Symbol.("y" @: Ty_real) in
      let c0 = Expr.relop Ty_bool Eq x y in
      let c1 =
        Expr.relop Ty_bool Eq
          (Expr.cvtop Ty_real ToString x)
          (Expr.cvtop Ty_real ToString y)
      in
      `Sat = Solver.check solver [ c0; c1 ] )
end

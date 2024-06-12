open Smtml
open Test_harness

module Make (M : Mappings_intf.S) = struct
  module I8 = Expr.Bitv.I8
  module Solver = Solver.Incremental (M)

  let () =
    let solver = Solver.create ~logic:QF_BVFP () in
    let x = Expr.symbol @@ Symbol.make (Ty_bitv 8) "x" in
    Solver.add solver I8.[ x > v 0; x < v 2 ];
    assert_sat (Solver.check solver []);
    let val_x = Solver.get_value solver x in
    assert (Expr.equal val_x (I8.v 1))
end

open Smtml
open Test_harness

module Make (M : Mappings_intf.S) = struct
  module I8 = Expr.Bitv.I8
  module Solver = Solver.Incremental (M)

  let () =
    let solver = Solver.create ~logic:QF_BVFP () in
    let x = I8.sym "x" in
    assert_sat (Solver.check solver I8.[ x > v 0 ])
end

open Smtml
open Test_harness

module Make (M : Mappings_intf.S) = struct
  module Solver = Solver.Incremental (M)
  module F32 = Expr.Fpa.F32
  module F64 = Expr.Fpa.F64

  (* Regression for get value *)
  let () =
    let solver = Solver.create ~logic:QF_BVFP () in
    let x = F32.sym "x" in
    let const = F32.v 50.0 in
    Solver.add solver F32.[ x = const ];
    assert_sat (Solver.check solver []);
    assert (Expr.equal (Solver.get_value solver x) const)

  let () =
    let solver = Solver.create ~logic:QF_BVFP () in
    let x = F64.sym "x" in
    let const = F64.v 50.0 in
    Solver.add solver F64.[ x = const ];
    assert_sat (Solver.check solver []);
    assert (Expr.equal (Solver.get_value solver x) const)

  (* Sqrt *)
  let () =
    let solver = Solver.create ~logic:QF_BVFP () in
    let x = F32.sym "x" in
    Solver.add solver F32.[ x = F32.v 4.0 ];
    Solver.add solver F32.[ Expr.unop (Ty_fp 32) Sqrt x = F32.v 2.0 ];
    assert_sat (Solver.check solver []);
    assert (Expr.equal (Solver.get_value solver x) (F32.v 4.0))
end

open Smtml
open Test_harness

module Make (M : Mappings_intf.S) = struct
  module I8 = Expr.Bitv.I8
  module Solver = Solver.Incremental (M)

  let () =
    let solver = Solver.create ~logic:QF_BVFP () in
    let symbol_x = Symbol.("x" @: Ty_bitv 8) in
    let x = Expr.mk_symbol symbol_x in
    Solver.add solver I8.[ x > v 0; x < v 2 ];
    assert_sat (Solver.check solver []);
    let model = Solver.model solver in
    let val_x = Option.bind model (fun m -> Model.evaluate m symbol_x) in
    assert (Stdlib.(Some (Value.Num (I8 1)) = val_x))
end

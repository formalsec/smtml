open Smtml
open Test_harness

module Make (M : Mappings_intf.S) = struct
  open Test_harness.Infix
  module I8 = Expr.Bitv.I8
  module I32 = Expr.Bitv.I32
  module Solver = Solver.Incremental (M)

  let params = Params.default ()

  let () =
    let solver = Solver.create ~params ~logic:QF_BVFP () in
    let x = Expr.symbol @@ Symbol.make (Ty_bitv 8) "h" in
    Solver.add solver I8.[ x > v 0; x < v 2 ];
    assert_sat (Solver.check solver []);
    let val_x = Solver.get_value solver x in
    assert (Expr.equal val_x (I8.v 1))

  let () =
    let solver = Solver.create ~params ~logic:QF_BVFP () in
    let x = Expr.symbol @@ Symbol.make (Ty_bitv 32) "x" in
    let y = Expr.symbol @@ Symbol.make (Ty_bitv 32) "y" in
    let z = Expr.symbol @@ Symbol.make (Ty_bitv 32) "z" in
    let w = Expr.symbol @@ Symbol.make (Ty_bitv 32) "w" in
    Solver.add solver I32.[ x > v 0l && w < v 5l ];
    Solver.add solver I32.[ x < y && y < z && z < w ];
    assert_sat (Solver.check solver []);
    match Solver.model solver with
    | None -> assert false
    | Some m -> Model.pp Format.std_formatter ~no_values:false m
end

open Encoding
open Expr
module Solver = Solver.Z3_batch
module I8 = Bitv.I8

let solver = Solver.create ~logic:QF_BVFP ()

let () =
  let x = I8.sym "x" in
  assert (`Sat = Solver.check solver [ I8.(x > v 0) ]);
  assert (
    let v = Solver.get_value solver x in
    view v = Val (Num (I8 1)) )

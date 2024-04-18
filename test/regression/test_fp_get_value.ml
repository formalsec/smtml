open Smtml
open Expr
module Z3 = Solver.Z3_batch
module F32 = Fpa.F32
module F64 = Fpa.F64

let () =
  let solver = Z3.create ~logic:QF_BVFP () in
  let x = F32.sym "x" in
  let const = F32.v 50.0 in
  assert (`Sat = Z3.check solver F32.[ x = const ]);
  assert (Z3.get_value solver x = const);
  let x = F64.sym "x" in
  let const = F64.v 50.0 in
  assert (`Sat = Z3.check solver F64.[ x = const ]);
  assert (Z3.get_value solver x = const)

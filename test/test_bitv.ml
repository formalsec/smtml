open Encoding
open Expr
module Z3 = Solver.Z3_batch

let () =
  let solver = Z3.create ~logic:QF_BVFP () in
  let zero = Val (Num (I8 0)) @: Ty_bitv S8 in
  let x = mk_symbol Symbol.("x" @: Ty_bitv S8) in
  assert (Z3.check solver [ Relop (Gt, x, zero) @: Ty_bitv S8 ]);
  let v = Z3.get_value solver x in
  assert (v.e = Val (Num (I8 1)))

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

let () =
  let solver = Z3.create ~logic:QF_BVFP () in
  let x = Expr.mk_symbol Symbol.("x" @: Ty_fp S32) in
  let not_nan = Unop (Not, Unop (Is_nan, x) @: Ty_fp S32) @: Ty_bool in
  let is_nan = Unop (Is_nan, x) @: Ty_fp S32 in
  assert (not @@ Z3.check solver [ not_nan; is_nan ]);
  let nan = Val (Num (F32 (Int32.bits_of_float nan))) @: Ty_fp S32 in
  let ne_nan = Relop (Ne, x, nan) @: Ty_fp S32 in
  assert (Z3.check solver [ ne_nan; is_nan ])

let () =
  let solver = Z3.create ~logic:QF_BVFP () in
  let x = mk_symbol Symbol.("x" @: Ty_fp S32) in
  let const = Val (Num (F32 (Int32.bits_of_float 50.0))) @: Ty_fp S32 in
  assert (Z3.check solver [ Relop (Eq, x, const) @: Ty_fp S32 ]);
  assert (Z3.get_value solver x = const)

let () =
  let solver = Z3.create ~logic:QF_BVFP () in
  let x = mk_symbol Symbol.("x" @: Ty_fp S64) in
  let const = Val (Num (F64 (Int64.bits_of_float 50.0))) @: Ty_fp S64 in
  assert (Z3.check solver [ Relop (Eq, x, const) @: Ty_fp S64 ]);
  assert (Z3.get_value solver x = const)

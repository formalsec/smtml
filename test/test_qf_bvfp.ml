open Encoding
open Expr
module Z3 = Solver.Z3_batch
module F32 = Fpa.F32
module F64 = Fpa.F64

let () =
  let module I8 = Bitv.I8 in
  let solver = Z3.create ~logic:QF_BVFP () in
  let x = I8.sym "x" in
  assert (Z3.check solver [ I8.(x > v 0) ]);
  let v = Z3.get_value solver x in
  assert (v.e = Val (Num (I8 1)))

let () =
  let solver = Z3.create ~logic:QF_BVFP () in
  let x = Expr.mk_symbol Symbol.("x" @: Ty_fp S32) in
  let not_nan = Unop (Not, Unop (Is_nan, x) @: Ty_fp S32) @: Ty_bool in
  let is_nan = Unop (Is_nan, x) @: Ty_fp S32 in
  assert (not @@ Z3.check solver [ not_nan; is_nan ]);
  let ne_nan = Relop (Ne, x, F32.v nan) @: Ty_fp S32 in
  assert (Z3.check solver [ ne_nan; is_nan ])

let () =
  let solver = Z3.create ~logic:QF_BVFP () in
  let x = F32.sym "x" in
  let const = F32.v 50.0 in
  assert (Z3.check solver F32.[ x = const ]);
  assert (Z3.get_value solver x = const);
  let x = F64.sym "x" in
  let const = F64.v 50.0 in
  assert (Z3.check solver F64.[ x = const ]);
  assert (Z3.get_value solver x = const)

let () =
  let solver = Z3.create ~logic:QF_BVFP () in
  let x = F32.sym "x" in
  let zero = F32.v 0.0 in
  let one = F32.v 1.0 in
  let half = F32.v 0.504 in
  Z3.add solver F32.[ x = half ];
  assert (Z3.check solver F32.[ one = Unop (Ceil, x) @: Ty_fp S32 ]);
  assert (Z3.check solver F32.[ zero = Unop (Floor, x) @: Ty_fp S32 ])

let () =
  let solver = Z3.create ~logic:QF_BVFP () in
  Z3.add solver F64.[ sym "x" = v 314159265359.000 ];
  Z3.add solver F64.[ Unop (Is_nan, sym "y") @: Ty_fp S64 ];
  Z3.add solver F64.[ sym "z" = Binop (Div, v 1.0, v 0.0) @: Ty_fp S64 ];
  let big_float =
    F64.v
      (-338460706455329128135018695364373065576360054171204343201037173805357995568625611429596369334345869072216864991017687935090688.000000
      )
  in
  Z3.add solver F64.[ sym "w" = big_float ];
  Z3.add solver F64.[ sym "v" = v 1.0 ];
  assert (Z3.check solver []);
  let model = Z3.model solver in
  Option.iter (Model.pp Format.std_formatter) model

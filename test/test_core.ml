open Encoding
open Ty
open Expr
module Solver = Solver.Batch (Z3_mappings)

let ( => ) b1 b2 = Bool.((not b1) || b2)

let () =
  let symbol_x = Symbol.("x" @: Ty_bool) in
  let x = mk_symbol symbol_x in
  let y = mk_symbol Symbol.("y" @: Ty_bool) in
  let z = mk_symbol Symbol.("z" @: Ty_bool) in
  let solver = Solver.create () in
  assert (Solver.check solver Bool.[ x != v false ]);
  assert ({ e = Val True; ty = Ty_bool } = Solver.get_value solver x);
  Solver.add solver Bool.[ x => y && y => z ];
  assert (Solver.check solver [ x => z ]);
  let w = mk_symbol Symbol.("w" @: Ty_bool) in
  let xor_xz = Binop (Xor, x, z) @: Ty_bool in
  Solver.add solver Bool.[ w = Triop (Ite, xor_xz, v true, v false) @: Ty_bool ];
  assert (Solver.check solver Bool.[ x && z => not w ]);
  let model = Solver.model solver in
  Option.iter (Model.pp Format.std_formatter) model

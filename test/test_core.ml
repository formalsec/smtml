open Encoding
open Ty
open Expr
module Solver = Solver.Batch (Z3_mappings)

let symbol_x = Symbol.("x" @: Ty_bool)
let x = mk_symbol symbol_x

let () =
  let solver = Solver.create () in
  assert (Solver.check solver Bool.[ x = v true ]);
  assert ({ e = Val True; ty = Ty_bool } = Solver.get_value solver x);
  assert (Solver.check solver Bool.[ not (x = v true) ]);
  let model = Solver.model solver in
  let val_x = Option.bind model (fun m -> Model.evaluate m symbol_x) in
  assert (Some Value.False = val_x)

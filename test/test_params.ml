open Encoding
module Batch = Solver.Batch (Z3_mappings)

let%test_unit _ =
  let params = Params.(default () & (Ematching, false)) in
  let _ = Batch.create ~params () in
  ()

open Encoding
module Batch = Solver.Z3_batch

let () =
  let params = Params.(default () & (Ematching, false)) in
  ignore @@ Batch.create ~params ()

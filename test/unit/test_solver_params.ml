open Smtml
module Batch = Solver.Z3_batch

let () =
  assert (Params.default_value Timeout = Int32.(to_int max_int));
  assert (Params.default_value Model = true);
  assert (Params.default_value Unsat_core = false);
  assert (Params.default_value Ematching = true)

let () =
  let open Params in
  let params =
    default () $ (Timeout, 900) $ (Unsat_core, true) $ (Model, false)
    $ (Ematching, false)
  in
  ignore @@ Batch.create ~params ()

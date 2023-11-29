open Encoding
module Batch = Batch.Make (Z3_mappings)

let%test_unit _ =
  let params = Params.(default () & (Ematching, false)) in
  let _ = Batch.create ~params () in
  ()

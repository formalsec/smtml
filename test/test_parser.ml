open Core
open Encoding
module Batch = Batch.Make (Z3_mappings)
module Interpret = Interpret.Make (Batch)

let parse_and_run script = Run.parse_string script |> Interpret.start

let%expect_test _ =
  let script =
    {|
    (declare-fun x real)
    (declare-fun y real)
    (assert (real.eq y (real.mul x x)))
    (assert (real.eq y 2.0))
    (check-sat)
    |}
  in
  parse_and_run script 2;
  [%expect {| sat |}]

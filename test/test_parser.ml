open Encoding
module Batch = Solver.Batch (Z3_mappings)
module Interpret = Interpret.Make (Batch)

let () =
  let script =
    {|
    (declare-fun x real)
    (declare-fun y real)
    (assert (real.eq y (real.mul x x)))
    (assert (real.eq y 2.0))
    (check-sat)
    |}
  in
  Parse.from_string script |> Interpret.start |> ignore

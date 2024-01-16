open Encoding
module Batch = Solver.Batch (Z3_mappings)
module Interpret = Interpret.Make (Batch)

let () =
  (* FIXME: when y = 2.0 this fails in Z3! *)
  let script =
    {|
    (declare-fun x real)
    (declare-fun y real)
    (assert (real.eq y (real.mul x x)))
    (assert (real.eq y 4.0))
    (check-sat)
    (get-model)
    |}
  in
  Parse.from_string script |> Interpret.start |> ignore

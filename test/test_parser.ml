open Encoding
module Batch = Solver.Batch (Z3_mappings)
module Interpret = Interpret.Make (Batch)

let () =
  (* FIXME: when y = 2.0 this fails in Z3! *)
  let script =
    {|
    (let-const x real)
    (let-const y real)
    (assert (= y (real.mul x x)))
    (assert (= y 4.0))
    (check-sat)
    (get-model)
    |}
  in
  Parse.from_string script |> Interpret.start |> ignore

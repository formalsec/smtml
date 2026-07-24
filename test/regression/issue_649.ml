let expr =
  Smtml.Typed.Int.mod_ (Smtml.Typed.Int.v 4)
    (Smtml.Typed.symbol Smtml.Typed.Types.int "x")

let expr = Smtml.Typed.Int.eq expr (Smtml.Typed.Int.v 0)

module Z3 = Smtml.Solver.Batch (Smtml.Z3_mappings)

let solver = Z3.create ()

let () =
  match Z3.check solver [ Smtml.Typed.Unsafe.unwrap expr ] with
  | `Sat -> Format.printf "SAT@\n"
  | `Unsat -> Format.printf "UNSAT@\n"
  | `Unknown -> Format.printf "UNKNOWN@\n"

let y = Smtml.Typed.Float32.symbol (Smtml.Symbol.make (Smtml.Ty.Ty_fp 32) "y")

let expr =
  Smtml.Typed.Float32.neg
    (Smtml.Typed.Float32.add (Smtml.Typed.Float32.of_float 42.) y)

let expr = Smtml.Typed.Bitv32.reinterpret_f32 expr

let expr = Smtml.Typed.Bitv32.lt expr Smtml.Typed.Bitv32.one

module CVC5 = Smtml.Solver.Batch (Smtml.Cvc5_mappings)

let solver = CVC5.create ()

let () =
  assert (
    match CVC5.check solver [ Smtml.Typed.Unsafe.unwrap expr ] with
    | `Sat -> true
    | `Unsat | `Unknown -> false )

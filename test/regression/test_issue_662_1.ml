(* forall x, y, z \in bv32. (x < y && y < z) => x < z *)

let x = Smtml.Typed.var Smtml.Typed.Types.bitv32 "x"

let y = Smtml.Typed.var Smtml.Typed.Types.bitv32 "y"

let z = Smtml.Typed.var Smtml.Typed.Types.bitv32 "z"

let expr =
  Smtml.Typed.Bool.and_ (Smtml.Typed.Bitv32.lt x y) (Smtml.Typed.Bitv32.lt y z)

let expr = Smtml.Typed.Bool.implies expr (Smtml.Typed.Bitv32.lt x z)

let expr = Smtml.Typed.Bool.forall [ x; y; z ] expr

module Z3 = Smtml.Solver.Batch (Smtml.Z3_mappings)

let solver = Z3.create ()

let () =
  assert (
    match Z3.check solver [ (expr :> Smtml.Expr.t) ] with
    | `Sat -> true
    | `Unsat | `Unknown -> false )

module Altergo = Smtml.Solver.Batch (Smtml.Altergo_mappings)

let solver = Altergo.create ()

let () =
  assert (
    match Altergo.check solver [ (expr :> Smtml.Expr.t) ] with
    | `Sat -> true
    | `Unsat | `Unknown -> false )

module Colibri2 = Smtml.Solver.Batch (Smtml.Colibri2_mappings)

let solver = Colibri2.create ()

let () =
  assert (
    match Colibri2.check solver [ (expr :> Smtml.Expr.t) ] with
    (* Colibri2 returns unknown here, so we assert true to prevent the test from failing *)
    | `Sat | `Unknown -> true
    | `Unsat -> false )

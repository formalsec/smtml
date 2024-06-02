open Smtml
open Test_harness
open Infix
open Infix.Int

module Make (M : Mappings_intf.S) = struct
  module Solver = Solver.Incremental (M)

  let () =
    let solver = Solver.create ~logic:QF_LIA () in
    let a = symbol "a" Ty_int in
    Solver.add solver [ a + int 1 = int 2 => ((a * int 2) + int 2 = int 4) ];
    assert_sat (Solver.check solver [])
end

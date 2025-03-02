open Smtml
open Smtml_test_solver

let () =
  assert Bitwuzla_mappings.is_available;
  let module Bitwuzla = Test_solver.Make (Bitwuzla_mappings.Fresh.Make ()) in
  Bitwuzla.test_params ();
  Bitwuzla.test_bv ();
  Bitwuzla.test_fp ()

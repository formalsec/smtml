open Smtml
open Smtml_test_solver

let () =
  assert Colibri2_mappings.is_available;
  let module C2 = Test_solver.Make (Colibri2_mappings) in
  C2.test_params ();
  C2.test_cached_solver ();
  C2.test ();
  C2.test_bv ();
  C2.test_fp ();
  C2.test_lia ()

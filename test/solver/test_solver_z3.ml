open Smtml
open Smtml_test_solver

let () =
  assert Z3_mappings.is_available;
  let module Z3_mappings = Z3_mappings.Fresh.Make () in
  let module Z3_opt = Test_optimizer.Make (Z3_mappings) in
  Z3_opt.test ();
  let module Z3 = Test_solver.Make (Z3_mappings) in
  Z3.test_params ();
  Z3.test_cached_solver ();
  Z3.test ();
  Z3.test_lia ();
  Z3.test_lra ();
  Z3.test_bv ();
  Z3.test_fp ()

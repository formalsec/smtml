open Smtml
open Smtml_test_solver

let () =
  Printexc.record_backtrace true;
  assert Altergo_mappings.is_available;
  let module Alt_ergo = Test_solver.Make (Altergo_mappings.Fresh.Make ()) in
  Alt_ergo.test_params ();
  Alt_ergo.test_cached ();
  Alt_ergo.test ();
  Alt_ergo.test_bv ();
  Alt_ergo.test_lia ()

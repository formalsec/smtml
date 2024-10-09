open Smtml
open Smtml_tests

let () =
  Printexc.record_backtrace true;
  assert Altergo_mappings.is_available;
  let module Test_solver_params =
    Test_solver_params.Make (Altergo_mappings.Fresh.Make ()) in
  let module Test_solver = Test_solver.Make (Altergo_mappings.Fresh.Make ()) in
  (* let module Test_optimizer =
     Test_optimizer.Make (Altergo_mappings.Fresh.Make ()) in *)
  let module Test_bv = Test_bv.Make (Altergo_mappings.Fresh.Make ()) in
  (* let module Test_fp = Test_fp.Make (Altergo_mappings.Fresh.Make ()) in *)
  let module Test_lia = Test_lia.Make (Altergo_mappings.Fresh.Make ()) in
  (* let module Test_lra = Test_lra.Make (Altergo_mappings.Fresh.Make ()) in *)
  ()

open Smtml
open Smtml_tests

let () =
  assert Colibri2_mappings.is_available;
  let module Test_solver_params = Test_solver_params.Make (Colibri2_mappings) in
  let module Test_solver = Test_solver.Make (Colibri2_mappings) in
  let module Test_bv = Test_bv.Make (Colibri2_mappings) in
  let module Test_fp = Test_fp.Make (Colibri2_mappings) in
  let module Test_lia = Test_lia.Make (Colibri2_mappings) in
  ()

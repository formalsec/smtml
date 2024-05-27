open Smtml_tests
module Test_solver_params = Test_solver_params.Make (Colibri2_mappings)
module Test_solver = Test_solver.Make (Colibri2_mappings)
module Test_bv = Test_bv.Make (Colibri2_mappings)
module Test_lia = Test_lia.Make (Colibri2_mappings)

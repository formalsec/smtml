open Encoding
open Encoding_tests
module Test_solver_params = Test_solver_params.Make (Z3_mappings)
module Test_lia = Test_lia.Make (Z3_mappings)
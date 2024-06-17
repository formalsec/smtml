open Smtml
open Smtml_tests

let () = assert Z3_mappings.is_available

module Test_solver_params = Test_solver_params.Make (Z3_mappings.Fresh.Make ())
module Test_solver = Test_solver.Make (Z3_mappings.Fresh.Make ())
module Test_optimizer = Test_optimizer.Make (Z3_mappings.Fresh.Make ())
module Test_bv = Test_bv.Make (Z3_mappings.Fresh.Make ())
module Test_fp = Test_fp.Make (Z3_mappings.Fresh.Make ())
module Test_lia = Test_lia.Make (Z3_mappings.Fresh.Make ())
module Test_lra = Test_lra.Make (Z3_mappings.Fresh.Make ())

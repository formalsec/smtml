open OUnit2
open Smtml
open Smtml_test_solver

let is_available =
  "is_available" >:: fun _ -> assert_equal Z3_mappings.is_available true

let test_suite =
  assert Z3_mappings.is_available;
  let module Z3_mappings = Z3_mappings.Fresh.Make () in
  let module Z3_opt = Test_optimizer.Make (Z3_mappings) in
  let module Z3 = Test_solver.Make (Z3_mappings) in
  "Z3"
  >::: [ is_available
       ; Z3_opt.test
       ; Z3.test_params
       ; Z3.test_cached
       ; Z3.test_lia
       ; Z3.test_lra
       ; Z3.test_bv
       ; Z3.test_fp
       ]

let () = run_test_tt_main test_suite

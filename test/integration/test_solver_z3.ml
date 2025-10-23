open OUnit2
open Smtml
open Smtml_test_solver

let is_available =
  "is_available" >:: fun _ -> assert_equal Z3_mappings.is_available true

let test_suite =
  let module Z3_opt = Test_optimizer.Make (Z3_mappings) in
  let module Z3_solv = Test_solver.Make (Z3_mappings) in
  let module Z3_bindings = Test_bindings.Make (Z3_mappings.Mappings) in
  "Z3"
  >::: [ is_available
       ; Z3_opt.test
       ; Z3_solv.test_params
       ; Z3_solv.test_cached
       ; Z3_solv.test_lia
       ; Z3_solv.test_lra
       ; Z3_solv.test_bv
       ; Z3_solv.test_fp
       ; Z3_solv.test_uninterpreted
       ; Z3_bindings.test_adt
       ]

let () = run_test_tt_main test_suite

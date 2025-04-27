open OUnit2
open Smtml
open Smtml_test_solver

let is_available =
  "is_available" >:: fun _ -> assert_equal Colibri2_mappings.is_available true

let test_suite =
  let module C2 = Test_solver.Make (Colibri2_mappings) in
  "Colibri2"
  >::: [ is_available
       ; C2.test_params
       ; C2.test_cached
       ; C2.test_bv
       ; C2.test_fp
       ; C2.test_lia
       ]

let () =
  run_test_tt_main test_suite

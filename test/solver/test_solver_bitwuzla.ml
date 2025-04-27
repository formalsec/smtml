open OUnit2
open Smtml
open Smtml_test_solver

let is_available =
  "is_available" >:: fun _ -> assert_equal Bitwuzla_mappings.is_available true

let test_suite =
  assert Bitwuzla_mappings.is_available;
  let module Bitwuzla = Test_solver.Make (Bitwuzla_mappings.Fresh.Make ()) in
  "Bitwuzla"
  >::: [ is_available
       ; Bitwuzla.test_params
       ; Bitwuzla.test_bv
       ; Bitwuzla.test_fp
       ]

let () = run_test_tt_main test_suite

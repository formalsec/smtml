open OUnit2
open Smtml
open Smtml_test_solver

let is_available =
  "is_available" >:: fun _ -> assert_equal Cvc5_mappings.is_available true

let test_suite =
  let module Cvc5_solv = Test_solver.Make (Cvc5_mappings) in
  "cvc5"
  >::: [ is_available
       ; Cvc5_solv.test_params
       ; Cvc5_solv.test_lia
       ; Cvc5_solv.test_bv
       ; Cvc5_solv.test_regexp
       ; Cvc5_solv.test_extract
       ; Cvc5_solv.test_typed_api_consistency
       ]

let () = run_test_tt_main test_suite

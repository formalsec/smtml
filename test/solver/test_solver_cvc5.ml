open OUnit2
open Smtml
open Smtml_test_solver

let is_available =
  "is_available" >:: fun _ -> assert_equal Cvc5_mappings.is_available true

let () =
  let module Cvc5 = Test_solver.Make (Cvc5_mappings) in
  "cvc5"
  >::: [ is_available
       ; Cvc5.test_params
       ; Cvc5.test_cached
       ; Cvc5.test_lia
       ; Cvc5.test_bv
       ]

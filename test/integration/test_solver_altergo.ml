open OUnit2
open Smtml
open Smtml_test_solver

let is_available =
  "is_available" >:: fun _ -> assert_equal Altergo_mappings.is_available true

let test_suite =
  let module Alt_ergo = Test_solver.Make (Altergo_mappings) in
  "Alt-ergo"
  >::: [ is_available
       ; Alt_ergo.test_params
         (* ; Alt_ergo.test_cached *)
         (* ; Alt_ergo.test_lia *)
         (* ; Alt_ergo.test_bv *)
       ]

let () =
  Printexc.record_backtrace true;
  run_test_tt_main test_suite

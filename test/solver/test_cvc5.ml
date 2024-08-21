open Smtml
open Smtml_tests

let () =
  assert Cvc5_mappings.is_available;
  let module Test_solver_params =
    Test_solver_params.Make (Cvc5_mappings.Fresh.Make ()) in
  let module Test_bv = Test_bv.Make (Cvc5_mappings) in
  (* let module Test_fp = Test_fp.Make (Cvc5_mappings) in *)
  ()

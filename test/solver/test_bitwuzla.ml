open Smtml
open Smtml_tests

let () =
  assert Bitwuzla_mappings.is_available;
  let module Test_solver_params =
    Test_solver_params.Make (Bitwuzla_mappings.Fresh.Make ()) in
  let module Test_bv = Test_bv.Make (Bitwuzla_mappings) in
  let module Test_fp = Test_fp.Make (Bitwuzla_mappings) in
  ()

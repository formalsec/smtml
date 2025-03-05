open Smtml
open Smtml_test_solver

let () =
  assert Cvc5_mappings.is_available;
  let module Cvc5 = Test_solver.Make (Cvc5_mappings.Fresh.Make ()) in
  Cvc5.test_params ();
  Cvc5.test_cached ();
  Cvc5.test ();
  Cvc5.test_bv ()

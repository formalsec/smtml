open OUnit2
open Smtml

(* TODO: Move this some place else? *)
module Make (M : Mappings_intf.S) = struct
  open Smtml_test.Test_harness
  module Optimizer = Optimizer.Make (M)

  let test =
    "test_optimizer" >:: fun _ ->
    let open Infix in
    let opt = Optimizer.create () in
    let x = symbol "x" Ty_int in
    Optimizer.add opt Int.[ x >= int 0; x < int 5 ];
    Optimizer.protect opt (fun () ->
      assert (Stdlib.(Some (Value.Int 0) = Optimizer.minimize opt x)) );
    assert (Stdlib.(Some (Value.Int 4) = Optimizer.maximize opt x))
end

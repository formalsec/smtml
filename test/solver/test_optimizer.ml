open Smtml

module Make (M : Mappings_intf.S) = struct
  open Test_harness
  module Optimizer = Optimizer.Make (M)

  let () =
    let open Infix in
    let opt = Optimizer.create () in
    let x = symbol "x" Ty_int in
    Optimizer.add opt Int.[ x >= int 0; x < int 5 ];
    Optimizer.protect opt (fun () ->
        assert (Stdlib.(Some (Value.Int 0) = Optimizer.minimize opt x)) );
    assert (Stdlib.(Some (Value.Int 4) = Optimizer.maximize opt x))
end

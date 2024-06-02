open Smtml

module Make (M : Mappings_intf.S) = struct
  open Params
  module Solver = Solver.Incremental (M)

  let () =
    assert (default_value Timeout = Int32.(to_int max_int));
    assert (default_value Model = true);
    assert (default_value Unsat_core = false);
    assert (default_value Ematching = true)

  let () =
    let params =
      default () $ (Timeout, 900) $ (Unsat_core, true) $ (Model, false)
      $ (Ematching, false)
    in
    ignore (Solver.create ~params ())
end

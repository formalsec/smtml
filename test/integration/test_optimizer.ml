(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2026 formalsec *)
(* Written by the Smtml programmers *)

open Smtml

module Make (M : Mappings_intf.S) = struct
  open Smtml_test.Test_harness
  module Optimizer = Optimizer.Make (M)

  let test_optimizer () =
    let open Infix in
    let opt = Optimizer.create () in
    let x = symbol "x" Ty_int in
    Optimizer.add opt Int.[ int 0 <= x; x < int 5 ];
    Optimizer.protect opt (fun () ->
      Alcotest.(
        check
          (option (testable Value.pp Value.equal))
          "minimize" (Some (Value.Int Z.zero)) (Optimizer.minimize opt x) ) );
    Alcotest.(
      check
        (option (testable Value.pp Value.equal))
        "maximize"
        (Some (Value.Int (Z.of_int 4)))
        (Optimizer.maximize opt x) )

  let test =
    ( "test_optimizer"
    , [ Alcotest.test_case "test_optimizer" `Quick test_optimizer ] )
end

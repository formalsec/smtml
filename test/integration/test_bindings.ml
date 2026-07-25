(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2026 formalsec *)
(* Written by the Smtml programmers *)

[@@@ocaml.warning "-26"]

module Make (Mappings : Smtml.Mappings_intf.M) = struct
  open Mappings

  let test_simple_adt () =
    let ptr_adt =
      Adt.make "Ptr"
        [ Adt.Cons.make "mk-ptr"
            ~fields:[ ("loc", Some Types.int); ("ofs", Some Types.int) ]
        ]
    in
    let ty = Adt.ty ptr_adt in
    let loc, ofs =
      ( Adt.selector "loc" ptr_adt |> Option.get
      , Adt.selector "ofs" ptr_adt |> Option.get )
    in
    let mk_ptr = Adt.constructor "mk-ptr" ptr_adt |> Option.get in
    let p1 = const "p1" ty in
    let solver = Solver.make () in
    Solver.add solver
      [ eq p1 (Func.apply mk_ptr [ int (Z.of_int 10); int (Z.of_int 8) ])
      ; eq (Func.apply loc [ p1 ]) (int (Z.of_int 10))
      ; eq (Func.apply ofs [ p1 ]) (int (Z.of_int 8))
      ];
    Alcotest.(check bool)
      "test_simple_adt"
      ( match Solver.check solver ~assumptions:[] with
      | `Sat -> true
      | `Unknown | `Unsat -> false )
      true

  let test_adt =
    ("test_adt", [ Alcotest.test_case "test_simple_adt" `Quick test_simple_adt ])
end

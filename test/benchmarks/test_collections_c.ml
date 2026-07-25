(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2026 formalsec *)
(* Written by the Smtml programmers *)

module Solver = Smtml.Solver.Incremental (Smtml.Z3_mappings)
module Interpreter = Smtml.Interpret.Make (Solver)

let benchmarks =
  let dir = Fpath.(v "datasets" / "collections-c") in
  Bos.OS.Dir.fold_contents ~traverse:`Any ~elements:`Files
    (fun path acc -> if Fpath.has_ext ".smt2" path then path :: acc else acc)
    [] dir
  |> Result.map_error (fun (`Msg err) ->
    Fmt.epr "%s" err;
    `Msg err )
  |> Result.get_ok

let make_test path () =
  let script = Smtml.Compile.until_rewrite path |> Result.get_ok in
  let _ = Interpreter.start ~no_strict_status:true ~quiet:true script in
  ()

let () =
  let test_cases =
    List.map
      (fun path ->
        Alcotest.test_case (Fpath.to_string path) `Quick (make_test path) )
      benchmarks
  in
  Alcotest.run "collections-c" [ ("collections-c", test_cases) ]

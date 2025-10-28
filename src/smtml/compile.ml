(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2024 formalsec *)
(* Written by the Smtml programmers *)

let until_rewrite filename ~no_simpls =
  match Utils.perf_log_path with
  | None ->
    let script = Parse.from_file filename in
    Rewrite.rewrite script ~no_simpls
  | Some _ ->
    let test_name = Fpath.to_string filename in
    Utils.start_test_timing test_name;
    let script =
      Utils.run_and_time_call
        ~use:(fun t -> Utils.add_parse_time t)
        (fun () -> Parse.from_file filename)
    in
    let rewritten_script =
      Utils.run_and_time_call
        ~use:(fun t -> Utils.add_rewrite_time t)
        (fun () -> Rewrite.rewrite script ~no_simpls)
    in
    rewritten_script

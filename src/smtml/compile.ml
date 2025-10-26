(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2024 formalsec *)
(* Written by the Smtml programmers *)

let until_rewrite filename ~no_simpls =
  let script = Parse.from_file filename in
  Rewrite.rewrite script ~no_simpls

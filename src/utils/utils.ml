(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2024 formalsec *)
(* Written by the Smtml programmers *)

let run_and_time_call ~use f =
  let start = (Rusage.get Self).utime in
  let result = f () in
  let stop = (Rusage.get Self).utime in
  use (stop -. start);
  result

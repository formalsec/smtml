(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2024 formalsec *)
(* Written by the Smtml programmers *)

let run_and_time_call ~use f =
  let start = Unix.gettimeofday () in
  let result = f () in
  let stop = Unix.gettimeofday () in
  use (stop -. start);
  result

let query_log_path : Fpath.t option =
  let env_var = "QUERY_LOG_PATH" in
  match Bos.OS.Env.var env_var with Some p -> Some (Fpath.v p) | None -> None

let[@inline never] protect m f =
  Mutex.lock m;
  match f () with
  | x ->
    Mutex.unlock m;
    x
  | exception e ->
    Mutex.unlock m;
    let bt = Printexc.get_raw_backtrace () in
    Printexc.raise_with_backtrace e bt

(* If the environment variable [QUERY_LOG_PATH] is set, stores and writes
   all queries sent to the solver (with their timestamps) to the given file *)
let write =
  match query_log_path with
  | None -> fun _ _ _ -> ()
  | Some path ->
    let log_entries : (string * Expr.t list * int64) list ref = ref [] in
    let close () =
      let entries = List.rev !log_entries in
      let bytes = Marshal.to_string entries [] in
      match Bos.OS.File.write path bytes with
      | Ok () -> ()
      | Error (`Msg e) -> Fmt.failwith "Failed to write log: %s" e
    in
    at_exit close;
    Sys.set_signal Sys.sigterm (Sys.Signal_handle (fun _ -> close ()));
    (* write *)
    let mutex = Mutex.create () in
    fun solver_name assumptions time ->
      let entry = (solver_name, assumptions, time) in
      protect mutex (fun () -> log_entries := entry :: !log_entries)

(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2024 formalsec *)
(* Written by the Smtml programmers *)

let run_and_time_call ~use f =
  let start_counter = Mtime_clock.counter () in
  let result = f () in
  let span = Mtime_clock.count start_counter in
  use (Mtime.Span.to_float_ns span);
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
      if List.compare_length_with !log_entries 0 <> 0 then
        try
          let oc =
            (* open with wr/r/r rights, create if it does not exit and append to
            it if it exists. *)
            Out_channel.open_gen
              [ Open_creat; Open_binary; Open_append ]
              0o644 (Fpath.to_string path)
          in
          Marshal.to_channel oc !log_entries [];
          Out_channel.close oc
        with e ->
          Fmt.failwith "Failed to write log: %s@." (Printexc.to_string e)
    in
    at_exit close;
    Sys.set_signal Sys.sigterm (Sys.Signal_handle (fun _ -> close ()));
    (* write *)
    let mutex = Mutex.create () in
    fun solver_name assumptions time ->
      let entry = (solver_name, assumptions, time) in
      protect mutex (fun () -> log_entries := entry :: !log_entries)

type partial_timing =
  { mutable test_name : string
  ; mutable parse_s : float
  ; mutable rewrite_s : float
  }

type test_timing =
  { test_name : string
  ; parse_s : float
  ; rewrite_s : float
  ; check_sat_s : float
  }

let perf_log_path : Fpath.t option =
  let env_var = "PERF_LOG_PATH" in
  match Bos.OS.Env.var env_var with Some p -> Some (Fpath.v p) | None -> None

let pending_timings : (int, partial_timing) Hashtbl.t = Hashtbl.create 16

let completed_timings : test_timing list ref = ref []

let perf_mutex = Mutex.create ()

let close_perf_log () =
  match perf_log_path with
  | None -> ()
  | Some path ->
    protect perf_mutex (fun () ->
      let entries = List.rev !completed_timings in

      let buffer = Buffer.create 1024 in
      let ppf = Fmt.with_buffer buffer in

      Fmt.pf ppf "test_name,parse_time_s,rewrite_time_s,check_sat_time_s\n";

      List.iter
        (fun e ->
          Fmt.pf ppf "%s,%.9f,%.9f,%.9f\n" e.test_name e.parse_s e.rewrite_s
            e.check_sat_s )
        entries;

      Fmt.flush ppf ();

      let csv_content = Buffer.contents buffer in

      match Bos.OS.File.write path csv_content with
      | Ok () -> ()
      | Error (`Msg e) -> Fmt.epr "Failed to write perf log: %s\n" e )

let () =
  match perf_log_path with
  | None -> ()
  | Some _ ->
    at_exit close_perf_log;
    Sys.set_signal Sys.sigterm (Sys.Signal_handle (fun _ -> close_perf_log ()))

let start_test_timing (name : string) =
  match perf_log_path with
  | None -> ()
  | Some _ ->
    let tid = Thread.id (Thread.self ()) in
    protect perf_mutex (fun () ->
      Hashtbl.replace pending_timings tid
        { test_name = name; parse_s = 0.0; rewrite_s = 0.0 } )

let add_parse_time (time_s : float) =
  match perf_log_path with
  | None -> ()
  | Some _ ->
    let tid = Thread.id (Thread.self ()) in
    protect perf_mutex (fun () ->
      match Hashtbl.find_opt pending_timings tid with
      | None ->
        Fmt.epr
          "PerfLog Error: add_parse_time called before start_test_timing\n"
      | Some partial -> partial.parse_s <- time_s )

let add_rewrite_time (time_s : float) =
  match perf_log_path with
  | None -> ()
  | Some _ ->
    let tid = Thread.id (Thread.self ()) in
    protect perf_mutex (fun () ->
      match Hashtbl.find_opt pending_timings tid with
      | None ->
        Fmt.epr
          "PerfLog Error: add_rewrite_time called before start_test_timing\n"
      | Some partial -> partial.rewrite_s <- time_s )

let add_check_sat_and_commit (time_s : float) =
  match perf_log_path with
  | None -> ()
  | Some _ ->
    let tid = Thread.id (Thread.self ()) in
    protect perf_mutex (fun () ->
      match Hashtbl.find_opt pending_timings tid with
      | None ->
        Fmt.epr
          "PerfLog Error: add_check_sat_and_commit called before \
           start_test_timing\n"
      | Some partial ->
        Hashtbl.remove pending_timings tid;
        let complete_record =
          { test_name = partial.test_name
          ; parse_s = partial.parse_s
          ; rewrite_s = partial.rewrite_s
          ; check_sat_s = time_s
          }
        in
        completed_timings := complete_record :: !completed_timings )

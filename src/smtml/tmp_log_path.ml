let log_path : Fpath.t option =
  let env_var = "QUERY_LOG_PATH" in
  match Bos.OS.Env.var env_var with Some p -> Some (Fpath.v p) | None -> None

let log_entries : (Expr.t list * int64) list ref = ref []

let write =
  match log_path with
  | None -> fun _ _ -> ()
  | Some _ ->
    let mutex = Mutex.create () in
    fun assumptions time ->
      let entry = (assumptions, time) in
      Mutex.protect mutex (fun () -> log_entries := entry :: !log_entries)

let close =
  match log_path with
  | None -> fun () -> ()
  | Some path -> (
    fun () ->
      let entries = List.rev !log_entries in
      let bytes = Marshal.to_string entries [] in
      match Bos.OS.File.write path bytes with
      | Ok () -> ()
      | Error (`Msg e) -> Fmt.failwith "Failed to write log: %s" e )

let () =
  match log_path with
  | None -> ()
  | Some _ ->
    at_exit close;
    Sys.set_signal Sys.sigterm (Sys.Signal_handle (fun _ -> close ()))

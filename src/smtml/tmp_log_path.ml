
let log_path : Fpath.t option ref = ref None

let mutex = Mutex.create ()

let write assumptions user_time system_time =
  let data = (assumptions, user_time, system_time) in
  let s = Marshal.to_string data [] in

  let path =
    match !log_path with
    | None ->
      let env_var = "QUERY_LOG_PATH" in
      begin match Bos.OS.Env.var env_var with
        | Some path ->
          let path = (Fpath.v path) in
          log_path := Some path;
          path
        | None ->
          Fmt.failwith "Temporary log path not set and QUERY_LOG_PATH is not defined"
      end
    | Some oc -> oc
  in
  Mutex.lock mutex;
  match Bos.OS.File.write path s with _ -> ();
  Mutex.unlock mutex

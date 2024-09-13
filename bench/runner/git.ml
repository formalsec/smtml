(** Best effort to try and get commit hash of HEAD *)
let get_head ?(length = 6) () =
  let open Bos in
  let short = Format.asprintf "--short=%d" length in
  let cmd = Cmd.(v "git" % "rev-parse" % short % "HEAD") in
  let output = OS.Cmd.run_out ~err:OS.Cmd.err_run_out cmd in
  match OS.Cmd.out_string ~trim:true output with
  | Ok (stdout, (_, `Exited 0)) -> stdout
  | Error (`Msg err) ->
    Format.eprintf "ERROR: %s@." err;
    "unknown"
  | Ok (stdout, (_, (`Exited _ | `Signaled _))) ->
    Format.eprintf "%s@\nWARN: Unable to fetch git HEAD@." stdout;
    "unknown"

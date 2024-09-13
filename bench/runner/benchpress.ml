open Bos

let ( let* ) v f = Result.bind v f

let cfg = Fpath.v "./benchpress.sexp"

let task task = Cmd.(v "benchpress" % "run" % "-c" % p cfg % "--task" % task)

let list_files = Cmd.(v "benchpress" % "list-files")

let show nc file =
  Cmd.(v "benchpress" % "show" % "--detail" % file %% on nc (v "--no-color"))

let string_of_run_out run_out =
  let* output, (_, status) = OS.Cmd.out_string run_out in
  match (status, output) with
  | `Exited 0, output -> Ok output
  | _ -> Error (`Msg "Cmd exited with non-zero code")

let stdout_lines cmd =
  let run_out = OS.Cmd.run_out cmd in
  let* lines, (_, status) = OS.Cmd.out_lines run_out in
  match (status, lines) with
  | `Exited 0, lines -> Ok lines
  | _ -> Error (`Msg "Cmd exited with non-zero code")

let remove_ansi_escape_codes s =
  let re = Str.regexp "\x1b\\[[0-9;]*m" in
  Str.global_replace re "" s

let notify_done url results =
  let url = Webhook.url_of_string url in
  let head = Git.get_head () in
  let title = Fmt.str "Test results (commit hash=%s) :octopus:" head in
  let body = Fmt.str "```%s```" (remove_ansi_escape_codes results) in
  let body = Webhook.default_slack_mrkdwn title body in
  Lwt_main.run @@ Webhook.post_and_forget url body

let latest_results' nc =
  let* lines = stdout_lines list_files in
  match lines with
  | [] -> Error (`Msg "Benchpress wasn't run yet")
  | hd :: _ -> (
    match String.split_on_char ' ' hd with
    | [ file; _ ] -> Ok (OS.Cmd.run_out @@ show nc file)
    | _ -> assert false )

let latest_results nc =
  let* run_out = latest_results' nc in
  let _ = OS.Cmd.out_stdout run_out in
  Ok ()

let run hook t =
  let* () = OS.Cmd.run @@ task t in
  match hook with
  | None -> Ok ()
  | Some web_hook ->
    let* run_out = latest_results' true in
    let* output = string_of_run_out run_out in
    Ok (notify_done web_hook output)

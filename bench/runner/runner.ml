let cli =
  let open Cmdliner in
  let run =
    let task = Arg.(required & opt (some string) None & info [ "task" ]) in
    let hook = Arg.(value & opt (some string) None & info [ "web-hook" ]) in
    let info = Cmd.info "run" in
    Cmd.v info Term.(const Benchpress.run $ hook $ task)
  in
  let latest_results =
    let nc = Arg.(value & flag & info [ "nc" ]) in
    let info = Cmd.info "latest-results" in
    Cmd.v info Term.(const Benchpress.latest_results $ nc)
  in
  let info = Cmd.info "runner" in
  Cmd.group info [ run; latest_results ]

let () =
  match Cmdliner.Cmd.eval_value' cli with
  | `Exit code -> exit code
  | `Ok (Error (`Msg err)) -> failwith err
  | `Ok (Ok ()) -> exit 0

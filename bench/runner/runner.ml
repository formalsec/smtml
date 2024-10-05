let prover_conv = Cmdliner.Arg.conv (Tool.prover_of_string, Tool.pp_prover)

let fpath_dir =
  let dir_parser, _ = Cmdliner.Arg.dir in
  ( (fun str ->
      match dir_parser str with
      | `Ok dir -> `Ok (Fpath.v dir)
      | `Error _ as err -> err )
  , Fpath.pp )

let cli =
  let open Cmdliner in
  let hook = Arg.(value & opt (some string) None & info [ "web-hook" ]) in
  let dirs = Arg.(value & pos_all fpath_dir [] & info [] ~docv:"PATH") in
  let provers = Arg.(value & opt_all prover_conv [] & info [ "p"; "prover" ]) in
  let timeout = Arg.(value & opt (some int) None & info [ "timeout" ]) in
  let single_query =
    let info = Cmd.info "single-query" in
    Cmd.v info Term.(const Single_query.main $ hook $ provers $ timeout $ dirs)
  in
  let multi_query =
    let info = Cmd.info "multi-query" in
    Cmd.v info Term.(const Multi_query.main $ hook $ provers $ dirs)
  in
  let info = Cmd.info "runner" in
  Cmd.group info [ single_query; multi_query ]

let () =
  match Cmdliner.Cmd.eval_value' cli with
  | `Exit code -> exit code
  | `Ok (Error (`Msg err)) -> Fmt.failwith "%s" err
  | `Ok (Ok ()) -> exit 0

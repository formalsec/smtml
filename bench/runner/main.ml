open Cmdliner

let file =
  let fpath = Arg.conv (Fpath.of_string, Fpath.pp) in
  Arg.(required & pos 0 (some fpath) None & info [] ~docv:"FILE")

let fpath_dir =
  let dir_parser = Arg.(conv_parser dir) in
  Arg.conv
    ( (fun str ->
        match dir_parser str with
        | Ok dir -> Ok (Fpath.v dir)
        | Error _ as err -> err )
    , Fpath.pp )

let dir_arg ~default arg_name =
  Arg.(value & opt fpath_dir default & info [ arg_name ])

let dirs = Arg.(value & pos_all fpath_dir [] & info [] ~docv:"PATH")

let provers =
  let prover_conv = Arg.conv (Tool.prover_of_string, Tool.pp_prover) in
  let default = Tool.(Smtml { name = Z3; st = false }) in
  Arg.(value & opt_all prover_conv [ default ] & info [ "prover" ])

let timeout = Arg.(value & opt (some int) None & info [ "timeout" ])

let hook = Arg.(value & opt (some string) None & info [ "webhook" ])

let _from_file = Arg.(value & opt (some file) None & info [ "from-file" ])

let exec =
  let open Term.Syntax in
  let info = Cmd.info "exec" in
  let cmd =
    let+ hook
    and+ timeout
    and+ provers
    and+ dirs in
    Cmd_exec.main ~hook ~timeout ~provers ~dirs
  in
  Cmd.v info cmd

let setup =
  let open Term.Syntax in
  let info = Cmd.info "setup" in
  let cmd =
    let+ file
    and+ datasets_dir = dir_arg ~default:Fpath.(v "_datasets") "datasets-dir" in
    Cmd_setup.main ~datasets_dir ~file
  in
  Cmd.v info cmd

let cli =
  let info = Cmd.info "benchme" in
  Cmd.group info [ exec; setup ]

let () =
  match Cmdliner.Cmd.eval_value' cli with
  | `Exit code -> exit code
  | `Ok (Error (`Msg err)) -> Fmt.failwith "%s" err
  | `Ok (Ok ()) -> exit 0

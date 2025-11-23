open Smtml
open Cmdliner

let fpath =
  let open Cmdliner in
  let parser = Arg.(conv_parser file) in
  Arg.conv
    ( (fun file ->
        if String.equal "-" file then Ok (Fpath.v file)
        else
          match parser file with
          | Ok file -> Ok (Fpath.v file)
          | Error _ as err -> err )
    , Fpath.pp )

let filename =
  let doc = "Input file" in
  let docv = "FILE" in
  Arg.(required & pos 0 (some fpath) None & info [] ~doc ~docv)

let filenames =
  let doc = "Input files" in
  let docv = "FILES" in
  Arg.(value & pos_all fpath [] & info [] ~docv ~doc)

let solver_type =
  let doc =
    Fmt.str "Configure which SMT solver to use. Available options are: %a"
      (Fmt.list ~sep:Fmt.comma (fun fmt v ->
         Fmt.pf fmt "$(b,%a)" Solver_type.pp v ) )
      Solver_dispatcher.available
  in
  Arg.(value & opt Solver_type.conv Z3_solver & info [ "s"; "solver" ] ~doc)

let solver_mode =
  let doc = "SMT solver mode" in
  Arg.(value & opt Solver_mode.conv Batch & info [ "mode" ] ~doc)

let debug =
  let doc = "Print debugging messages" in
  Arg.(value & flag & info [ "debug" ] ~doc)

let dry =
  let doc = "Dry run on tests" in
  Arg.(value & flag & info [ "dry" ] ~doc)

let print_statistics =
  let doc = "Print statistics" in
  Arg.(value & flag & info [ "print-statistics" ] ~doc)

let no_strict_status =
  let doc = "Disable strict status checking" in
  Arg.(value & flag & info [ "no-strict-status" ] ~doc)

let from_file =
  let doc =
    "File containing a list of files to run. This argument discards any \
     positional arguments provided."
  in
  Arg.(
    value & opt (some fpath) None & info [ "F"; "from-file" ] ~doc ~docv:"VAL" )

let version = Cmd_version.version

let cmd_run =
  let info =
    let doc =
      "Runs one or more scripts using. Also supports directory inputs"
    in
    Cmd.info ~version "run" ~doc
  in
  let term =
    let open Term.Syntax in
    let+ debug
    and+ dry
    and+ print_statistics
    and+ no_strict_status
    and+ solver_type
    and+ solver_mode
    and+ from_file
    and+ filenames in
    Cmd_run.run ~debug ~dry ~print_statistics ~no_strict_status ~solver_type
      ~solver_mode ~from_file ~filenames
  in
  Cmd.v info term

let cmd_to_smt2 =
  let info =
    let doc = "Convert .smtml into .smt2" in
    Cmd.info ~version "to-smt2" ~doc
  in
  let term =
    let open Term.Syntax in
    let+ debug
    and+ solver_type
    and+ filename in
    Cmd_to_smt2.run ~debug ~solver_type ~filename
  in
  Cmd.v info term

let cmd_to_smtml =
  let info =
    let doc = "Print/Format the content of an .smtml file" in
    Cmd.info ~version "to-smtml" ~doc
  in
  let term =
    let open Term.Syntax in
    let+ filename in
    Cmd_to_smt2.run_to_smtml ~filename
  in
  Cmd.v info term

let cmd_version =
  let info =
    let doc = "Print smtml version and linked libraries" in
    Cmd.info ~version "version" ~doc
  in
  let term = Term.(const Cmd_version.run $ const ()) in
  Cmd.v info term

let commands =
  let info = Cmd.info ~version "smtml" in
  Cmd.group info [ cmd_run; cmd_to_smt2; cmd_to_smtml; cmd_version ]

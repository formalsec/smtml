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

let from_file =
  let doc =
    "File containing a list of files to run. This argument discards any \
     positional arguments provided."
  in
  Arg.(
    value & opt (some fpath) None & info [ "F"; "from-file" ] ~doc ~docv:"VAL" )

let info_run =
  let doc = "Runs one or more scripts using. Also supports directory inputs" in
  Cmd.info "run" ~doc

let cmd_run =
  let open Term.Syntax in
  let+ debug
  and+ dry
  and+ print_statistics
  and+ solver_type
  and+ solver_mode
  and+ from_file
  and+ filenames in
  Cmd_run.run ~debug ~dry ~print_statistics ~solver_type ~solver_mode ~from_file
    ~filenames

let info_to_smt2 =
  let doc = "Convert .smtml into .smt2" in
  Cmd.info "to-smt2" ~doc

let cmd_to_smt2 =
  let open Term.Syntax in
  let+ debug
  and+ solver_type
  and+ filename in
  Cmd_to_smt2.run ~debug ~solver_type ~filename

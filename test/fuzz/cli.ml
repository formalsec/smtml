(* cmdliner stuff *)
open Cmdliner

let randomness_file =
  let doc =
    "A file containing some bytes, consulted in constructing test cases.  When \
     `afl-fuzz` is calling the test binary, use `@@` to indicate that \
     `afl-fuzz` should put its test case here (e.g. `afl-fuzz -i input -o \
     output ./my_crowbar_test @@`).  Re-run a test by supplying the test file \
     here (e.g. `./my_crowbar_test output/crashes/id:000000`).  If no file is \
     specified, the test will use OCaml's Random module as a source of \
     randomness for a predefined number of rounds."
  in
  Cmdliner.Arg.(value & pos 0 (some file) None & info [] ~doc ~docv:"FILE")

let seed =
  let doc =
    "The seed (an int64) for the PRNG. Use as an alternative to FILE\n\
    \    when running in non-AFL (quickcheck) mode."
  in
  Cmdliner.Arg.(
    value & opt (some int64) None & info [ "s"; "seed" ] ~doc ~docv:"SEED" )

let repeat =
  let doc = "The number of times to repeat the test in quick-check." in
  Cmdliner.Arg.(value & opt int 1 & info [ "r"; "repeat" ] ~doc ~docv:"REPEAT")

let log_level =
  let env = Cmd.Env.info "VERBOSITY" in
  Logs_cli.level ~env ()

let setup_log =
  let open Term.Syntax in
  let+ log_level = log_level
  and+ style_renderer = Fmt_cli.style_renderer () in
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level log_level;
  let reporter = Logs_fmt.reporter () in
  Logs.set_reporter reporter

let infinity =
  let doc =
    "In non-AFL (quickcheck) mode, continue running until a test failure is \
     discovered.  No attempt is made to track which tests have already been \
     run, so some tests may be repeated, and if there are no failures \
     reachable, the test will never terminate without outside intervention."
  in
  Cmdliner.Arg.(value & flag & info [ "i" ] ~doc ~docv:"INFINITE")

let crowbar_info = Cmdliner.Cmd.info "test"
(* Cmdliner.Cmd.info @@ Filename.basename Sys.argv.(0) *)

let disable_z3 =
  let doc = "Z3 disable" in
  Arg.(value & flag & info [ "z" ] ~doc)
(* Arg.(value & flag & info["disable-z3"] ~doc) *)

let disable_altergo =
  let doc = "Alt-Ergo disable" in
  Arg.(value & flag & info [ "a" ] ~doc)
(* Arg.(value & flag & info["disable-alt"] ~doc) *)

let disable_cvc5 =
  let doc = "CVC5 disable" in
  Arg.(value & flag & info [ "c" ] ~doc)
(* Arg.(value & flag & info["disable-cvc5"] ~doc) *)

let disable_bitwuzla =
  let doc = "Bitwuzla disable" in
  Arg.(value & flag & info [ "b" ] ~doc)
(* Arg.(value & flag & info["disable-bitwulza"] ~doc) *)

let disable_colibri2 =
  let doc = "Colibri2 disable" in
  Arg.(value & flag & info [ "co" ] ~doc)
(* Arg.(value & flag & info["disable-colibri2"] ~doc) *)

let disable_smtzilla =
  let doc = "Smtzilla disable" in
  Arg.(value & flag & info [ "sz" ] ~doc)
(* Arg.(value & flag & info["disable-smtzilla"] ~doc) *)

let get_model =
  let doc = "get model" in
  Arg.(value & flag & info [ "model" ] ~doc)

let cmp_model =
  let doc = "Compare model" in
  Arg.(value & flag & info [ "cmp-model" ] ~doc)

let theory =
  let parse s =
    match String.lowercase_ascii s with
    | "all" -> Ok (Some Theory.ALL)
    | "qf_bv" -> Ok (Some Theory.QF_BV)
    | "qf_bvfp" -> Ok (Some Theory.QF_BVFP)
    | "qf_fp" -> Ok (Some Theory.QF_FP)
    | "qf_lia" -> Ok (Some Theory.QF_LIA)
    | "lia" -> Ok (Some Theory.LIA)
    | "only_bool" -> Ok (Some Theory.ONLY_BOOL)
    | _ -> Fmt.error_msg {|"%s" n'est pas une theorie valide|} s
  in
  let pp_req fmt = function
    | Theory.ALL -> Fmt.string fmt "ALL"
    | Theory.QF_BV -> Fmt.string fmt "QF_BV"
    | Theory.QF_BVFP -> Fmt.string fmt "QF_BVFP"
    | Theory.QF_FP -> Fmt.string fmt "QF_FP"
    | Theory.QF_LIA -> Fmt.string fmt "QF_LIA"
    | Theory.ONLY_BOOL -> Fmt.string fmt "ONLY_BOOL"
    | Theory.LIA -> Fmt.string fmt "LIA"
  in
  let pp fmt = function Some m -> pp_req fmt m | None -> assert false in
  Arg.conv (parse, pp)

let set_theory =
  let doc = "set theory" in
  Arg.(value & opt theory None & info [ "theory" ] ~doc)

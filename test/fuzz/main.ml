open Crowbar

let cmd =
  let open Cli in
  let open Cmdliner.Term.Syntax in
  let+ () = Cli.setup_log
  and+ seed
  and+ repeat
  and+ randomness_file
  and+ infinity
  and+ disable_z3
  and+ disable_altergo
  and+ disable_bitwuzla
  and+ disable_cvc5
  and+ disable_colibri2
  and+ disable_smtzilla
  and+ set_theory
  and+ get_model in
  Cmd_smt.run ~seed ~repeat ~randomness_file ~infinity ~disable_z3
    ~disable_altergo ~disable_bitwuzla ~disable_cvc5 ~disable_colibri2
    ~disable_smtzilla ~set_theory ~get_model

let cmd = Cmdliner.Cmd.v Cli.crowbar_info cmd

let cmd_print_reparse =
  let open Cli in
  let open Cmdliner.Term.Syntax in
  let+ () = Cli.setup_log
  and+ seed
  and+ repeat
  and+ set_theory
  and+ randomness_file
  and+ infinity in
  Cmd_print_parse.run_print_parse ~seed ~repeat ~set_theory ~randomness_file
    ~infinity

let cmd2 =
  let info = Cmdliner.Cmd.info "parse-reparse" in
  Cmdliner.Cmd.v info cmd_print_reparse

let cmp_model =
  let open Cli in
  let open Cmdliner.Term.Syntax in
  let+ () = Cli.setup_log
  and+ seed
  and+ repeat
  and+ randomness_file
  and+ infinity
  and+ disable_z3
  and+ disable_altergo
  and+ disable_bitwuzla
  and+ disable_cvc5
  and+ disable_colibri2
  and+ disable_smtzilla
  and+ set_theory
  and+ cmp_model in
  Cmp_model.run ~seed ~repeat ~randomness_file ~infinity ~disable_z3
    ~disable_altergo ~disable_bitwuzla ~disable_cvc5 ~disable_colibri2
    ~disable_smtzilla ~set_theory ~cmp_model

let cmd4 =
  let info = Cmdliner.Cmd.info "model" in
  Cmdliner.Cmd.v info cmp_model

let cli =
  let open Cmdliner in
  let info =
    let doc = "Smt Fuzzer" in
    Cmd.info "fuzz" ~doc
  in
  Cmd.group info [ cmd; cmd2; cmd4 ]

let exit_code = Cmdliner.Cmd.eval' cli

let () = exit exit_code

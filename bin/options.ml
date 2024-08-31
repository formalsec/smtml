open Smtml
open Cmdliner

type prove_mode =
  | Batch
  | Cached
  | Incremental

let solver_conv =
  Cmdliner.Arg.conv
    (Solver_dispatcher.solver_type_of_string, Solver_dispatcher.pp_solver_type)

let prove_mode_conv =
  Cmdliner.Arg.enum
    [ ("batch", Batch); ("cached", Cached); ("incremental", Incremental) ]

let path = ((fun s -> `Ok (Fpath.v s)), Fpath.pp)

let file0 =
  let doc = "Input file" in
  let docv = "FILE" in
  Arg.(required & pos 0 (some path) None & info [] ~doc ~docv)

let files =
  let doc = "Input files" in
  let docv = "FILES" in
  Arg.(value & pos_all path [] & info [] ~docv ~doc)

let solver =
  let doc = "SMT solver to use" in
  Arg.(value & opt solver_conv Z3_solver & info [ "s"; "solver" ] ~doc)

let solver_mode =
  let doc = "SMT solver mode" in
  Arg.(value & opt prove_mode_conv Batch & info [ "mode" ] ~doc)

let debug =
  let doc = "Print debugging messages" in
  Arg.(value & flag & info [ "debug" ] ~doc)

let print_statistics =
  let doc = "Print statistics" in
  Arg.(value & flag & info [ "st" ] ~doc)

let cmd_run main =
  let doc = "Run one script" in
  let info = Cmd.info "run" ~doc in
  Cmd.v info
    Term.(const main $ debug $ solver $ solver_mode $ print_statistics $ file0)

let cmd_test main =
  let doc =
    "Tests one or more scripts using the intermediate state. Also supports \
     directory inputs"
  in
  let info = Cmd.info "test" ~doc in
  Cmd.v info
    Term.(const main $ debug $ solver $ solver_mode $ print_statistics $ files)

let cmd_to_smt2 main =
  let doc = "Convert .smtml into .smt2" in
  let info = Cmd.info "to-smt2" ~doc in
  Cmd.v info Term.(const main $ debug $ solver $ file0)

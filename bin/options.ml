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

let dry =
  let doc = "Dry run on tests" in
  Arg.(value & flag & info [ "dry" ] ~doc)

let print_statistics =
  let doc = "Print statistics" in
  Arg.(value & flag & info [ "print-statistics" ] ~doc)

let cmd_run f =
  let doc = "Runs one or more scripts using. Also supports directory inputs" in
  let info = Cmd.info "run" ~doc in
  Cmd.v info
    Term.(
      const f $ debug $ solver $ solver_mode $ dry $ print_statistics $ files )

let cmd_to_smt2 f =
  let doc = "Convert .smtml into .smt2" in
  let info = Cmd.info "to-smt2" ~doc in
  Cmd.v info Term.(const f $ debug $ solver $ file0)

open Smtml

type prover =
  | Z3_prover
  | Colibri2_prover
  | Bitwuzla_prover

let prover_conv =
  Cmdliner.Arg.enum
    [ ("z3", Z3_prover)
    ; ("Z3", Z3_prover)
    ; ("c2", Colibri2_prover)
    ; ("colibri2", Colibri2_prover)
    ; ("Colibri2", Colibri2_prover)
    ; ("bitwuzla", Bitwuzla_prover)
    ]

let parse_cmdline =
  let aux files prover incremental debug =
    let module Mappings =
      ( val match prover with
            | Z3_prover -> (module Z3_mappings)
            | Colibri2_prover -> (module Colibri2_mappings)
            | Bitwuzla_prover -> (module Bitwuzla_mappings)
          : Mappings_intf.S )
    in
    Mappings.set_debug debug;
    let module Interpret =
      ( val if incremental then
              (module Interpret.Make (Solver.Incremental (Mappings)))
            else (module Interpret.Make (Solver.Batch (Mappings)))
          : Interpret_intf.S )
    in
    match files with
    | [] ->
      let ast = Parse.from_file ~filename:"-" in
      ignore @@ Interpret.start ast
    | _ ->
      ignore
      @@ List.fold_left
           (fun state filename ->
             let ast = Parse.from_file ~filename in
             Some (Interpret.start ?state ast) )
           None files
  in
  let open Cmdliner in
  let files =
    Arg.(value & pos_all string [] & info [] ~docv:"files" ~doc:"files to read")
  and prover =
    Arg.(
      value & opt prover_conv Z3_prover
      & info [ "p"; "prover" ] ~doc:"SMT solver to use" )
  and incremental =
    Arg.(
      value & flag
      & info [ "incremental" ] ~doc:"Use the SMT solver in the incremental mode" )
  and debug =
    Arg.(value & flag & info [ "debug" ] ~doc:"Print debugging messages")
  in
  Cmd.v
    (Cmd.info "smtml" ~version:"%%VERSION%%")
    Term.(const aux $ files $ prover $ incremental $ debug)

let () =
  match Cmdliner.Cmd.eval_value parse_cmdline with
  | Error (`Parse | `Term | `Exn) -> exit 2
  | Ok (`Ok () | `Version | `Help) -> ()

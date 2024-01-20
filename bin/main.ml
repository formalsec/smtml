open Encoding

let get_contents = function
  | "-" -> In_channel.input_all In_channel.stdin
  | filename ->
    let chan = open_in filename in
    Fun.protect
      ~finally:(fun () -> close_in chan)
      (fun () -> In_channel.input_all chan)

let parse_file file = get_contents file |> Parse.from_string

type prover =
  | Z3_prover
  | Colibri2_prover

let prover_conv =
  Cmdliner.Arg.conv
    ( (fun str ->
        try
          Ok
            ( match str with
            | "Z3" | "z3" -> Z3_prover
            | "Colibri2" | "colibri2" -> Colibri2_prover
            | _ ->
              failwith (Fmt.str "Unsupported prover %s, try Z3 or Colibri2" str)
            )
        with Failure s -> Error (`Msg s) )
    , fun fmt -> function
        | Z3_prover -> Fmt.pf fmt "Z3"
        | Colibri2_prover -> Fmt.pf fmt "Colibri2" )

let parse_cmdline =
  let aux files prover incremental debug =
    let module Mappings =
      ( val match prover with
            | Z3_prover -> (module Z3_mappings)
            | Colibri2_prover -> (module Colibri2_mappings)
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
      let ast = parse_file "-" in
      ignore @@ Interpret.start ast
    | _ ->
      ignore
      @@ List.fold_left
           (fun state file ->
             let ast = Parse.from_file ~filename:file in
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

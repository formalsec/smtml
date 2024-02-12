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
  Cmdliner.Arg.enum
    [ ("z3", Z3_prover)
    ; ("Z3", Z3_prover)
    ; ("c2", Colibri2_prover)
    ; ("colibri2", Colibri2_prover)
    ; ("Colibri2", Colibri2_prover)
    ]

let files =
  let doc = "Source file(s)." in
  Cmdliner.Arg.(value & pos_all non_dir_file [] & info [] ~doc)

let run_cmd =
  let run files prover incremental debug =
    let module Mappings =
      ( val match prover with
            | Z3_prover -> (module Z3_mappings)
            | Colibri2_prover ->
              Log.err "Please install Colibri2 and use 'smtml2'"
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
  let prover =
    let doc = "SMT solver to use" in
    Arg.(value & opt prover_conv Z3_prover & info [ "p"; "prover" ] ~doc)
  and incremental =
    let doc = "Use the SMT solver in the incremental mode" in
    Arg.(value & flag & info [ "incremental" ] ~doc)
  and debug =
    let doc = "Print debugging messages" in
    Arg.(value & flag & info [ "debug" ] ~doc)
  in
  let info = Cmd.info "run" in
  Cmd.v info Term.(const run $ files $ prover $ incremental $ debug)

let fmt_cmd =
  let open Cmdliner in
  let fmt files inplace =
    let open Format in
    let pp_ast fmt ast =
      pp_print_list ~pp_sep:pp_print_newline Ast.pp fmt ast
    in
    match files with
    | [] -> pp_ast std_formatter (parse_file "-")
    | _ ->
      List.iter
        (fun file ->
          let ast = parse_file file in
          if inplace then
            Out_channel.with_open_text file (fun out ->
                Format.fprintf (formatter_of_out_channel out) "%a@." pp_ast ast )
          else Format.printf "%a@." pp_ast ast )
        files
  in
  let inplace =
    let doc = "Format in-place, overwriting input file(s)." in
    Cmdliner.Arg.(value & flag & info [ "inplace"; "i" ] ~doc)
  in
  let info = Cmd.info "fmt" in
  Cmd.v info Term.(const fmt $ files $ inplace)

let cli =
  let open Cmdliner in
  let info = Cmd.info "smtml" ~version:"%%VERSION%%" in
  Cmd.group info [ run_cmd; fmt_cmd ]

let () =
  match Cmdliner.Cmd.eval_value cli with
  | Error (`Parse | `Term | `Exn) -> exit 2
  | Ok (`Ok () | `Version | `Help) -> ()

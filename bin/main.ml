open Encoding
open Cmdliner
module Z3_batch = Solver.Batch (Z3_mappings)
module Z3_incremental = Solver.Incremental (Z3_mappings)
module Interpret = Interpret.Make (Z3_batch)

let ( let*! ) o f = match o with Error msg -> failwith msg | Ok v -> f v

let get_contents = function
  | "-" -> In_channel.input_all In_channel.stdin
  | filename ->
    let chan = open_in filename in
    Fun.protect
      ~finally:(fun () -> close_in chan)
      (fun () -> In_channel.input_all chan)

let parse_file file =
  let*! ast = get_contents file |> Parse.Script.from_string in
  ast

let fmt file =
  let es =
    parse_file file
    |> List.map (function Ast.Assert e -> [ e ] | _ -> [])
    |> List.flatten
  in
  let script = Expr.Smtlib.to_script es in
  Format.printf "%a" Smtlib.Fmt.pp_script script

let run files =
  match files with
  | [] ->
    let ast = parse_file "-" in
    ignore @@ Interpret.start ast
  | _ ->
    ignore
    @@ List.fold_left
         (fun state file ->
           let*! ast = Parse.Script.from_file file in
           Some (Interpret.start ?state ast) )
         None files

let help = [ `S Manpage.s_common_options ]
let sdocs = Manpage.s_common_options

let fmt_cmd =
  let file =
    let doc = "source file" in
    Arg.(required & pos 0 (some non_dir_file) None & info [] ~doc)
  in
  let doc = "format smt-lib scripts" in
  let info = Cmd.info "fmt" ~doc ~sdocs in
  Cmd.v info Term.(const fmt $ file)

let run_cmd =
  let files =
    let doc = "source files" in
    Arg.(value & pos_all non_dir_file [] & info [] ~doc)
  in
  let doc = "interpret smt-lib files" in
  let info = Cmd.info "run" ~doc ~sdocs in
  Cmd.v info Term.(const run $ files)

let cli =
  let doc = "a toy smt-lib interpreter" in
  let man = help in
  let info = Cmd.info "smtml" ~version:"%%VERSION%%" ~doc ~sdocs ~man in
  Cmd.group info [ run_cmd; fmt_cmd ]

let () = exit @@ Cmd.eval cli

open Encoding
open Cmdliner
module Z3_batch = Solver.Batch (Z3_mappings)
module Z3_incremental = Solver.Incremental (Z3_mappings)
module Interpret = Interpret.Make (Z3_batch)

let get_contents = function
  | "-" -> In_channel.input_all In_channel.stdin
  | filename ->
    let chan = open_in filename in
    Fun.protect
      ~finally:(fun () -> close_in chan)
      (fun () -> In_channel.input_all chan)

let parse_file file = get_contents file |> Parse.from_string

let run files =
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

let fmt files =
  let pp_ast fmt ast =
    Format.pp_print_list ~pp_sep:Format.pp_print_newline Ast.pp fmt ast
  in
  match files with
  | [] -> Format.printf "%a@." pp_ast (parse_file "-")
  | _ ->
    List.iter (fun file -> Format.printf "%a@." pp_ast (parse_file file)) files

let files =
  let doc = "source files" in
  Arg.(value & pos_all non_dir_file [] & info [] ~doc)

let run_cmd =
  let info = Cmd.info "run" in
  Cmd.v info Term.(const run $ files)

let fmt_cmd =
  let info = Cmd.info "fmt" in
  Cmd.v info Term.(const fmt $ files)

let cli =
  let info = Cmd.info "smtml" ~version:"%%VERSION%%" in
  Cmd.group info [ run_cmd; fmt_cmd ]

let () =
  Printexc.record_backtrace true;
  exit @@ Cmd.eval cli

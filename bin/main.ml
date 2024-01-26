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

let fmt files inplace =
  let open Format in
  let pp_ast fmt ast = pp_print_list ~pp_sep:pp_print_newline Ast.pp fmt ast in
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

let files =
  let doc = "Source file(s)." in
  Arg.(value & pos_all non_dir_file [] & info [] ~doc)

let inplace =
  let doc = "Format in-place, overwriting input file(s)." in
  Arg.(value & flag & info [ "inplace"; "i" ] ~doc)

let run_cmd =
  let info = Cmd.info "run" in
  Cmd.v info Term.(const run $ files)

let fmt_cmd =
  let info = Cmd.info "fmt" in
  Cmd.v info Term.(const fmt $ files $ inplace)

let cli =
  let info = Cmd.info "smtml" ~version:"%%VERSION%%" in
  Cmd.group info [ run_cmd; fmt_cmd ]

let () =
  Printexc.record_backtrace true;
  exit @@ Cmd.eval cli

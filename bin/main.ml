open Core
open Encoding

module Z3_batch = Batch.Make (Z3_mappings)
module Z3_incremental = Incremental.Make (Z3_mappings)
module Interpret = Interpret.Make (Z3_incremental)

let get_contents = function
  | "-" -> In_channel.input_all In_channel.stdin
  | filename -> In_channel.read_all filename

let parse_file file = get_contents file |> Run.parse_string

let rec find_smt2_files path =
  match Sys_unix.is_directory path with
  | `Yes ->
    Sys_unix.ls_dir path
    |> List.map ~f:(fun subpath -> find_smt2_files (Filename.concat path subpath))
    |> List.concat
  | `No ->
    if Filename.check_suffix path ".smt2" then [path] else []
  | `Unknown -> []

let mode_of_string = function
  | "n" -> 0
  | "d" -> 1
  | _ -> 2  

let command =
  Command.basic ~summary:"SMTLIB-like parser and interpreter"
    ~readme:(fun () -> "More detailed information")
    (let%map_open.Command targets =
        anon (sequence ("filename/directory" %: Filename_unix.arg_type))
      and mode = 
        flag "-mode" (optional_with_default 2 (Arg_type.create mode_of_string)) 
          ~doc:"MODE is 'n' for Normalize And Cache, 'd' for Dumb Cache, or leave unspecified for No Cache"
      in
      fun () ->
        match targets with
        | [] ->
          let ast = parse_file "-" in
          Interpret.start ast mode
        | _ ->
          let files = List.concat_map targets ~f:find_smt2_files |> List.sort ~compare:String.compare in
          let asts = List.map files ~f:parse_file in
          List.iter asts ~f:(fun ast -> Interpret.start ast mode))
    

let () = Command_unix.run ~version:"0.1" command

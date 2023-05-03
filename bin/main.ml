open Core
open Encoding

let get_contents = function
  | "-" -> In_channel.input_all In_channel.stdin
  | filename -> In_channel.read_all filename

let parse_file file =
  get_contents file |> Run.parse_string

let command =
  Command.basic ~summary:"SMT-LIB v2.6 parser and interpreter"
    ~readme:(fun () -> "More detailed information")
    (let%map_open.Command files =
       anon (sequence ("filename" %: Filename_unix.arg_type))
       (*and trial = flag "-t" no_arg ~doc:" run a built-in time trial" in*)
     in
     fun () ->
       match files with
       | [] -> parse_file "-"
       | _ -> List.iter files ~f:parse_file)

let () = Command_unix.run ~version:"0.1" command

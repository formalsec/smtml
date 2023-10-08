open Lexer
open Lexing

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  Printf.fprintf outx "%s:%d:%d" pos.pos_fname pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
  try Parser.script Lexer.token lexbuf with
  | SyntaxError msg ->
    Printf.fprintf stderr "%a: %s\n" print_position lexbuf msg;
    []
  | Parser.Error ->
    Printf.fprintf stderr "%a: syntax error\n" print_position lexbuf;
    exit 1

let parse_file filename =
  let chan = open_in filename in
  Fun.protect
    ~finally:(fun () -> close_in chan)
    (fun () ->
      let lexbuf = Lexing.from_channel chan in
      lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
      parse_with_error lexbuf )

let parse_string contents = parse_with_error (Lexing.from_string contents)

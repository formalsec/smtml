open Lexer
open Lexing
open Format

let pp_pos fmt lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf fmt "%s:%d:%d" pos.pos_fname pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
  try Parser.script Lexer.token lexbuf with
  | SyntaxError msg ->
    eprintf "%a: %s\n" pp_pos lexbuf msg;
    []
  | Parser.Error ->
    eprintf "%a: syntax error\n" pp_pos lexbuf;
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

{
open Core
open Lexing
open Parser

exception SyntaxError of string
}

let white   = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let digit   = ['0'-'9']
let letter  = ['a'-'z' 'A'-'Z']
let numeral = '0' | [ '1'-'9' ] digit*
let decimal = numeral '.' '0'* numeral
let hexadec = "#x" (['a'-'f' 'A'-'F'] | digit)+
let binary  = "#b" ('0' | '1')+

let s = 
  '~' | '!' | '@' | '$' | '%' | '^' | '&' | '*' | '_' | '-' | '+' |
  '=' | '<' | '>' | '.' | '?' | '/'

let symbol  = (letter | s) (letter | digit | s)*
(* TODO: Quoted symbols: |symbol| *)

rule read =
  parse
  | white   { read lexbuf }
  | newline { new_line lexbuf; read lexbuf }
  | numeral { NUM (Int.of_string (Lexing.lexeme lexbuf)) }
  | decimal { DEC (Float.of_string (Lexing.lexeme lexbuf)) }
  | hexadec { failwith "TODO: Lexer(hexadec)" }
  | binary  { failwith "TODO: Lexer(binary)" }
  | symbol  { SYMBOL (Lexing.lexeme lexbuf) }
  | ';'     { read_comment lexbuf }
  | '"'     { read_string (Buffer.create 17) lexbuf }
  | '('     { LPAREN }
  | ')'     { RPAREN }
  | '_'     { HOLE }
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof     { EOF }

and read_comment =
  parse
  | newline { new_line lexbuf; read lexbuf }
  | _ { read_comment lexbuf }

and read_string buf =
  parse
  | '"' { STR (Buffer.contents buf) }
  | '"' '"' { Buffer.add_char buf '"'; read_string buf lexbuf }
  | [^ '"']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf); read_string buf lexbuf }
  | _ { raise (SyntaxError ("Illegal string char: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (SyntaxError "String is not terminated") }


{
open Core
open Lexing
open Parser

exception SyntaxError of string

let error msg = raise (SyntaxError msg)
}

let white   = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let digit   = ['0'-'9']
let letter  = ['a'-'z' 'A'-'Z']
let numeral = '0' | [ '1'-'9' ] digit*
let decimal = numeral '.' '0'* numeral
let hexadec = "#x" (['a'-'f' 'A'-'F'] | digit)+
let binary  = "#b" ('0' | '1')+
let symbols = ['~''!''@''$''%''^''&''*''_''-''+''=''<''>''.''?''/']
let symbol  = (letter | symbols) (letter | digit | symbols)*
(* TODO: Quoted symbols: |symbol| *)

rule token = parse
  | '('     { LPAREN }
  | ')'     { RPAREN }

  | numeral as s { NUM (Int.of_string s) }
  | decimal as s { DEC (Float.of_string s) }
  | hexadec { failwith "TODO: Lexer(hexadec)" }
  | binary  { failwith "TODO: Lexer(binary)" }
  | '"'     { string (Buffer.create 17) lexbuf }

  | '_'     { HOLE }

  | symbol as s  { SYMBOL s }

  | ';'     { comment lexbuf }
  | white   { token lexbuf }
  | newline { new_line lexbuf; token lexbuf }
  | eof     { EOF }

  | _ { error ("Unexpected char: " ^ Lexing.lexeme lexbuf) }

and comment = parse
  | newline { new_line lexbuf; token lexbuf }
  | _ { comment lexbuf }

and string buf = parse
  | '"' { STR (Buffer.contents buf) }
  | '"' '"' { Buffer.add_char buf '"'; string buf lexbuf }
  | [^ '"']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf); string buf lexbuf }
  | eof { error "nonterminated string" }
  | _ { error ("illegal string char: " ^ Lexing.lexeme lexbuf) }


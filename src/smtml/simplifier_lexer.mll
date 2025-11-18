(* lexer.mll *)
{
  open Simplifier_parser
}

rule token = parse
  | [' ' '\t' '\n'] { token lexbuf } (* Skip whitespace *)
  | "(*"     { comment lexbuf }     (* Comments *)
  | "==>"    { IMPLIES }
  | "when"   { WHEN }
  | "&&"     { AND }
  | "||"     { OR }
  | "!"      { NOT }
  | "=="     { EQ }
  | "!="     { NEQ }
  | ">="     { GEQ }
  | "<="     { LEQ }
  | ">"      { GT }
  | "<"      { LT }
  | "+"      { PLUS }
  | "-"      { MINUS }
  | "*"      { TIMES }
  | "/"      { DIV }
  | "("      { LPAREN }
  | ")"      { RPAREN }
  | "["      { LBRACK }
  | "]"      { RBRACK }
  | ","      { COMMA }
  | "."      { DOT }
  | "_"      { UNDERSCORE }
  | ['a'-'z']['a'-'z' '0'-'9' '_']* as s { LOWERID s } (* x, e1, l, concrete, eval, rev *)
  | ['A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as s { UPPERID s } (* Not, Add, Val, List, Bitvector *)
  | ['0'-'9']+ as i { INT (int_of_string i) }
  | eof      { EOF }

and comment = parse
  | "*)"     { token lexbuf }
  | "(*"     { comment lexbuf } (* nested comments *)
  | "\n"     { Lexing.new_line lexbuf; comment lexbuf }
  | _        { comment lexbuf }
  | eof      { failwith "Comment not terminated" }
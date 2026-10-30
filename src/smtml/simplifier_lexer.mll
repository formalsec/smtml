(* lexer.mll *)
{
  open Simplifier_parser
}

rule token = parse
  | [' ' '\t'] { token lexbuf }      (* Skip spaces and tabs *)
  | '\n'     { Lexing.new_line lexbuf; token lexbuf } (* Track line numbers *)
  | "(*"     { comment 0 lexbuf }    (* Comments *)
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

and comment depth = parse
  | "*)"     { if depth = 0 then token lexbuf else comment (depth - 1) lexbuf }
  | "(*"     { comment (depth + 1) lexbuf } (* nested comments *)
  | "\n"     { Lexing.new_line lexbuf; comment depth lexbuf }
  | _        { comment depth lexbuf }
  | eof      { failwith "Comment not terminated" }
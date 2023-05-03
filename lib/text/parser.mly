%token <int> NUM
%token <float> DEC
%token <string> STR
%token <string> SYMBOL
%token LPAREN
%token RPAREN
%token HOLE
%token EOF

%start <unit> script
%%

script:
  | EOF        { () }
  | e = s_expr { Core.printf "%s\n" e }
  ;

s_expr:
  | c = spec_constant { c }
  | s = SYMBOL { s }
  | LPAREN; RPAREN { "()" }
  | LPAREN; e = s_expr; RPAREN {"(" ^ e ^ ")" }
  ;

spec_constant :
  | x = NUM { Int.to_string x }
  | x = DEC { Float.to_string x }
  | x = STR { x }
  ;

index :
  | x = NUM { Int.to_string x }
  | x = SYMBOL { s }
  ;

identifier :
  | x = SYMBOL { s }
  | LPAREN; HOLE; x = SYMBOL; i = index; RPAREN { "(_ " ^ x ^ " " ^ i ^ ")" }
  ;

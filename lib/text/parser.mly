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
  | LPAREN; s = SYMBOL; es = list(s_expr); RPAREN
    { "("^ s ^ " " ^ (Core.String.concat ~sep:" " es) ^ ")" }
  ;

spec_constant :
  | x = NUM { Int.to_string x }
  | x = DEC { Float.to_string x }
  | x = STR { x }
  | x = SYMBOL { x }
  ;

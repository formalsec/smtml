%token <int> NUM
%token <float> DEC
%token <string> STR
%token LPAREN
%token RPAREN
%token EOF

%start <Expression.t option> script
%%

script:
  | EOF      { None }
  | e = term { Some e }
  ;

s_expr:
  | c = spec_constant { c }
  | LPAREN; e = s_expr; RPAREN { e }
  ;

term:
  | c = spec_constant { c }
  ;

spec_constant :
  | x = NUM { Integer.mk_val x }
  | x = DEC { Real.mk_val x }
  | x = STR { Strings.mk_val x }
  ;

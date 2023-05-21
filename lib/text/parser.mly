%{
open Core
open Value
open Expression

let varmap = Hashtbl.create (module String)

let add_bind x t = Hashtbl.add_exn varmap ~key:x ~data:t
let get_bind x = Hashtbl.find_exn varmap x

%}
%token LPAREN
%token RPAREN
%token DECL ASSERT CHECKSAT
(*%token HOLE*)
%token EOF

%token <int> NUM
%token <float> DEC
%token <bool> BOOL
%token <string> STR
%token <string> SYMBOL
%token <Types.unop> UNARY
%token <Types.binop> BINARY
%token <Types.triop> TERNARY 
%token <Types.relop> RELOP
%token <Types.cvtop> CVTOP
%token <Types.expr_type> TYPE

%start <Ast.t list> script
%%

script: list(stmt) EOF { $1 }

stmt:
  | LPAREN; DECL; x = SYMBOL; t = TYPE; RPAREN
    {
      add_bind x t;
      Ast.Decl (Symbol.mk_symbol t x) 
    }
  | LPAREN; ASSERT; e = s_expr; RPAREN { Ast.Assert e }
  | LPAREN; CHECKSAT; RPAREN { Ast.CheckSat }
  ;

s_expr:
  | x = SYMBOL { mk_symbol_s (get_bind x) x }
  | c = spec_constant { Val c }
  | LPAREN; op = UNARY; e = s_expr; RPAREN { Unop (op, e) }
  | LPAREN; op = BINARY; e1 = s_expr; e2 = s_expr; RPAREN { Binop (op, e1, e2) }
  | LPAREN; op = TERNARY; e1 = s_expr; e2 = s_expr; e3 = s_expr; RPAREN
    { Triop (op, e1, e2, e3) }
  | LPAREN; op = CVTOP; e = s_expr; RPAREN { Cvtop (op, e) }
  | LPAREN; op = RELOP; e1 = s_expr; e2 = s_expr; RPAREN { Relop (op, e1, e2) }
  ;

spec_constant :
  | NUM { Int $1 }
  | DEC { Real $1 }
  | STR { Str $1 }
  | BOOL { Bool $1 }
  ;

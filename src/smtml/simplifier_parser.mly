%{
  open Simplifier_ast.Dsl_ast
%}

%token <string> LOWERID UPPERID
%token <int> INT
%token UNDERSCORE
%token IMPLIES WHEN AND OR NOT
%token EQ NEQ GEQ LEQ GT LT
%token PLUS MINUS TIMES DIV
%token LPAREN RPAREN LBRACK RBRACK COMMA DOT EOF

%left OR
%left AND
%right NOT
%nonassoc EQ NEQ LEQ GEQ LT GT
%left PLUS MINUS
%left TIMES DIV

%start <Simplifier_ast.Dsl_ast.rule list> rules
%%

rules:
  | r = rule; rs = rules { r :: rs }
  | EOF                { [] }
;

rule:
  | lhs = lhs_pattern; IMPLIES; rhs = rhs_expression; WHEN; cond = condition
    { { lhs; rhs; cond = Some cond } }
  | lhs = lhs_pattern; IMPLIES; rhs = rhs_expression
    { { lhs; rhs; cond = None } }
;

/* --- 1. LHS Pattern Grammar --- */
lhs_atom:
  | v = LOWERID               { LVar v }
  | UNDERSCORE                { LVar "_" }
  | c = UPPERID; LPAREN; args = separated_list(COMMA, lhs_pattern); RPAREN
                              { LConstructor (c, args) }
  | c = UPPERID               { LConstructor (c, []) }
  | LBRACK; items = separated_list(COMMA, lhs_pattern); RBRACK
                              { LList items }
;

lhs_pattern:
  | a = lhs_atom { a }
  | c = UPPERID; args = non_empty_lhs_atom_list
    { LConstructor (c, args) }

rhs_atom:
  | v = LOWERID               { RVar v }
  | i = INT                   { RInt i }
  | c = UPPERID; LPAREN; args = separated_list(COMMA, rhs_expression); RPAREN
                              { RConstructor (c, args) }
  | c = UPPERID               { RConstructor (c, []) }
  | f = LOWERID; LPAREN; args = separated_list(COMMA, rhs_expression); RPAREN
                              { RFuncall (f, args) }
  | LBRACK; items = separated_list(COMMA, rhs_expression); RBRACK
                              { RList items }
;

rhs_expression:
  | a = rhs_atom { a }
  | m = UPPERID; DOT; f = LOWERID; args = non_empty_rhs_atom_list
    { RNamespacedFuncall (m, f, args) }
  | c = UPPERID; args = non_empty_rhs_atom_list
    { RConstructor (c, args) }
;

non_empty_lhs_atom_list:
  | e = lhs_atom
    { [e] }
  | e = lhs_atom; l = non_empty_lhs_atom_list
    { e :: l }
;

non_empty_rhs_atom_list:
  | e = rhs_atom
    { [e] }
  | e = rhs_atom; l = non_empty_rhs_atom_list
    { e :: l }
;

condition:
  | c1 = condition; AND; c2 = condition { CAnd (c1, c2) }
  | c1 = condition; OR; c2 = condition  { COr (c1, c2) }
  | NOT; c = condition                 { CNot c }
  | r = rel_expr                     { r }
;

rel_expr:
  | a1 = arith_expr; op = rel_op; a2 = arith_expr
    { CInfix (a1, op, a2) }
  | a = arith_expr
    { a }
;

arith_expr:
  | a1 = arith_expr; PLUS; a2 = arith_expr  { CInfix (a1, "+", a2) }
  | a1 = arith_expr; MINUS; a2 = arith_expr { CInfix (a1, "-", a2) }
  | a1 = arith_expr; TIMES; a2 = arith_expr { CInfix (a1, "*", a2) }
  | a1 = arith_expr; DIV; a2 = arith_expr   { CInfix (a1, "/", a2) }
  | a = atom                               { a }
;

atom:
  | m = UPPERID; DOT; f = LOWERID; args = non_empty_lhs_atom_list
                                { CNamespacedApp (m, f, List.map (fun p -> CPat p) args) }
  | p = lhs_pattern             { CPat p }
  | i = INT                     { CInt i }
  | f = LOWERID; LPAREN; args = separated_list(COMMA, condition); RPAREN
                                { CApp (f, args) }
  | m = UPPERID; DOT; f = LOWERID; LPAREN; args = separated_list(COMMA, condition); RPAREN
                                { CNamespacedApp (m, f, args) }
  | LPAREN; c = condition; RPAREN
                                { c }
;

rel_op:
  | EQ  { "=" } | NEQ { "!=" } | LT  { "<" }
  | LEQ { "<=" } | GT  { ">" }  | GEQ { ">=" }
;
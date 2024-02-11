%{
open Ty
open Ast
open Expr
open Value

let varmap = Hashtbl.create 512

let add_bind (x, t) = Hashtbl.replace varmap x t
let get_bind x = Hashtbl.find varmap x

%}
%token PTR
%token LET
%token EXTRACT
%token CONCAT
%token LPAREN
%token RPAREN
%token ASSERT
%token LET_CONST CHECK_SAT GET_MODEL
%token PUSH POP
(*%token HOLE*)
%token EOF

%token <int> NUM
%token <float> DEC
%token <bool> BOOL
%token <string> STR
%token <string> SYMBOL
%token <Ty.t * Ty.unop> UNARY
%token <Ty.t * Ty.binop> BINARY
%token <Ty.t * Ty.triop> TERNARY
%token <Ty.t * Ty.relop> RELOP
%token <Ty.t * Ty.cvtop> CVTOP
%token <Ty.t> TYPE

%start <Ast.t list> script
%%

let script := stmts = list(stmt); EOF; { stmts }

let stmt :=
  | LPAREN; LET_CONST; x = SYMBOL; t = TYPE; RPAREN;
    {
      add_bind (x, t);
      Ast.Let_const (Symbol.make t x)
    }
  | LPAREN; ASSERT; ~ = term; RPAREN; <Ast.Assert>
  | LPAREN; CHECK_SAT; RPAREN; { Ast.Check_sat }
  | LPAREN; PUSH; RPAREN; { Ast.Push }
  | LPAREN; POP; ~ = NUM; RPAREN; <Ast.Pop>
  | LPAREN; GET_MODEL; RPAREN; { Ast.Get_model }

let var_binding :=
  | LPAREN; x = SYMBOL; ~ = term; RPAREN;
    {
      add_bind (x, Ty_str);
      (x,  term)
    }

let term :=
  | ~ = s_expr; <E>
  | LPAREN; LET; LPAREN; ~ = var_binding+; RPAREN; ~ = term; RPAREN; <Let>

let s_expr :=
  | x = SYMBOL; { mk_symbol @@ Symbol.make (get_bind x) x }
  | c = spec_constant; { mk (Val c) }
  | LPAREN; op = paren_op; RPAREN; { mk op }

let paren_op :=
  | PTR; LPAREN; _ = TYPE; x = NUM; RPAREN; e = s_expr;
    { Ptr (Int32.of_int x, e) }
  | (ty, op) = UNARY; ~ = s_expr; <Unop>
  | (ty, op) = BINARY; e1 = s_expr; e2 = s_expr; <Binop>
  | (ty, op) = TERNARY; e1 = s_expr; e2 = s_expr; e3 = s_expr; <Triop>
  | (ty, op) = CVTOP; ~ = s_expr; <Cvtop>
  | (ty, op) = RELOP; e1 = s_expr; e2 = s_expr; <Relop>
  | EXTRACT; ~ = s_expr; ~ = NUM; ~ = NUM; <Extract>
  | CONCAT; ~ = s_expr+; <Concat>

let spec_constant :=
  | x = NUM; <Int>
  | x = DEC; <Real>
  | x = STR; <Str>
  | x = BOOL; { if x then True else False }
  | LPAREN; ty = TYPE; x = NUM; RPAREN;
    {
      match ty with
      | Ty_bitv 32 -> Num (I32 (Int32.of_int x))
      | Ty_bitv 64 -> Num (I64 (Int64.of_int x))
      | _ -> failwith "invalid bitv type"
    }
  | LPAREN; ty = TYPE; x = DEC; RPAREN;
    {
      match ty with
      | Ty_fp 32 -> Num (F32 (Int32.bits_of_float x))
      | Ty_fp 64 -> Num (F64 (Int64.bits_of_float x))
      | _ -> failwith "invalid fp type"
    }

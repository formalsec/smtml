%{
open Value
open Expr
open Ty

let varmap = Hashtbl.create 512

let add_bind x t = Hashtbl.replace varmap x t
let get_bind x = Hashtbl.find varmap x

%}
%token LPAREN
%token RPAREN
%token ASSERT
%token DECLARE_FUN CHECK_SAT GET_MODEL
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
  | LPAREN; DECLARE_FUN; x = SYMBOL; t = TYPE; RPAREN;
    {
      add_bind x t;
      Ast.Declare (Symbol.mk_symbol t x)
    }
  | LPAREN; ASSERT; e = s_expr; RPAREN; { Ast.Assert e }
  | LPAREN; CHECK_SAT; RPAREN; { Ast.CheckSat }
  | LPAREN; GET_MODEL; RPAREN; { Ast.GetModel }

let s_expr :=
  | x = SYMBOL; { mk_symbol @@ Symbol.mk_symbol (get_bind x) x }
  | c = spec_constant; { { ty = Value.type_of c; e = Val c } }
  | LPAREN; (ty, op) = UNARY; e = s_expr; RPAREN;
    { { ty; e = Unop (op, e) } }
  | LPAREN; (ty, op) = BINARY; e1 = s_expr; e2 = s_expr; RPAREN;
    { { ty; e = Binop (op, e1, e2) } }
  | LPAREN; (ty, op) = TERNARY; e1 = s_expr; e2 = s_expr; e3 = s_expr; RPAREN;
    { { ty; e = Triop (op, e1, e2, e3) } }
  | LPAREN; (ty, op) = CVTOP; e = s_expr; RPAREN;
    { { ty; e = Cvtop (op, e) } }
  | LPAREN; (ty, op) = RELOP; e1 = s_expr; e2 = s_expr; RPAREN;
    { { ty; e = Relop (op, e1, e2) } }

let spec_constant :=
  | x = NUM; { Int x }
  | x = DEC; { Real x }
  | x = STR; { Str x }
  | x = BOOL; { Bool x }
  | LPAREN; ty = TYPE; x = NUM; RPAREN;
    {
      match ty with
      | Ty_bitv S32 -> Num (I32 (Int32.of_int x))
      | Ty_bitv S64 -> Num (I64 (Int64.of_int x))
      | _ -> failwith "invalid integer type"
    }
  | LPAREN; ty = TYPE; x = DEC; RPAREN;
    {
      match ty with
      | Ty_fp S32 -> Num (F32 (Int32.bits_of_float x))
      | Ty_fp S64 -> Num (F64 (Int64.bits_of_float x))
      | _ -> failwith "invalid integer type"
    }

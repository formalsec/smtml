(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2024 formalsec *)
(* Written by the Smtml programmers *)

%{
open Value
open Ty
open Expr

let varmap = Hashtbl.create 512

let add_bind x t = Hashtbl.replace varmap x t
let get_bind x = Hashtbl.find_opt varmap x

%}
%token PTR
%token EXTRACT
%token CONCAT
%token LPAREN
%token RPAREN
%token ASSERT
%token LET_CONST CHECK_SAT GET_MODEL SET_LOGIC
%token PUSH POP
(*%token HOLE*)
%token EOF

%token <int> NUM
%token <float> DEC
%token <bool> BOOL
%token <string> STR
%token <string> SYMBOL
%token <Utils_parse.unop> UNARY
%token <Ty.t * Ty.Binop.t> BINARY
%token <Ty.t * Ty.Triop.t> TERNARY
%token <Ty.t * Ty.Relop.t> RELOP
%token <Ty.t * Ty.Cvtop.t> CVTOP
%token <Ty.t * Ty.Naryop.t> NARY
%token <Ty.t> TYPE
%token <Logic.t> LOGIC

%start <Ast.t list> script
%%

let script := stmts = list(stmt); EOF; { stmts }

let stmt :=
  | LPAREN; LET_CONST; x = SYMBOL; t = TYPE; RPAREN;
    {
      add_bind x t;
      Ast.Declare_const { id = (Symbol.make t x); sort = (Symbol.make t x) }
    }
  | LPAREN; ASSERT; ~ = s_expr; RPAREN; <Ast.Assert>
  | LPAREN; CHECK_SAT; RPAREN; { Ast.Check_sat [] }
  | LPAREN; PUSH; RPAREN; { Ast.Push 1 }
  | LPAREN; POP; n = NUM; RPAREN; { Ast.Pop n }
  | LPAREN; GET_MODEL; RPAREN; { Ast.Get_model }
  | LPAREN; SET_LOGIC; ~ = LOGIC; RPAREN; <Ast.Set_logic>

let s_expr :=
  | x = SYMBOL; {
    match get_bind x with
    | None -> assert false
    | Some v -> Expr.symbol (Symbol.make v x)
  }
  | c = spec_constant; { value c }
  | LPAREN; op = paren_op; RPAREN; { op }

let paren_op :=
  | PTR; LPAREN; _ = TYPE; x = NUM; RPAREN; offset = s_expr;
    { Expr.ptr (Int32.of_int x) offset }
  | op = UNARY; e = s_expr;
    { let U (ty, op) = op in Expr.unop ty op e }
  | (ty, op) = BINARY; e1 = s_expr; e2 = s_expr;
    { let Ty ty = ty in Expr.binop ty op e1 e2 }
  | (ty, op) = TERNARY; e1 = s_expr; e2 = s_expr; e3 = s_expr;
    { let Ty ty = ty in Expr.triop ty op e1 e2 e3 }
  | (ty, op) = CVTOP; e = s_expr;
    { let Ty ty = ty in Expr.cvtop ty op e }
  | (ty, op) = RELOP; e1 = s_expr; e2 = s_expr;
    { let Ty ty = ty in Expr.relop ty op e1 e2 }
  | (ty, op) = NARY; es = list(s_expr);
    { let Ty ty = ty in
      Expr.naryop ty op es }
  | EXTRACT; ~ = s_expr; l = NUM; h = NUM;
    { Expr.extract s_expr ~high:h ~low:l }
  | CONCAT; e1 = s_expr; e2 = s_expr;
    { Expr.concat e1 e2 }

let spec_constant :=
  | x = NUM; { Int x }
  | x = DEC; { Real x }
  | x = STR; { Str x }
  | x = BOOL; { if x then True else False }
  | LPAREN; ty = TYPE; x = NUM; RPAREN;
    {
      match ty with
      | Ty (Ty_bitv 32) -> Bitv (Bitvector.of_int32 (Int32.of_int x))
      | Ty (Ty_bitv 64) -> Bitv (Bitvector.of_int64 (Int64.of_int x))
      | _ -> Fmt.failwith "invalid bitv type"
    }
  | LPAREN; ty = TYPE; x = DEC; RPAREN;
    {
      match ty with
      | Ty (Ty_fp 32) -> Num (F32 (Int32.bits_of_float x))
      | Ty (Ty_fp 64) -> Num (F64 (Int64.bits_of_float x))
      | _ -> Fmt.failwith "invalid fp type"
    }

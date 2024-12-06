(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2024 formalsec *)
(* Written by the Smtml programmers *)

%{
open Value
open Ty
open Expr

let varmap = Hashtbl.create 512

let add_bind x t = Hashtbl.replace varmap x t
let get_bind x = Hashtbl.find varmap x

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
%token <Ty.t * Ty.unop> UNARY
%token <Ty.t * Ty.binop> BINARY
%token <Ty.t * Ty.triop> TERNARY
%token <Ty.t * Ty.relop> RELOP
%token <Ty.t * Ty.cvtop> CVTOP
%token <Ty.t * Ty.naryop> NARY
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
  | x = SYMBOL; { Expr.symbol (Symbol.make (get_bind x) x) }
  | c = spec_constant; { make (Val c) }
  | LPAREN; op = paren_op; RPAREN; { make op }

let paren_op :=
  | PTR; LPAREN; _ = TYPE; x = NUM; RPAREN; offset = s_expr;
    { Ptr { base = Int32.of_int x; offset } }
  | (ty, op) = UNARY; e = s_expr; <Unop>
  | (ty, op) = BINARY; e1 = s_expr; e2 = s_expr; <Binop>
  | (ty, op) = TERNARY; e1 = s_expr; e2 = s_expr; e3 = s_expr; <Triop>
  | (ty, op) = CVTOP; e = s_expr; <Cvtop>
  | (ty, op) = RELOP; e1 = s_expr; e2 = s_expr; <Relop>
  | (ty, op) = NARY; es = list(s_expr); <Naryop>
  | EXTRACT; ~ = s_expr; l = NUM; h = NUM; { Extract ( s_expr, h, l) }
  | CONCAT; e1 = s_expr; e2 = s_expr; <Concat>

let spec_constant :=
  | x = NUM; { Int x }
  | x = DEC; { Real x }
  | x = STR; { Str x }
  | x = BOOL; { if x then True else False }
  | LPAREN; ty = TYPE; x = NUM; RPAREN;
    {
      match ty with
      | Ty_bitv 32 -> Num (I32 (Int32.of_int x))
      | Ty_bitv 64 -> Num (I64 (Int64.of_int x))
      | _ -> Fmt.failwith "invalid bitv type"
    }
  | LPAREN; ty = TYPE; x = DEC; RPAREN;
    {
      match ty with
      | Ty_fp 32 -> Num (F32 (Int32.bits_of_float x))
      | Ty_fp 64 -> Num (F64 (Int64.bits_of_float x))
      | _ -> Fmt.failwith "invalid fp type"
    }

(***************************************************************************)
(* This file is part of the third-party OCaml library `smtml`.             *)
(* Copyright (C) 2023-2024 formalsec                                       *)
(*                                                                         *)
(* This program is free software: you can redistribute it and/or modify    *)
(* it under the terms of the GNU General Public License as published by    *)
(* the Free Software Foundation, either version 3 of the License, or       *)
(* (at your option) any later version.                                     *)
(*                                                                         *)
(* This program is distributed in the hope that it will be useful,         *)
(* but WITHOUT ANY WARRANTY; without even the implied warranty of          *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the           *)
(* GNU General Public License for more details.                            *)
(*                                                                         *)
(* You should have received a copy of the GNU General Public License       *)
(* along with this program.  If not, see <https://www.gnu.org/licenses/>.  *)
(***************************************************************************)

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
%token <Ty.t * [ `Unary ] Ty.op> UNARY
%token <Ty.t * [ `Binary ] Ty.op> BINARY
%token <Ty.t * [ `Ternary ] Ty.op> TERNARY
%token <Ty.t * [ `Nary ] Ty.op> NARY
%token <Ty.t> TYPE
%token <Ty.logic> LOGIC

%start <Ast.t list> script
%%

let script := stmts = list(stmt); EOF; { stmts }

let stmt :=
  | LPAREN; LET_CONST; x = SYMBOL; t = TYPE; RPAREN;
    {
      add_bind x t;
      Ast.Let_const (Symbol.make t x)
    }
  | LPAREN; ASSERT; ~ = s_expr; RPAREN; <Ast.Assert>
  | LPAREN; CHECK_SAT; RPAREN; { Ast.Check_sat}
  | LPAREN; PUSH; RPAREN; { Ast.Push }
  | LPAREN; POP; n = NUM; RPAREN; { Ast.Pop n }
  | LPAREN; GET_MODEL; RPAREN; { Ast.Get_model }
  | LPAREN; SET_LOGIC; ~ = LOGIC; RPAREN; <Ast.Set_logic>

let s_expr :=
  | x = SYMBOL; { mk_symbol @@ Symbol.make (get_bind x) x }
  | c = spec_constant; { make (Val c) }
  | LPAREN; op = paren_op; RPAREN; { make op }

let paren_op :=
  | PTR; LPAREN; _ = TYPE; x = NUM; RPAREN; offset = s_expr;
    { Ptr { base = Int32.of_int x; offset } }
  | (ty, op) = UNARY; e = s_expr;
    { Op { ty; op; args = U e } }
  | (ty, op) = BINARY; e1 = s_expr; e2 = s_expr;
    { Op { ty; op; args = B (e1, e2) } }
  | (ty, op) = TERNARY; e1 = s_expr; e2 = s_expr; e3 = s_expr;
    { Op { ty; op; args = T (e1, e2, e3) } }
  | (ty, op) = NARY; es = list(s_expr);
    { Op { ty; op; args = N es } }
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
      | _ -> failwith "invalid bitv type"
    }
  | LPAREN; ty = TYPE; x = DEC; RPAREN;
    {
      match ty with
      | Ty_fp 32 -> Num (F32 (Int32.bits_of_float x))
      | Ty_fp 64 -> Num (F64 (Int64.bits_of_float x))
      | _ -> failwith "invalid fp type"
    }

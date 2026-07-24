(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2024 formalsec *)
(* Written by the Smtml programmers *)

%{
module Env = Map.Make (String)
open Value
open Ty
open Expr
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
%token <string> HEX
%token <bool> BOOL
%token <string> STR
%token <string> SYMBOL
%token <Ty.t * Ty.Unop.t> UNARY
%token <Ty.t * Ty.Unop.t> FAKE_BINARY
%token <Ty.t * Ty.Binop.t> BINARY
%token <Ty.t * Ty.Triop.t> TERNARY
%token <Ty.t * Ty.Relop.t> RELOP
%token <Ty.t * Ty.Cvtop.t> CVTOP
%token <Ty.t * Ty.Naryop.t> NARY
%token <Ty.t> TYPE
%token <Logic.t> LOGIC

%start <Ast.t list> script
%start <Expr.t> expression
%%

let script := stmts = list(stmt); EOF; {
  let _, stmts =
    List.fold_left
      (fun (env, acc) f ->
        let stmt, env = f env in
        (env, stmt :: acc) )
      (Env.empty, []) stmts
  in
  List.rev stmts
}

let expression := e = sexpr_with_decls; EOF; { e Env.empty }

let sexpr_with_decls :=
  | decl = declaration; e = sexpr_with_decls; { fun env -> e (decl env) }
  | e = sexpr; { fun env -> e env }

let declaration :=
  LPAREN; LET_CONST; x = SYMBOL; t = TYPE; RPAREN; { fun env -> Env.add x t env }

let stmt :=
  | LPAREN; LET_CONST; x = SYMBOL; t = TYPE; RPAREN;
    {
      fun env ->
        ( Ast.Declare_const { id = Symbol.make t x; sort = Symbol.make t x }
        , Env.add x t env )
    }
  | LPAREN; ASSERT; e = sexpr; RPAREN;
    { fun env -> (Ast.Assert (e env), env) }
  | LPAREN; CHECK_SAT; RPAREN; { fun env -> (Ast.Check_sat [], env) }
  | LPAREN; PUSH; RPAREN; { fun env -> (Ast.Push 1, env) }
  | LPAREN; POP; n = NUM; RPAREN; { fun env -> (Ast.Pop n, env) }
  | LPAREN; GET_MODEL; RPAREN; { fun env -> (Ast.Get_model, env) }
  | LPAREN; SET_LOGIC; l = LOGIC; RPAREN; { fun env -> (Ast.Set_logic l, env) }

let sexpr :=
  | x = SYMBOL;
    {
      fun env ->
        match Env.find_opt x env with
        | None -> Expr.symbol (Symbol.make Ty_none x)
        | Some v -> Expr.symbol (Symbol.make v x)
    }
  | c = spec_constant; { fun _env -> value c }
  | LPAREN; op = paren_op; RPAREN; { op }

let paren_op :=
  | PTR; LPAREN; _ = TYPE; x = NUM; RPAREN; offset = sexpr;
    { fun env -> Expr.ptr (Int32.of_int x) (offset env) }
  | (ty, op) = UNARY; e = sexpr; { fun env -> Expr.unop ty op (e env) }
  | (ty, op) = FAKE_BINARY; n = NUM; e = sexpr;
    { fun env ->
        let op =
          match op with
          | Rotr 0 -> Ty.Unop.Rotr n
          | Rotl 0 -> Rotl n
          | _ ->
            Fmt.failwith "Malformed fake binary paren_op: %a %d"
              Ty.Unop.pp op n
        in
        Expr.unop ty op (e env)
    }
  | (ty, op) = BINARY; e1 = sexpr; e2 = sexpr;
    { fun env -> Expr.binop ty op (e1 env) (e2 env) }
  | (ty, op) = TERNARY; e1 = sexpr; e2 = sexpr; e3 = sexpr;
    { fun env -> Expr.triop ty op (e1 env) (e2 env) (e3 env) }
  | (ty, op) = CVTOP; e = sexpr; { fun env -> Expr.cvtop ty op (e env) }
  | (ty, op) = RELOP; e1 = sexpr; e2 = sexpr;
    { fun env -> Expr.relop ty op (e1 env) (e2 env) }
  | (ty, op) = NARY; es = list(sexpr);
    { fun env -> Expr.naryop ty op (List.map (fun e -> e env) es) }
  | EXTRACT; e = sexpr; l = NUM; h = NUM;
    { fun env -> Expr.extract (e env) ~high:h ~low:l }
  | CONCAT; e1 = sexpr; e2 = sexpr;
    { fun env -> Expr.concat (e1 env) (e2 env) }

let spec_constant :=
  | x = NUM; { Int x }
  | x = DEC; { Real x }
  | x = STR; { Str x }
  | x = BOOL; { if x then True else False }
  | LPAREN; ty = TYPE; x = NUM; RPAREN;
    {
      match ty with
      | Ty_bitv 32 -> Bitv (Bitvector.of_int32 (Int32.of_int x))
      | Ty_bitv 64 -> Bitv (Bitvector.of_int64 (Int64.of_int x))
      | _ -> Fmt.failwith "invalid bitv type"
    }
  | LPAREN; ty = TYPE; x = DEC; RPAREN;
    {
      match ty with
      | Ty_fp 32 -> Num (F32 (Int32.bits_of_float x))
      | Ty_fp 64 -> Num (F64 (Int64.bits_of_float x))
      | _ -> Fmt.failwith "invalid fp type"
    }
  | LPAREN; ty = TYPE; x = HEX; RPAREN;
    {
      match ty with
      | Ty_bitv 32 -> Bitv (Bitvector.of_int32 (Int32.of_string x))
      | Ty_bitv 64 -> Bitv (Bitvector.of_int64 (Int64.of_string x))
      | Ty_fp 32 -> Num (F32 (Int32.of_string x))
      | Ty_fp 64 -> Num (F64 (Int64.of_string x))
      | _ -> Fmt.failwith "invalid type"
    }

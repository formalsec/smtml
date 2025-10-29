(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2024 formalsec *)
(* Written by the Smtml programmers *)

type t =
  | Assert of Expr.t
  | Check_sat of Expr.t list
  | Declare_const of
      { id : Symbol.t
      ; sort : Symbol.t
      }
  | Declare_fun of
      { id : Symbol.t
      ; args : Symbol.t list
      ; sort : Symbol.t
      }
  | Echo of string
  | Exit
  | Get_assertions
  | Get_assignment
  | Get_info of string
  | Get_option of string
  | Get_model
  | Get_value of Expr.t list
  | Pop of int
  | Push of int
  | Reset
  | Reset_assertions
  | Set_info of Expr.t
  | Set_logic of Logic.t
  | Set_option of Expr.t

let pp fmt (instr : t) =
  match instr with
  | Assert e -> Fmt.pf fmt "@[<hov 1>(assert@ %a@])" Expr.Smtlib.pp e
  | Check_sat [] -> Fmt.string fmt "(check-sat)"
  | Check_sat assumptuions ->
    Fmt.pf fmt "(check-sat-assuming@ (%a))"
      (Fmt.list ~sep:Fmt.sp Expr.Smtlib.pp)
      assumptuions
  | Declare_const { id; sort } ->
    Fmt.pf fmt "(declare-const %a %a)" Symbol.Smtlib.pp id Symbol.Smtlib.pp sort
  | Declare_fun { id; args; sort } ->
    Fmt.pf fmt "(declare-fun %a (%a) %a)" Symbol.Smtlib.pp id
      (Fmt.list ~sep:Fmt.sp Symbol.Smtlib.pp)
      args Symbol.Smtlib.pp sort
  | Echo line -> Fmt.pf fmt "(echo %S)" line
  | Exit -> Fmt.string fmt "(exit)"
  | Get_assertions -> Fmt.string fmt "(get-assertions)"
  | Get_assignment -> Fmt.string fmt "(get-assignment)"
  | Get_info info -> Fmt.pf fmt "(get-info %a)" Fmt.string info
  | Get_option opt -> Fmt.pf fmt "(get-option %a)" Fmt.string opt
  | Get_model -> Fmt.string fmt "(get-model)"
  | Get_value htes ->
    Fmt.pf fmt "(get-value %a)"
      (Fmt.parens (Fmt.list ~sep:Fmt.sp Expr.Smtlib.pp))
      htes
  | Pop n -> Fmt.pf fmt "(pop %d)" n
  | Push n -> Fmt.pf fmt "(push %d)" n
  | Reset -> Fmt.string fmt "(reset)"
  | Reset_assertions -> Fmt.string fmt "(reset-assertions)"
  | Set_info info -> Fmt.pf fmt "(set-info %a)" Expr.Smtlib.pp info
  | Set_logic logic -> Fmt.pf fmt "(set-logic %a)" Logic.pp logic
  | Set_option opt -> Fmt.pf fmt "(set-option %a)" Expr.Smtlib.pp opt

let to_string (instr : t) : string = Fmt.str "%a" pp instr

module Script = struct
  type nonrec t = t list

  let const (id : Symbol.t) =
    let sort = Symbol.make3 id.ty id.name Symbol.sort in
    Declare_const { id; sort }

  let assert_ e = Assert e

  let of_exprs (exprs : Expr.t list) : t =
    let syms = Expr.get_symbols exprs in
    List.map const syms @ List.map assert_ exprs @ [ Check_sat [] ]

  let pp = Fmt.list pp
end

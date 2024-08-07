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

type t =
  | Assert of Expr.t
  | Check_sat
  | Push
  | Pop of int
  | Let_const of Symbol.t
  | Get_model
  | Set_logic of Ty.logic

type script = t list

let pp fmt (instr : t) =
  match instr with
  | Assert e -> Fmt.pf fmt "(assert @[<h 2>%a@])" Expr.pp e
  | Check_sat -> Fmt.string fmt "(check-sat)"
  | Push -> Fmt.string fmt "(push)"
  | Pop n -> Fmt.pf fmt "(pop %d)" n
  | Let_const s ->
    let ty = Symbol.type_of s in
    Fmt.pf fmt "(let-const %a %a)" Symbol.pp s Ty.pp ty
  | Get_model -> Fmt.string fmt "(get-model)"
  | Set_logic logic -> Fmt.pf fmt "(set-logic %a)" Ty.pp_logic logic

let to_string (instr : t) : string = Fmt.str "%a" pp instr

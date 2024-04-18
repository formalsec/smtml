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

let pp fmt (instr : t) =
  match instr with
  | Assert e -> Format.fprintf fmt "(assert @[<h 2>%a@])" Expr.pp e
  | Check_sat -> Format.pp_print_string fmt "(check-sat)"
  | Push -> Format.pp_print_string fmt "(push)"
  | Pop n -> Format.fprintf fmt "(pop %d)" n
  | Let_const s ->
    let ty = Symbol.type_of s in
    Format.fprintf fmt "(let-const %a %a)" Symbol.pp s Ty.pp ty
  | Get_model -> Format.pp_print_string fmt "(get-model)"
  | Set_logic logic -> Format.fprintf fmt "(set-logic %a)" Ty.pp_logic logic

let to_string (instr : t) : string = Format.asprintf "%a" pp instr

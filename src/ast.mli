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

type script = t list

val pp : t Fmt.t

val to_string : t -> string

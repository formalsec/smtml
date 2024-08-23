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

type 'a state =
  { stmts : Ast.script
  ; smap : (string, Ty.t) Hashtbl.t
  ; solver : 'a
  }

module type S = sig
  type solver

  type exec_state

  val start : ?state:exec_state -> Ast.script -> exec_state
end

module type Intf = sig
  module Make (Solver : Solver_intf.S) :
    S with type solver = Solver.t and type exec_state = Solver.t state
end

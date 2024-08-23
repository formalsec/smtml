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

include Interpret_intf

module Make (Solver : Solver_intf.S) = struct
  open Ast

  type solver = Solver.t

  type exec_state = solver state

  let init_state stmts =
    let params = Params.(default () $ (Model, true)) in
    let solver = Solver.create ~params () in
    Solver.push solver;
    { stmts; smap = Hashtbl.create 16; solver }

  let eval stmt (state : exec_state) : exec_state =
    let { solver; _ } = state in
    match stmt with
    | Assert e ->
      Solver.add solver [ e ];
      state
    | Check_sat ->
      ( match Solver.check solver [] with
      | `Sat -> Fmt.pr "sat@."
      | `Unsat -> Fmt.pr "unsat@."
      | `Unknown -> Fmt.pr "unknown@." );
      state
    | Push ->
      Solver.push solver;
      state
    | Pop n ->
      Solver.pop solver n;
      state
    | Let_const _x -> state
    | Get_model ->
      assert (
        (function `Sat -> true | `Unsat | `Unknown -> false)
          (Solver.check solver []) );
      let model = Solver.model solver in
      Fmt.pr "%a@." (Fmt.option (Model.pp ~no_values:false)) model;
      state
    | Set_logic logic ->
      let solver = Solver.create ~logic () in
      Solver.push solver;
      { state with solver }

  let rec loop (state : exec_state) : exec_state =
    match state.stmts with
    | [] -> state
    | stmt :: stmts -> loop (eval stmt { state with stmts })

  let start ?state (stmts : Ast.script) : exec_state =
    let st =
      match state with
      | None -> init_state stmts
      | Some st ->
        Solver.pop st.solver 1;
        Solver.push st.solver;
        { st with stmts }
    in
    loop st
end

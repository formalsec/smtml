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

  let init_state () =
    let params = Params.(default () $ (Model, true)) in
    let solver = Solver.create ~params () in
    { solver; stack = Stack.create (); top = Expr.Set.empty }

  let exec_cmd (st : exec_state) cmd : unit =
    match cmd with
    | Assert e ->
      Log.debug (fun k -> k "assert: %a" Expr.pp e);
      st.top <- Expr.Set.add e st.top
    | Check_sat assumptions -> (
      Log.debug (fun k -> k "check-sat: %a" Expr.pp_list assumptions);
      let assertions = Expr.Set.(union (of_list assumptions) st.top) in
      match Solver.check_set st.solver assertions with
      | `Sat -> Fmt.pr "sat@."
      | `Unsat -> Fmt.pr "unsat@."
      | `Unknown -> Fmt.pr "unknown@." )
    | Declare_const _x -> ()
    | Echo x -> Fmt.pr "%a" Fmt.string x
    | Get_model ->
      assert (
        (function `Sat -> true | `Unsat | `Unknown -> false)
          (Solver.check_set st.solver st.top) );
      let model = Solver.model st.solver in
      Fmt.pr "%a@." (Fmt.option (Model.pp ~no_values:false)) model
    | Push n ->
      assert (n >= 0);
      let rec loop n =
        if n <= 0 then ()
        else (
          Solver.push st.solver;
          Stack.push st.top st.stack;
          loop (n - 1) )
      in
      loop n
    | Pop n ->
      assert (n <= Stack.length st.stack);
      let rec loop n =
        if n <= 0 then ()
        else (
          Solver.pop st.solver 1;
          st.top <- Stack.pop st.stack;
          loop (n - 1) )
      in
      loop n
    | Set_logic _logic ->
      (* FIXME: Ignoring logic for now *)
      (* let solver = Solver.create ~logic () in *)
      (* Solver.push solver; *)
      (* { state with solver } *)
      ()
    | Set_info attr ->
      Log.debug (fun k -> k "Unsupported: (set-info %a)" Expr.pp attr)
    | Get_assertions | Get_assignment | Reset | Reset_assertions | Get_info _
    | Get_option _ | Get_value _ | Set_option _ ->
      Log.debug (fun k -> k "Unsupported: %a" Ast.pp cmd)
    | Exit -> assert false

  let rec loop (state : exec_state) script : exec_state =
    match script with
    | [] | Exit :: _ -> state
    | cmd :: cmds ->
      exec_cmd state cmd;
      loop state cmds

  let start ?state (script : Ast.script) : exec_state =
    Log.debug (fun k -> k "Starting interpreter...");
    let st =
      match state with
      | None -> init_state ()
      | Some st ->
        Stack.clear st.stack;
        { st with top = Expr.Set.empty }
    in
    loop st script
end

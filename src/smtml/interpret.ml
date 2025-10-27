(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2024 formalsec *)
(* Written by the Smtml programmers *)

include Interpret_intf

module Make (Solver : Solver_intf.S) = struct
  open Ast

  type solver = Solver.t

  type exec_state = solver state

  let init_state stmts expected_status ~timeout =
    let params = Params.(default () $ (Model, true) $ (Timeout, timeout)) in
    let solver = Solver.create ~params () in
    Solver.push solver;
    { stmts; smap = Hashtbl.create 16; solver; expected_status }

  let eval stmt (state : exec_state) ~no_strict_status : exec_state =
    let { solver; _ } = state in
    match stmt with
    | Assert e ->
      Log.debug (fun k -> k "assert: %a" Expr.pp e);
      Solver.add solver [ e ];
      state
    | Check_sat assumptions ->
      Log.debug (fun k -> k "check-sat: %a" Expr.pp_list assumptions);
      let actual = Solver.check solver assumptions in
      ( match actual with
      | `Sat -> Fmt.pr "sat@."
      | `Unsat -> Fmt.pr "unsat@."
      | `Unknown -> Fmt.pr "unknown@." );
      ( match (state.expected_status, actual) with
      | Some `Sat, `Unsat ->
        if no_strict_status then
          Log.warn (fun k ->
            k "Expected status: sat, but solver returned unsat" )
        else Fmt.failwith "Expected status: sat, but solver returned unsat"
      | Some `Unsat, `Sat ->
        if no_strict_status then
          Log.err (fun k ->
            k "Expected status: unsat, but solver returned sat" )
        else Fmt.failwith "Expected status: unsat, but solver returned sat"
      | _ -> () (* Unknown or matching cases are fine *) );
      state
    | Declare_const _x -> state
    | Declare_fun _x -> state
    | Echo x ->
      Fmt.pr "%a" Fmt.string x;
      state
    | Exit -> { state with stmts = [] }
    | Get_model ->
      assert (
        (function `Sat -> true | `Unsat | `Unknown -> false)
          (Solver.check solver []) );
      let model = Solver.model solver in
      Fmt.pr "%a@." (Fmt.option (Model.pp ~no_values:false)) model;
      state
    | Push _n ->
      Solver.push solver;
      state
    | Pop n ->
      Solver.pop solver n;
      state
    | Set_logic _logic ->
      state
      (* FIXME: Ignoring logic for now *)
      (* let solver = Solver.create ~logic () in *)
      (* Solver.push solver; *)
      (* { state with solver } *)
    | Set_info attr ->
      Log.debug (fun k -> k "Unsupported: (set-info %a)" Expr.pp attr);
      state
    | Get_assertions | Get_assignment | Reset | Reset_assertions | Get_info _
    | Get_option _ | Get_value _ | Set_option _ ->
      Log.debug (fun k -> k "Unsupported: %a" Ast.pp stmt);
      state

  let rec loop (state : exec_state) ~no_strict_status : exec_state =
    match state.stmts with
    | [] -> state
    | stmt :: stmts ->
      loop (eval stmt { state with stmts } ~no_strict_status) ~no_strict_status

  let parse_status (t : Expr.t) : [ `Sat | `Unsat | `Unknown ] option =
    match Expr.view t with
    | App ({ name = Simple ":status"; _ }, [ st ]) -> (
      match Expr.view st with
      | Symbol { name = Simple "sat"; _ } -> Some `Sat
      | Symbol { name = Simple "unsat"; _ } -> Some `Unsat
      | Symbol { name = Simple "unknown"; _ } -> Some `Unknown
      | _ ->
        Log.debug (fun k -> k "Unrecognised status value: %a" Expr.pp st);
        None )
    | _ -> None

  let find_expected_status (script : Ast.Script.t) :
    [ `Sat | `Unsat | `Unknown ] option =
    List.find_map
      (fun cmd ->
        match cmd with Ast.Set_info term -> parse_status term | _ -> None )
      script

  let start ?state (stmts : Ast.Script.t) ~no_strict_status ~timeout :
    exec_state =
    Log.debug (fun k -> k "Starting interpreter...");
    let expected_status = find_expected_status stmts in
    let st =
      match state with
      | None -> init_state stmts expected_status ~timeout
      | Some st ->
        Solver.pop st.solver 1;
        Solver.push st.solver;
        { st with stmts; expected_status }
    in
    loop st ~no_strict_status
end

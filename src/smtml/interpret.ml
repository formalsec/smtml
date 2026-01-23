(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2024 formalsec *)
(* Written by the Smtml programmers *)

include Interpret_intf

module Status = struct
  let pp fmt = function
    | `Sat -> Fmt.pf fmt "sat"
    | `Unsat -> Fmt.pf fmt "unsat"
    | `Unknown -> Fmt.pf fmt "unknown"

  let is_valid expected real =
    match (expected, real) with
    | `Unknown, _ -> true
    | `Sat, `Sat | `Unsat, `Unsat -> true
    | `Sat, `Unsat | `Unsat, `Sat | (`Sat | `Unsat), `Unknown -> false

  let of_expr (t : Expr.t) : [ `Sat | `Unsat | `Unknown ] option =
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
end

module Make (Solver : Solver_intf.S) = struct
  open Ast

  type solver = Solver.t

  type exec_state = solver state

  let init_state stmts expected_status =
    let params = Params.(default () $ (Model, true)) in
    let solver = Solver.create ~params () in
    Solver.push solver;
    { stmts; smap = Hashtbl.create 16; solver; expected_status }

  let eval stmt (state : exec_state) ~no_strict_status ~quiet : exec_state =
    let { solver; expected_status; _ } = state in
    match stmt with
    | Assert e ->
      Log.debug (fun k -> k "assert: %a" Expr.pp e);
      Solver.add solver [ e ];
      state
    | Check_sat assumptions -> begin
      Log.debug (fun k -> k "check-sat: %a" Expr.pp_list assumptions);
      let status = Solver.check solver assumptions in
      if not quiet then Fmt.pr "%a@." Status.pp status;
      begin match expected_status with
      | None -> ()
      | Some expected_status ->
        if not (Status.is_valid expected_status status) then
          if no_strict_status then
            Log.err (fun k ->
              k "Expected status: %a, but solver returned %a" Status.pp
                expected_status Status.pp status )
          else
            Fmt.failwith "Expected status: %a, but solver returned %a" Status.pp
              expected_status Status.pp status
      end;
      state
    end
    | Declare_const _x -> state
    | Declare_fun _x -> state
    | Echo x ->
      if not quiet then Fmt.pr "%a" Fmt.string x;
      state
    | Exit -> { state with stmts = [] }
    | Get_model ->
      assert (
        match Solver.check solver [] with
        | `Sat -> true
        | `Unsat | `Unknown -> false );
      let model = Solver.model solver in
      if not quiet then
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

  let rec loop (state : exec_state) ~no_strict_status ~quiet : exec_state =
    match state.stmts with
    | [] -> state
    | stmt :: stmts ->
      loop
        (eval stmt { state with stmts } ~no_strict_status ~quiet)
        ~no_strict_status ~quiet

  let find_expected_status (script : Ast.Script.t) :
    [ `Sat | `Unsat | `Unknown ] option =
    List.find_map
      (fun cmd ->
        match cmd with Ast.Set_info expr -> Status.of_expr expr | _ -> None )
      script

  let start ?state ?(quiet = false) ~no_strict_status (stmts : Ast.Script.t) :
    exec_state =
    Log.debug (fun k -> k "Starting interpreter...");
    let expected_status = find_expected_status stmts in
    let st =
      match state with
      | None -> init_state stmts expected_status
      | Some st ->
        Solver.pop st.solver 1;
        Solver.push st.solver;
        { st with stmts; expected_status }
    in
    loop st ~no_strict_status ~quiet
end

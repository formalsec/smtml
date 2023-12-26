open Syntax.Result

module Make (Solver : Solver_intf.S) = struct
  open Smtlib

  type exec_state =
    { cmds : Smtlib.script
    ; ty_env : (string, Ty.t) Hashtbl.t
    ; pc : Expr.t list
    ; solver : Solver.t
    }

  let init_state cmds =
    let params = Params.(default () & (Model, false)) in
    { cmds
    ; ty_env = Hashtbl.create 0
    ; solver = Solver.create ~params ()
    ; pc = []
    }

  let exec_cmd cmd (state : exec_state) : (exec_state, string) Result.t =
    let { solver; pc; ty_env; _ } = state in
    let st pc = Ok { state with pc } in
    match cmd with
    | Assert _t ->
      st pc
      (* Solver.add solver [ e ]; *)
      (* st (e :: pc) *)
    | Check_sat ->
      if Solver.check solver [] then Format.printf "sat\n"
      else Format.printf "unsat\n";
      st pc
    | Check_sat_assuming -> assert false
    | Declare_const (sym, sort) ->
      let* ty = Expr.Smtlib.to_type sort in
      Hashtbl.add ty_env sym ty;
      st pc
    | Echo msg ->
      Format.printf "%s@\n" msg;
      st pc
    | Exit -> Ok { state with cmds = [] }
    | Get_model ->
      assert (Solver.check solver []);
      let model = Solver.model solver in
      Format.printf "%a" Model.pp (Option.get model);
      st pc
    | _ -> assert false

  let rec loop (state : exec_state) : (exec_state, string) Result.t =
    match state.cmds with
    | [] -> Ok state
    | cmd :: cmds ->
      let* state = exec_cmd cmd { state with cmds } in
      loop state

  let main ?state (cmds : Smtlib.script) : (exec_state, string) Result.t =
    let st =
      match state with
      | None -> init_state cmds
      | Some st ->
        Solver.reset st.solver;
        { st with cmds; pc = [] }
    in
    loop st
end

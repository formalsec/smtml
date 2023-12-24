module Make (Solver : Solver_intf.S) = struct
  open Smtlib

  type exec_state =
    { cmds : Smtlib.script
    ; smap : (string, Ty.t) Hashtbl.t
    ; pc : Expr.t list
    ; solver : Solver.t
    }

  let init_state cmds =
    let params = Params.(default () & (Model, false)) in
    { cmds
    ; smap = Hashtbl.create 0
    ; solver = Solver.create ~params ()
    ; pc = []
    }

  let eval cmd (state : exec_state) : exec_state =
    let { solver; pc; _ } = state in
    let st pc = { state with pc } in
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
    | Declare_const (_sym, _sort) -> st pc
    | Echo msg ->
      Format.printf "%s@\n" msg;
      st pc
    | Exit -> { state with cmds = [] }
    | Get_model ->
      assert (Solver.check solver []);
      let model = Solver.model solver in
      Format.printf "%a" Model.pp (Option.get model);
      st pc
    | _ -> assert false

  let rec loop (state : exec_state) : exec_state =
    match state.cmds with
    | [] -> state
    | cmd :: cmds -> loop (eval cmd { state with cmds })

  let start ?state (cmds : Smtlib.script) : exec_state =
    let st =
      match state with
      | None -> init_state cmds
      | Some st ->
        Solver.reset st.solver;
        { st with cmds; pc = [] }
    in
    loop st
end

include Interpret_intf

module Make (Solver : Solver_intf.S) = struct
  open Ast

  type solver = Solver.t

  type exec_state = solver state

  let init_state stmts =
    let params = Params.(default () $ (Model, false)) in
    let solver = Solver.create ~params () in
    Solver.push solver;
    { stmts; smap = Hashtbl.create 16; solver; pc = [] }

  let eval stmt (state : exec_state) : exec_state =
    let { solver; pc; _ } = state in
    let st pc = { state with pc } in
    match stmt with
    | Assert e ->
      Solver.add solver [ e ];
      st (e :: pc)
    | Check_sat ->
      ( match Solver.check solver [] with
      | `Sat -> Format.printf "sat@."
      | `Unsat -> Format.printf "unsat@."
      | `Unknown -> Format.printf "unknown@." );
      st pc
    | Push ->
      Solver.push solver;
      st pc
    | Pop n ->
      Solver.pop solver n;
      st pc
    | Let_const _x -> st pc
    | Get_model ->
      assert (`Sat = Solver.check solver []);
      let model = Solver.model solver in
      Format.printf "%a@."
        (Format.pp_print_option (Model.pp ~no_values:false))
        model;
      st pc
    | Set_logic logic ->
      let solver = Solver.create ~logic () in
      Solver.push solver;
      { state with solver }

  let rec loop (state : exec_state) : exec_state =
    match state.stmts with
    | [] -> state
    | stmt :: stmts -> loop (eval stmt { state with stmts })

  let start ?state (stmts : Ast.t list) : exec_state =
    let st =
      match state with
      | None -> init_state stmts
      | Some st ->
        Solver.pop st.solver 1;
        Solver.push st.solver;
        { st with stmts; pc = [] }
    in
    loop st
end

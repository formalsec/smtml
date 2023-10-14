module Make (Solver : Solver_intf.S) = struct
  open Ast

  type exec_state =
    { stmts : Ast.t list
    ; smap : (string, Types.expr_type) Hashtbl.t
    ; pc : Expression.t list
    ; solver : Solver.t
    }

  let init_state stmts =
    { stmts; smap = Hashtbl.create 0; solver = Solver.create (); pc = [] }

  let eval stmt (state : exec_state) : exec_state =
    let { smap; solver; pc; _ } = state in
    let st pc = { state with pc } in
    match stmt with
    | Declare x ->
      Hashtbl.add smap (Symbol.to_string x) (Symbol.type_of x);
      st pc
    | Assert e ->
      Solver.add solver [ e ];
      st (e :: pc)
    | CheckSat ->
      if Solver.check solver [] then Format.printf "sat\n"
      else Format.printf "unsat\n";
      st pc
    | GetModel ->
      assert (Solver.check solver []);
      let model = Solver.model solver in
      Format.printf "%s" (Model.to_string (Option.get model));
      st pc

  let rec loop (state : exec_state) : exec_state =
    match state.stmts with
    | [] -> state
    | stmt :: stmts -> loop (eval stmt { state with stmts })

  let start ?state (stmts : Ast.t list) : exec_state =
    let st =
      match state with
      | None -> init_state stmts
      | Some st ->
        Solver.reset st.solver;
        { st with stmts }
    in
    loop st
end

module Make (Solver : Solver_intf.S) = struct
  open Ast

  type exec_state =
    { stmts : Ast.t list
    ; smap : (string, Ty.t) Hashtbl.t
    ; pc : Expr.t list
    ; solver : Solver.t
    }

  let init_state stmts =
    let params = Params.(default () $ (Model, false)) in
    { stmts
    ; smap = Hashtbl.create 16
    ; solver = Solver.create ~params ()
    ; pc = []
    }

  let eval stmt (state : exec_state) : exec_state =
    let { solver; pc; _ } = state in
    let st pc = { state with pc } in
    match stmt with
    | Assert e ->
      Solver.add solver [ e ];
      st (e :: pc)
    | Check_sat ->
      ( match Solver.check solver [] with
      | true -> Format.printf "sat@\n"
      | false -> Format.printf "unsat@\n" );
      st pc
    | Push ->
      Solver.push solver;
      st pc
    | Pop n ->
      Solver.pop solver n;
      st pc
    | Let_const _x -> st pc
    | Get_model ->
      assert (Solver.check solver []);
      let model = Solver.model solver in
      Format.printf "%a@."
        (Format.pp_print_option (Model.pp ~no_values:false))
        model;
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
        { st with stmts; pc = [] }
    in
    loop st
end

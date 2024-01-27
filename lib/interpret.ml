module Make (Solver : Solver_intf.S) = struct
  open Ast
  module SMap = Map.Make (String)

  type exec_state =
    { stmts : Ast.t list
    ; ty_env : Ty.t SMap.t
    ; pc : Expr.t list
    ; solver : Solver.t
    }

  let init_state stmts =
    let params = Params.(default () $ (Model, false)) in
    { stmts; ty_env = SMap.empty; solver = Solver.create ~params (); pc = [] }

  let rewrite ty_env map (e : Expr.t) =
    let rec traverse (e : Expr.t) =
      let open Expr in
      match e.node.e with
      | Val _ | Ptr _ -> e
      | Unop (op, e') -> Unop (op, traverse e') @: e.node.ty
      | Binop (op, e1, e2) -> Binop (op, traverse e1, traverse e2) @: e.node.ty
      | Triop (op, e1, e2, e3) ->
        let e1 = traverse e1 in
        let e2 = traverse e2 in
        let e3 = traverse e3 in
        Triop (op, e1, e2, e3) @: e.node.ty
      | Relop (op, e1, e2) -> Relop (op, traverse e1, traverse e2) @: e.node.ty
      | Cvtop (op, e') -> Cvtop (op, traverse e') @: e.node.ty
      | Symbol s -> (
        let name = Symbol.name s in
        match SMap.find name map with
        | exception Not_found ->
          if not @@ SMap.mem name ty_env then
            Log.err "Undefined variable '%s'" name;
          e
        | expr -> expr )
      | Extract (e', h, l) -> Extract (traverse e', h, l) @: e.node.ty
      | Concat (e1, e2) -> Concat (traverse e1, traverse e2) @: e.node.ty
    in
    traverse e

  let eval_term ty_env t =
    let rec eval' map = function
      | E e -> rewrite ty_env map e
      | Let (binds, term) ->
        let map' =
          List.fold_left
            (fun acc (xi, ti) ->
              let ei = eval' map ti in
              SMap.add xi ei acc )
            map binds
        in
        eval' map' term
    in
    eval' SMap.empty t

  let eval stmt (state : exec_state) : exec_state =
    let { solver; pc; ty_env; _ } = state in
    let st pc = { state with pc } in
    match stmt with
    | Assert t ->
      let e = eval_term ty_env t in
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
    | Let_const sym ->
      { state with ty_env = SMap.add (Symbol.name sym) (Symbol.ty sym) ty_env }
    | Get_model ->
      assert (Solver.check solver []);
      let model = Solver.model solver in
      Format.printf "%a@." (Format.pp_print_option Model.pp) model;
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

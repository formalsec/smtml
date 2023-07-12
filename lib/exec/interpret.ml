module Make (Solver : Solver_intf.S) = struct
  open Core
  open Ast

  type config =
    { code : Ast.t list
    ; smap : (string, Types.expr_type) Hashtbl.t
    ; pc : Expression.t list
    ; solver : Solver.t
    }

  let eval (c : config) : config =
    let { code; smap; solver; pc } = c in
    let i = List.hd_exn code in
    let code', pc' =
      match i with
      | Declare x ->
        Hashtbl.add_exn smap ~key:(Symbol.to_string x) ~data:(Symbol.type_of x);
        (List.tl_exn code, pc)
      | Assert e ->
        Solver.add solver [ e ];
        (List.tl_exn code, e :: pc)
      | CheckSat ->
        if Solver.check solver [] then printf "sat\n" else printf "unsat\n";
        (List.tl_exn code, pc)
      | GetModel ->
        assert (Solver.check solver []);
        let model = Solver.model solver in
        printf "%s" (Model.to_string (Option.value_exn model));
        (List.tl_exn code, pc)
    in
    { c with code = code'; pc = pc' }

  let rec loop (c : config) : config =
    match c.code with [] -> c | _ -> loop (eval c)

  let start (prog : Ast.t list) : unit =
    let c =
      { code = prog
      ; smap = Hashtbl.create (module String)
      ; solver = Solver.create ()
      ; pc = []
      }
    in
    ignore (loop c)
end

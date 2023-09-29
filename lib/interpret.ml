module Make (Solver : Solver_intf.S) = struct
  open Ast

  type config =
    { code : Ast.t list
    ; smap : (string, Types.expr_type) Hashtbl.t
    ; pc : Expression.t list
    ; solver : Solver.t
    }

  let eval (c : config) : config =
    let { code; smap; solver; pc } = c in
<<<<<<< Updated upstream
    let i = List.hd code in
=======
    let i = List.hd_exn code in
    printf "%s" (Ast.to_string i);
<<<<<<< Updated upstream
>>>>>>> Stashed changes
=======
>>>>>>> Stashed changes
    let code', pc' =
      match i with
      | Declare x ->
        Hashtbl.add smap (Symbol.to_string x) (Symbol.type_of x);
        (List.tl code, pc)
      | Assert e ->
        Solver.add solver [ e ];
        (List.tl code, e :: pc)
      | CheckSat ->
        if Solver.check solver [] then Format.printf "sat\n"
        else Format.printf "unsat\n";
        (List.tl code, pc)
      | GetModel ->
        assert (Solver.check solver []);
        let model = Solver.model solver in
        Format.printf "%s" (Model.to_string (Option.get model));
        (List.tl code, pc)
    in
    { c with code = code'; pc = pc' }

  let rec loop (c : config) : config =
    match c.code with [] -> c | _ -> loop (eval c)

  let start (prog : Ast.t list) : unit =
    let c =
      { code = prog
      ; smap = Hashtbl.create 0
      ; solver = Solver.create ()
      ; pc = []
      }
    in
    ignore (loop c)
end

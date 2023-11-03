module Make (Solver : Solver_intf.S) = struct
  open Ast

  type mode =
  | NormalizeAndCache
  | DumbCache
  | NoCache

  let mode_of_int = function
  | 0 -> NormalizeAndCache
  | 1 -> DumbCache
  | _ -> NoCache


  type config =
    { code : Ast.t list
    ; smap : (string, Types.expr_type) Hashtbl.t
    ; pc : Expression.t list
    ; solver : Solver.t
    ; mode : mode
    }

  let check_sat_cache = Hashtbl.create 0

  (* Externalized CheckSat logic *)
  let handle_check_sat (c : config) =
    let use_cache, normalized_pc = match c.mode with
      | NormalizeAndCache -> true, Normalize.normalize c.pc
      | DumbCache -> true, c.pc
      | NoCache -> false, c.pc
    in
    let pc_string = String.concat ";" (List.map Expression.to_string normalized_pc) in 
    let compute_and_maybe_cache_result () =
      let is_sat = Solver.check c.solver [] in
      let result = if is_sat then "sat" else "unsat" in
      if use_cache then Hashtbl.add check_sat_cache pc_string result;
      result
    in
    let cache_hit, result = 
      if use_cache then 
        match Hashtbl.find_opt check_sat_cache pc_string with
        | Some result -> true, result
        | None -> false, compute_and_maybe_cache_result ()
      else
        false, compute_and_maybe_cache_result ()
    in
    if cache_hit then Format.printf "Cache Hit: ";
    Format.printf "%s\n" result;
    (List.tl c.code, c.pc)

  let eval (c : config) : config =
    let { code; smap; solver; pc; _ } = c in
    let i = List.hd code in
    let code', pc' =
      match i with
      | Declare x ->
        Hashtbl.add smap (Symbol.to_string x) (Symbol.type_of x);
        (List.tl code, pc)
      | Assert e ->
        Solver.add solver [ e ];
        (List.tl code, e :: pc)
      | GetModel ->
        assert (Solver.check solver []);
        let model = Solver.model solver in
        Format.printf "%s" (Model.to_string (Option.get model));
        (List.tl code, pc)
      | CheckSat -> handle_check_sat c 
    in
    { c with code = code'; pc = pc' }

  let rec loop (c : config) : config =
    match c.code with [] -> c | _ -> loop (eval c)

  let start (prog : Ast.t list) (mode : int) : unit =
    let mode = mode_of_int mode in  (* convert int to mode type *)
    let c =
      { code = prog
      ; smap = Hashtbl.create 0
      ; solver = Solver.create ()
      ; pc = []
      ; mode
      }
    in
    ignore (loop c)
    

end

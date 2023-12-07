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
    ; mutable last_result : string
    }

  let check_sat_cache = Hashtbl.create 0



  let handle_checksat (c : config) : string =

    (* Step 0: Normalize and check the cache first *)
    let es = Normalize.normalize c.pc in

    (* Convert the list of expressions to a string for caching *)
    let expr_string = String.concat ";" (List.map Expression.to_string es) in
    match Hashtbl.find_opt check_sat_cache expr_string with
      | Some result -> result
      | None -> 


    (* Step 1: Slice the program constraints into independent groups of expressions *)
    let ess = Slicing.slice es in

    (*
    (* Print out the sliced groups of expressions *)
    Format.printf "Sliced expressions:\n";
    List.iteri (fun i es ->
      Format.printf "Group %d:\n" (i + 1);
      List.iter (fun e -> 
        let expr_str = Expression.to_string e in
        Format.printf "  %s\n" expr_str
      ) es;
      Format.printf "\n";
    ) ess;*)
    
    
    (* Step 2: Check sat/unsat for each group, and cache the results *)
    let results = List.map (fun es ->
      
      (* Convert the list of expressions to a string for caching *)
      let expr_string = String.concat ";" (List.map Expression.to_string es) in
      
      (* Check the cache first *)
      match Hashtbl.find_opt check_sat_cache expr_string with
      | Some result -> 
        (*Format.printf "Cache Hit: %s\n" result;*)
        result
      | None -> 
        (* If not in the cache, use the solver *)
        Solver.reset c.solver;
        Solver.add c.solver es;
        let is_sat = Solver.check c.solver es in
        
        (* Cache the result *)
        let result = if is_sat then "sat" else "unsat" in
        Hashtbl.add check_sat_cache expr_string result;
        (*Format.printf "%s\n" result;*)
        result
    ) ess in
  

    (* Step 3: Return the final result *)
    if List.exists ((=) "unsat") results then "unsat" else "sat"


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
      | CheckSat -> 
        (*let start_time = Unix.gettimeofday () in*)
        c.last_result <- handle_checksat c;
        (*let end_time = Unix.gettimeofday () in*)
        Format.printf "%s\n" c.last_result;
        (*Format.printf "Time taken: %f seconds\n" (end_time -. start_time);*)
        (List.tl code, pc)
    in
    { c with code = code'; pc = pc'; last_result = c.last_result }

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
      ; last_result = ""
      }
    in
    ignore (loop c)
    

end

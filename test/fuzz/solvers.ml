module type Extended_solver_intf = sig
  include Smtml.Solver_intf.S

  val solver_type : Smtml.Solver_type.t
end

type 'a solver_module = (module Extended_solver_intf with type t = 'a)

type t = S : ('a solver_module * 'a) -> t

let fresh solver_ty =
  let module Mapping = (val Smtml.Solver_dispatcher.mappings_of_solver solver_ty)
  in
  let module Mapping = Mapping.Fresh.Make () in
  let module Batch = Smtml.Solver.Batch (Mapping) in
  let module Batch = struct
    include Batch

    let solver_type = solver_ty
  end in
  let solver_inst = Batch.create () in
  let solver = S ((module Batch), solver_inst) in
  solver

let init ~disable_z3 ~disable_altergo ~disable_bitwuzla ~disable_cvc5
  ~disable_colibri2 ~disable_smtzilla =
  (if disable_z3 then [] else [ fresh Smtml.Solver_type.Z3_solver ])
  @ (if disable_altergo then [] else [ fresh Smtml.Solver_type.Altergo_solver ])
  @ ( if disable_bitwuzla then []
      else [ fresh Smtml.Solver_type.Bitwuzla_solver ] )
  @ (if disable_cvc5 then [] else [ fresh Smtml.Solver_type.Cvc5_solver ])
  @ ( if disable_colibri2 then []
      else [ fresh Smtml.Solver_type.Colibri2_solver ] )
  @ if disable_smtzilla then [] else [ fresh Smtml.Solver_type.Smtzilla_solver ]

let pp_answer ppf = function
  | `Sat -> Fmt.pf ppf "SAT"
  | `Unsat -> Fmt.pf ppf "UNSAT"
  | `Unknown -> Fmt.pf ppf "UNKNOWN"

let pp_answer_mod ppf = function
  | `Model _ -> Fmt.pf ppf "SAT"
  | `Unsat -> Fmt.pf ppf "UNSAT"
  | `Unknown -> Fmt.pf ppf "UNKNOWN"

let get_model model s expr answer =
  let (S (module_solver, solver)) = s in
  let module Solver = (val module_solver) in
  let l = Smtml.Expr.Set.of_list expr in
  let f = Solver.get_sat_model solver l in
  if model && answer = `Sat then
    match f with
    | `Model model ->
      Logs.app (fun m -> m "%a" (Smtml.Model.pp ~no_values:false) model)
    | `Unknown | `Unsat -> ()

let solve expr model s =
  let (S (module_solver, solver)) = s in
  let module Solver = (val module_solver) in
  Logs.debug (fun m ->
    m "solving with %a..." Smtml.Solver_type.pp Solver.solver_type );
  let answer = Solver.check solver expr in
  Logs.debug (fun m -> m "answered %a" pp_answer answer);
  if model then get_model model s expr answer;
  Solver.reset solver;
  answer

(* let symbole_to_expr symbole =
  match Smtml.Symbol.type_of symbole with
  | Ty_int -> Smtml.Typed.Unsafe.unwrap (Smtml.Typed.Int.symbol symbole)
  | Ty_bool -> Smtml.Typed.Unsafe.unwrap (Smtml.Typed.Bool.symbol symbole)
  | Ty_bitv 32 -> Smtml.Typed.Unsafe.unwrap (Smtml.Typed.Bitv32.symbol symbole)
  | Ty_fp 32 -> Smtml.Typed.Unsafe.unwrap (Smtml.Typed.Float32.symbol symbole)
  | Ty_fp 64 -> Smtml.Typed.Unsafe.unwrap (Smtml.Typed.Float64.symbol symbole)
  | _ -> assert false

let value_to_expr value = 
  match value with
  | Smtml.Value.True -> Smtml.Typed.Bool.true_
  | Smtml.Value.False -> Smtml.Typed.Bool.false_
  | Smtml.Value.Int i -> Smtml.Typed.Int.v i 
  | _ -> assert false *)

let to_expr symbole valeur =
  match (Smtml.Symbol.type_of symbole, valeur) with
  | Ty_int, Smtml.Value.Int i ->
    let symbole = Smtml.Typed.Int.symbol symbole in
    let valeur = Smtml.Typed.Int.v i in
    Smtml.Typed.Int.eq symbole valeur
  | Ty_bool, Smtml.Value.True ->
    let symbole = Smtml.Typed.Bool.symbol symbole in
    let valeur = Smtml.Typed.Bool.true_ in
    Smtml.Typed.Bool.eq symbole valeur
  | Ty_bool, Smtml.Value.False ->
    let symbole = Smtml.Typed.Bool.symbol symbole in
    let valeur = Smtml.Typed.Bool.false_ in
    Smtml.Typed.Bool.eq symbole valeur
  | Ty_bitv 32, Smtml.Value.Bitv b ->
    let symbole = Smtml.Typed.Bitv32.symbol symbole in
    Smtml.Typed.Bitv32.eq symbole
      (Smtml.Typed.Bitv32.of_int32 (Smtml.Bitvector.to_int32 b))
  | Ty_fp 32, Smtml.Value.Bitv b ->
    let symbole = Smtml.Typed.Float32.symbol symbole in
    let valeur =
      Smtml.Typed.Float32.of_int32_bits (Smtml.Bitvector.to_int32 b)
    in
    Smtml.Typed.Float32.eq symbole valeur
  | Ty_fp 64, Smtml.Value.Bitv b ->
    let symbole = Smtml.Typed.Float64.symbol symbole in
    let b = Int64.to_float (Smtml.Bitvector.to_int64 b) in
    let valeur = Smtml.Typed.Float64.of_float b in
    Smtml.Typed.Float64.eq symbole valeur
  | _ -> assert false

let new_query (valeur : (Smtml.Symbol.t * Smtml.Value.t) list) =
  let valeur =
    List.fold_left
      (fun acc elt ->
        let symbole, valeur = elt in
        let expr = to_expr symbole valeur in
        Smtml.Typed.Bool.and_ expr acc )
      Smtml.Typed.Bool.true_ valeur
  in
  [ Smtml.Typed.Unsafe.unwrap valeur ]

let check_same_output (expr : Smtml.Typed.Bool.t) (solvers : t list)
  (get_model : bool) =
  Logs.app (fun m -> m "generated: %a@\n" Smtml.Typed.Bool.pp expr);
  let expr = [ Smtml.Typed.Unsafe.unwrap expr ] in
  let outputs = List.map (solve expr get_model) solvers in
  match outputs with
  | [] -> true
  | first_output :: outputs ->
    List.for_all (fun output -> output = first_output) outputs

let give_model s model1 expr =
  let (S (module_solver, solver)) = s in
  let module Solver = (val module_solver) in
  let l = Smtml.Expr.Set.of_list expr in
  let answer = Solver.get_sat_model solver l in
  match answer with
  | `Model model ->
    Logs.app (fun m -> m "%s\n" model1);
    let valeur = Smtml.Model.get_bindings model in
    let model2 =
      List.map
        (fun (symbole, valeur) ->
          (Smtml.Symbol.to_string symbole, Smtml.Value.to_string valeur) )
        valeur
    in
    let model2 =
      List.sort (fun (s1, _) (s2, _) -> String.compare s1 s2) model2
    in
    let model2 =
      List.fold_left (fun acc (s, v) -> acc ^ s ^ " " ^ v ^ " ") "" model2
    in
    Logs.app (fun m -> m "%s\n" model2);

    model1 = model2
  | `Unknown | `Unsat ->
    Logs.app (fun m -> m "answered %a\n" pp_answer_mod answer);
    false

let check_model (expr_bool : Smtml.Typed.Bool.t) (solvers : t list) =
  Logs.app (fun m -> m "generated: %a@\n" Smtml.Typed.Bool.pp expr_bool);
  let expr = [ Smtml.Typed.Unsafe.unwrap expr_bool ] in
  (* let s1, s2 = solvers in *)
  match solvers with
  | [] -> true
  | s1 :: tl -> (
    let (S (module_solver, solver)) = s1 in
    let module Solver = (val module_solver) in
    let l = Smtml.Expr.Set.of_list expr in
    let answer = Solver.get_sat_model solver l in
    match answer with
    | `Model model ->
      let valeur = Smtml.Model.get_bindings model in
      let expr = new_query valeur in
      let cmp_string =
        List.map
          (fun (symbole, valeur) ->
            (Smtml.Symbol.to_string symbole, Smtml.Value.to_string valeur) )
          valeur
      in
      let cmp_string =
        List.sort (fun (s1, _) (s2, _) -> String.compare s1 s2) cmp_string
      in
      let str =
        List.fold_left (fun acc (s, v) -> acc ^ s ^ " " ^ v ^ " ") "" cmp_string
      in
      List.for_all (fun s2 -> give_model s2 str expr) tl
    | `Unknown | `Unsat ->
      if check_same_output expr_bool solvers false then (
        Logs.app (fun m -> m "answered %a\n" pp_answer_mod answer);
        true )
      else (
        Logs.app (fun m -> m "ERROR %a\n" pp_answer_mod answer);
        false ) )

let check_same_output_and_sat expr solvers =
  Logs.app (fun m -> m "generated: %a@\n" Smtml.Typed.Bool.pp expr);
  let expr = [ Smtml.Typed.Unsafe.unwrap expr ] in
  let outputs = List.map (solve expr false) solvers in
  match outputs with
  | [] -> None
  | first_output :: outputs ->
    let bool_ = List.for_all (fun output -> output = first_output) outputs in
    if bool_ then Some first_output else None

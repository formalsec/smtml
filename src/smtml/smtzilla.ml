(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2024 formalsec *)
(* Written by Hichem Rami Ait El Hara *)

open Mappings_intf

let available_models : (string * Regression_model.t) list =
  let env_var = "MODEL_FILE_PATH" in
  match Bos.OS.Env.var env_var with
  | Some path ->
    let models = Regression_model.read_models_from_file path in
    models
  | None ->
    let models = Regression_model.read_models_from_file "./misc/model.json" in
    models

module Fresh = struct
  module Make () = struct
    type solver_instance =
      | SolverInst : (module S with type solver = 's) * 's -> solver_instance

    type solver =
      { solver_instances : (string, solver_instance) Hashtbl.t
      ; mutable rev_exprs : Expr.t list
      ; mutable last_solver : string option
      }

    type model =
      | Model :
          (module S with type solver = 's and type model = 'm) * 's * 'm
          -> model

    type optimize = unit

    type handle = unit

    module Solver = struct
      let make ?params:_ ?logic:_ () =
        { solver_instances = Hashtbl.create 16
        ; rev_exprs = []
        ; last_solver = None
        }

      let add s new_exprs =
        Hashtbl.iter
          (fun _ (SolverInst ((module S), instance)) ->
            S.Solver.add instance new_exprs )
          s.solver_instances;
        s.rev_exprs <- List.rev_append new_exprs s.rev_exprs

      let get_best_solver exprs : string =
        let feats = Feature_extraction.extract_feats exprs in
        let scores =
          List.map
            (fun (name, model) ->
              let score = Regression_model.predict feats model in
              (score, name) )
            available_models
        in
        let name = Regression_model.choose_best scores in
        Log.info (fun k -> k "Selected solver %s" name);
        name

      let get_solver_instance s name =
        match Hashtbl.find_opt s.solver_instances name with
        | Some s -> s
        | None ->
          let (module S) : (module Mappings.S_with_fresh) =
            match name with
            | "z3" -> (module Z3_mappings)
            | "bitwuzla" -> (module Bitwuzla_mappings)
            | _ -> Fmt.failwith "SMTZilla: Unknown solver %s" name
          in
          let instance = S.Solver.make () in
          S.Solver.add instance (List.rev s.rev_exprs);
          let solver_inst = SolverInst ((module S), instance) in
          Hashtbl.add s.solver_instances name solver_inst;
          solver_inst
      (* TODO: Need to move some declarations around to be able to use
        `Solver_type.t` instead of strings, mayba SMTZilla should not be
        one of the solver types? *)

      let check s ~assumptions =
        let best_solver_name = get_best_solver (s.rev_exprs @ assumptions) in
        s.last_solver <- Some best_solver_name;
        let (SolverInst ((module S), solver_inst)) =
          get_solver_instance s best_solver_name
        in
        S.Solver.check solver_inst ~assumptions

      let model s =
        match s.last_solver with
        | None -> None
        | Some name -> (
          match Hashtbl.find_opt s.solver_instances name with
          | None -> assert false
          | Some (SolverInst ((module S), s)) -> (
            match S.Solver.model s with
            | Some m -> Some (Model ((module S), s, m))
            | None -> None ) )

      let push _ = ()

      let pop _ _ = ()

      let reset s =
        Hashtbl.iter
          (fun _ (SolverInst ((module S), instance)) -> S.Solver.reset instance)
          s.solver_instances

      let clone s =
        { s with
          solver_instances =
            (let solver_instances = Hashtbl.create 16 in
             Hashtbl.iter
               (fun name (SolverInst ((module S), instance)) ->
                 Hashtbl.add solver_instances name
                   (SolverInst ((module S), S.Solver.clone instance)) )
               s.solver_instances;
             solver_instances )
        ; rev_exprs = s.rev_exprs
        }

      let interrupt s =
        Hashtbl.iter
          (fun _ (SolverInst ((module S), instance)) ->
            S.Solver.interrupt instance )
          s.solver_instances

      let add_simplifier s =
        Hashtbl.filter_map_inplace
          (fun _ (SolverInst ((module S), instance)) ->
            let instance = S.Solver.add_simplifier instance in
            Some (SolverInst ((module S), instance)) )
          s.solver_instances;
        s

      let get_statistics s =
        Hashtbl.fold
          (fun _ (SolverInst ((module S), instance)) acc ->
            Statistics.merge (S.Solver.get_statistics instance) acc )
          s.solver_instances Statistics.Map.empty
    end

    let value (Model ((module S), _, m)) expr = S.value m expr

    let values_of_model ?symbols (Model ((module S), _, m)) =
      S.values_of_model ?symbols m

    let set_debug _ = ()

    let die () = Fmt.failwith "Unsupported with SMTZilla"

    module Optimizer = struct
      let make _ = die ()

      let push _ = die ()

      let pop _ = die ()

      let add _ = die ()

      let check _ = die ()

      let model _ = die ()

      let maximize _ = die ()

      let minimize _ = die ()

      let interrupt _ = die ()

      let get_statistics _ = die ()
    end

    module Smtlib = struct
      let pp ?name:_ ?logic:_ ?status:_ _fmt _ = die ()
    end
  end
end

let is_available =
  match available_models with
  | [] -> false
  | [ _ ] ->
    (* No model should be trained on just one solver *)
    assert false
  | _ -> true

include Fresh.Make ()

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
      ; mutable exprs : Expr.t list
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
        { solver_instances = Hashtbl.create 2; exprs = []; last_solver = None }

      let add s exprs = s.exprs <- s.exprs @ exprs

      let get_best_solver exprs : string * (module Mappings.S_with_fresh) =
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
        match name with
        | "Z3" | "z3" -> (name, (module Z3_mappings))
        | "Bitwuzla" | "bitwuzla" -> (name, (module Bitwuzla_mappings))
        | _ -> Fmt.failwith "SMTZilla: Unknown solver %s" name
      (* TODO: Need to move some declarations around to be able to use
         `Solver_type.t` instead of strings, mayba SMTZilla should not be
         one of the solver types? *)

      let check s ~assumptions =
        let all_exprs = s.exprs @ assumptions in
        let best_solver_name, (module Best) = get_best_solver all_exprs in
        s.last_solver <- Some best_solver_name;
        let solver_inst = Best.Solver.make () in
        Hashtbl.add s.solver_instances best_solver_name
          (SolverInst ((module Best), solver_inst));
        let solver_inst = Best.Solver.make () in
        Best.Solver.add solver_inst s.exprs;
        Best.Solver.check solver_inst ~assumptions

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

      let reset s = s.exprs <- []

      let clone s = { s with exprs = s.exprs }

      let interrupt _ = ()

      let add_simplifier s = s

      let get_statistics _ = Statistics.Map.empty
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

(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2024 formalsec *)
(* Written by Hichem Rami Ait El Hara *)

open Yojson.Safe.Util
module FeatMap = Feature_extraction.FeatMap

type score = float

let score_of_int = float_of_int

let compare_score = Float.compare

let to_score = to_float

type tree =
  | Leaf of score
  | Node of
      { feature : string
      ; threshold : score
      ; left : tree
      ; right : tree
      }

type gb_model =
  { (* n_estimators : int; *)
    init_value : score
  ; trees : tree list
  }

type dt_model = tree

type t =
  | GBModel of gb_model
  | DTModel of dt_model

let rec tree_of_json json =
  match member "value" json with
  | `Float f -> Leaf f
  | `Int i -> Leaf (score_of_int i)
  | `Null ->
    Node
      { feature = member "feature" json |> to_string
      ; threshold = member "threshold" json |> to_score
      ; left = member "left" json |> tree_of_json
      ; right = member "right" json |> tree_of_json
      }
  | _ -> Fmt.failwith "Invalid tree structure in JSON"

let model_of_json json =
  let is_gb = member "gradient_boost" json |> to_bool in
  let model_data = member "model" json in
  if is_gb then
    GBModel
      { (* n_estimators = member "n_estimators" model_data |> to_int;  *)
        init_value = member "init_value" model_data |> to_score
      ; trees = member "trees" model_data |> to_list |> List.map tree_of_json
      }
  else DTModel (tree_of_json model_data)

let read_models_from_file filename =
  let json = Yojson.Safe.from_file filename in
  json |> to_assoc
  |> List.map (fun (solver_name, solver_json) ->
    (solver_name, model_of_json solver_json) )

let rec eval_tree (feats : int FeatMap.t) = function
  | Leaf v -> v
  | Node { feature; threshold; left; right } ->
    let value = score_of_int (FeatMap.find_def0 feature feats) in
    if compare_score value threshold <= 0 then eval_tree feats left
    else eval_tree feats right

let choose_best scores =
  match List.sort (fun (a, _) (b, _) -> compare_score a b) scores with
  | [] | [ _ ] -> assert false
  | (_, hd) :: _ -> hd

let predict (feats : int FeatMap.t) = function
  | DTModel t -> eval_tree feats t
  | GBModel gb ->
    let sum =
      List.fold_left (fun acc t -> acc +. eval_tree feats t) 0.0 gb.trees
    in
    gb.init_value +. sum

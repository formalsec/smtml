(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2024 formalsec *)
(* Written by Hichem Rami Ait El Hara *)

open Smtzilla_utils.Regression_model

let default_regression_model_path = "../../misc/model.json"

let () =
  let models = read_models_from_file default_regression_model_path in
  Fmt.pr "open Smtzilla_utils.Regression_model\n\n";
  Fmt.pr "let default_models = [%a]\n"
    (Fmt.list
       ~sep:(fun fmt () -> Fmt.pf fmt "; ")
       (fun fmt (name, model) -> Fmt.pf fmt "(%S, %a)" name pp model) )
    models

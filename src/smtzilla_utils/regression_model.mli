(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2024 formalsec *)
(* Written by Hichem Rami Ait El Hara *)

module FeatMap : sig
  include Map.S with type key = string

  val find_def0 : key -> int t -> int
end

type features = int FeatMap.t

type score

val compare_score : score -> score -> int

val score_of_float : float -> score

val pp_score : score Fmt.t

type tree =
  | Leaf of score
  | Node of
      { feature : string
      ; threshold : score
      ; left : tree
      ; right : tree
      }

type gb_model =
  { init_value : score
  ; trees : tree list
  }

type dt_model = tree

type t =
  | GBModel of gb_model
  | DTModel of dt_model

val pp : t Fmt.t

val read_models_from_file : string -> (string * t) list

val choose_best : (score * 'a) list -> 'a

val predict : features -> t -> score

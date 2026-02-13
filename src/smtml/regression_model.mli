(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2024 formalsec *)
(* Written by Hichem Rami Ait El Hara *)

type t

type score

val choose_best : (score * 'a) list -> 'a

val read_models_from_file : string -> (string * t) list

val predict : Feature_extraction.features -> t -> score

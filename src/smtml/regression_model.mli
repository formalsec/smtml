(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2024 formalsec *)
(* Written by Hichem Rami Ait El Hara *)

type t

val read_models_from_file : string -> (string * t) list

val predict : Feature_extraction.features -> t -> float

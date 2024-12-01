(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2024 formalsec *)
(* Written by the Smtml programmers *)

type t =
  | True
  | False
  | Unit
  | Int of int
  | Real of float
  | Str of string
  | Num of Num.t
  | List of t list
  | App : [> `Op of string ] * t list -> t
  | Nothing

val type_of : t -> Ty.t

val compare : t -> t -> int

val equal : t -> t -> bool

val map : t -> (t -> t) -> t

val ( let+ ) : t -> (t -> t) -> t

val pp : t Fmt.t

val to_string : t -> string

val of_string : Ty.t -> string -> (t, string) result

val to_json : t -> Yojson.Basic.t

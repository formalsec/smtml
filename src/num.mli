(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2024 formalsec *)
(* Written by the Smtml programmers *)

type t =
  | I8 of int
  | I32 of int32
  | I64 of int64
  | F32 of int32
  | F64 of int64

type printer =
  [ `Pretty
  | `Hexadecimal
  ]

val type_of : t -> Ty.t

val compare : t -> t -> int

val equal : t -> t -> bool

val num_of_bool : bool -> t

val set_default_printer : printer -> unit

val pp : t Fmt.t

val pp_no_type : t Fmt.t

val to_string : t -> string

val of_string : Ty.t -> string -> (t, string) result

val to_json : t -> Yojson.Basic.t

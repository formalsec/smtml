(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2024 formalsec *)
(* Written by the Smtml programmers *)

type t = (Symbol.t, Value.t) Hashtbl.t

val iter : (Symbol.t * Value.t -> unit) -> t -> unit

val get_symbols : t -> Symbol.t list

(** bindings are sorted by symbol *)
val get_bindings : t -> (Symbol.t * Value.t) list

val evaluate : t -> Symbol.t -> Value.t option

val pp : ?no_values:bool -> t Fmt.t

val to_string : t -> string

val to_json : t -> Yojson.t

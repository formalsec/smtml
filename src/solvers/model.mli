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

(* TODO: do we want to keep it or only leave the following three to_*_string ? *)
val to_string : t -> string

val to_json : t -> Yojson.t

val to_json_string : t -> string

val to_scfg : t -> Scfg.Types.config

val to_scfg_string : t -> string

(* TODO:
   val to_smtlib : t -> ?
*)
val to_smtlib_string : t -> string

module Parse : sig
  module Json : sig
    val from_string : string -> (t, string) Result.t

    val from_channel : in_channel -> (t, string) Result.t

    val from_file : Fpath.t -> (t, string) Result.t
  end

  module Scfg : sig
    val from_string : string -> (t, string) Result.t

    val from_channel : in_channel -> (t, string) Result.t

    val from_file : Fpath.t -> (t, string) Result.t
  end

  module Smtlib : sig
    val from_string : string -> (t, string) Result.t
    [@@alert unsafe "not implemented"]

    val from_channel : in_channel -> (t, string) Result.t
    [@@alert unsafe "not implemented"]

    val from_file : Fpath.t -> (t, string) Result.t
    [@@alert unsafe "not implemented"]
  end
end

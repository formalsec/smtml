(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2024 formalsec *)
(* Written by the Smtml programmers *)

(** Model Module. This module defines a symbol table that maps symbols to
    values. It provides utility functions for iteration, retrieval, evaluation,
    serialization, and parsing from various formats. *)

type t = (Symbol.t, Value.t) Hashtbl.t

(** {1 Iteration} *)

(** [iter f tbl] applies function [f] to all bindings in [tbl]. *)
val iter : (Symbol.t * Value.t -> unit) -> t -> unit

(** {1 Retrieval} *)

(** [get_symbols tbl] returns the list of symbols stored in [tbl]. *)
val get_symbols : t -> Symbol.t list

(** [get_bindings tbl] returns all bindings in [tbl], sorted by symbol. *)
val get_bindings : t -> (Symbol.t * Value.t) list

(** {1 Evaluation} *)

(** [evaluate tbl sym] returns the value associated with symbol [sym] in [tbl],
    if any. *)
val evaluate : t -> Symbol.t -> Value.t option

(** {1 Pretty Printing} *)

(** [pp ?no_values fmt tbl] formats [tbl] using [Fmt.t]. If [no_values] is true,
    values are omitted from the output. *)
val pp : ?no_values:bool -> t Fmt.t

(** {1 Serialization} *)

(** [to_string tbl] converts [tbl] to a human-readable string. *)
val to_string : t -> string

(** [to_json tbl] converts [tbl] to a JSON representation. *)
val to_json : t -> Yojson.t

(** [to_json_string tbl] converts [tbl] to a JSON string. *)
val to_json_string : t -> string

(** [to_scfg ~no_value tbl] converts [tbl] to an SCFG configuration. If
    [no_value] is true, values are omitted from the output. *)
val to_scfg : no_value:bool -> t -> Scfg.Types.config

(** [to_scfg_string ~no_value tbl] converts [tbl] to an SCFG string. *)
val to_scfg_string : no_value:bool -> t -> string

(** [to_smtlib_string tbl] converts [tbl] to an SMT-LIB string. *)
val to_smtlib_string : t -> string

(** {1 Parsing} *)

module Parse : sig
  (** {2 JSON Parsing} *)
  module Json : sig
    (** [from_string s] parses a symbol table from a JSON string. *)
    val from_string : string -> (t, [> `Msg of string ]) Result.t

    (** [from_channel ic] parses a symbol table from a JSON input channel. *)
    val from_channel : in_channel -> (t, [> `Msg of string ]) Result.t

    (** [from_file path] parses a symbol table from a JSON file. *)
    val from_file : Fpath.t -> (t, [> `Msg of string ]) Result.t
  end

  (** {2 SCFG Parsing} *)
  module Scfg : sig
    (** [from_string s] parses a symbol table from an SCFG string. *)
    val from_string : string -> (t, [> `Msg of string ]) Result.t

    (** [from_channel ic] parses a symbol table from an SCFG input channel. *)
    val from_channel : in_channel -> (t, [> `Msg of string ]) Result.t

    (** [from_file path] parses a symbol table from an SCFG file. *)
    val from_file : Fpath.t -> (t, [> `Msg of string ]) Result.t
  end

  (** {2 SMT-LIB Parsing} *)
  module Smtlib : sig
    (** [from_string s] parses a symbol table from an SMT-LIB string.
        {b Warning:} Not implemented. *)
    val from_string : string -> (t, [> `Msg of string ]) Result.t
    [@@alert unsafe "not implemented"]

    (** [from_channel ic] parses a symbol table from an SMT-LIB input channel.
        {b Warning:} Not implemented. *)
    val from_channel : in_channel -> (t, [> `Msg of string ]) Result.t
    [@@alert unsafe "not implemented"]

    (** [from_file path] parses a symbol table from an SMT-LIB file.
        {b Warning:} Not implemented. *)
    val from_file : Fpath.t -> (t, [> `Msg of string ]) Result.t
    [@@alert unsafe "not implemented"]
  end
end

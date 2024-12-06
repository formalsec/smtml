(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2024 formalsec *)
(* Written by the Smtml programmers *)

type name =
  | Simple of string
  | Indexed of
      { basename : string
      ; indices : string list
      }

type namespace =
  | Attr
  | Sort
  | Term
  | Var

type t =
  { ty : Ty.t
  ; name : name
  ; namespace : namespace
  }

val attr : namespace

val sort : namespace

val term : namespace

val var : namespace

val ( @: ) : string -> Ty.t -> t

val name : t -> name

val namespace : t -> namespace

val compare : t -> t -> int

val equal : t -> t -> Bool.t

val make : Ty.t -> string -> t

val make3 : Ty.t -> name -> namespace -> t

val mk : namespace -> string -> t

val indexed : namespace -> string -> string list -> t

val pp_namespace : namespace Fmt.t

val pp : t Fmt.t

val to_string : t -> string

val to_json : t -> Yojson.Basic.t

val type_of : t -> Ty.t

(***************************************************************************)
(* This file is part of the third-party OCaml library `smtml`.             *)
(* Copyright (C) 2023-2024 formalsec                                       *)
(*                                                                         *)
(* This program is free software: you can redistribute it and/or modify    *)
(* it under the terms of the GNU General Public License as published by    *)
(* the Free Software Foundation, either version 3 of the License, or       *)
(* (at your option) any later version.                                     *)
(*                                                                         *)
(* This program is distributed in the hope that it will be useful,         *)
(* but WITHOUT ANY WARRANTY; without even the implied warranty of          *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the           *)
(* GNU General Public License for more details.                            *)
(*                                                                         *)
(* You should have received a copy of the GNU General Public License       *)
(* along with this program.  If not, see <https://www.gnu.org/licenses/>.  *)
(***************************************************************************)
type name = Simple of string

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

let attr = Attr

let sort = Sort

let term = Term

let var = Var

let simple name = Simple name

let ( @: ) (name : string) (ty : Ty.t) : t =
  { name = simple name; namespace = var; ty }

let name { name; _ } = name

let namespace { namespace; _ } = namespace

let discr_namespace = function Attr -> 0 | Sort -> 1 | Term -> 2 | Var -> 3

let compare_namespace a b = compare (discr_namespace a) (discr_namespace b)

let compare_name (Simple a) (Simple b) = String.compare a b

let compare a b =
  let compare_name = compare_name a.name b.name in
  if compare_name = 0 then
    let compare_ty = Ty.compare a.ty b.ty in
    if compare_ty = 0 then compare_namespace a.namespace b.namespace
    else compare_ty
  else compare_name

let equal a b = phys_equal a b || compare a b = 0

let make ty name = name @: ty

let make3 ty name namespace = { ty; name; namespace }

let mk namespace name = { ty = Ty_none; name = simple name; namespace }

let indexed _ = assert false

let pp_namespace fmt = function
  | Attr -> Fmt.string fmt "attr"
  | Sort -> Fmt.string fmt "sort"
  | Term -> Fmt.string fmt "term"
  | Var -> Fmt.string fmt "var"

let pp fmt { name = Simple name; _ } = Fmt.string fmt name

let to_string { name = Simple name; _ } = name

let to_json { name = Simple name; ty; _ } =
  `Assoc [ (name, `Assoc [ ("ty", `String (Fmt.str "%a" Ty.pp ty)) ]) ]

let type_of { ty; _ } = ty

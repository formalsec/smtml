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

let attr = Attr

let sort = Sort

let term = Term

let var = Var

module Name = struct
  let simple name = Simple name

  let indexed basename indices = Indexed { basename; indices }
end

let ( @: ) (name : string) (ty : Ty.t) : t =
  { name = Name.simple name; namespace = term; ty }

let name { name; _ } = name

let namespace { namespace; _ } = namespace

let discr_namespace = function Attr -> 0 | Sort -> 1 | Term -> 2 | Var -> 3

(* Optimized mixer (DJB2 variant). Inlines to simple arithmetic. *)
let[@inline] combine h v = (h * 33) + v

let hash_name n =
  match n with
  | Simple s ->
    (* Hashtbl.hash is fine for strings (it's a C primitive) *)
    combine 0 (Hashtbl.hash s)
  | Indexed { basename; indices } ->
    let h = combine 1 (Hashtbl.hash basename) in
    (* Fold over indices to avoid list allocation *)
    List.fold_left (fun acc s -> combine acc (Hashtbl.hash s)) h indices

let hash { ty; name; namespace } =
  let h = Ty.hash ty in
  let h = combine h (hash_name name) in
  combine h (discr_namespace namespace)

let compare_namespace a b = compare (discr_namespace a) (discr_namespace b)

let compare_name a b =
  match (a, b) with
  | Simple a, Simple b -> String.compare a b
  | ( Indexed { basename = base1; indices = indices1 }
    , Indexed { basename = base2; indices = indices2 } ) ->
    let compare_base = String.compare base1 base2 in
    if compare_base = 0 then List.compare String.compare indices1 indices2
    else compare_base
  | Simple _, _ -> -1
  | Indexed _, _ -> 1

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

let mk namespace name = { ty = Ty_none; name = Name.simple name; namespace }

let indexed namespace basename indices =
  { ty = Ty_none; name = Name.indexed basename indices; namespace }

let pp_namespace fmt = function
  | Attr -> Fmt.string fmt "attr"
  | Sort -> Fmt.string fmt "sort"
  | Term -> Fmt.string fmt "term"
  | Var -> Fmt.string fmt "var"

let pp_name fmt = function
  | Simple name -> Fmt.string fmt name
  | Indexed { basename; indices } ->
    Fmt.pf fmt "(%s %a)" basename (Fmt.list ~sep:Fmt.sp Fmt.string) indices

let pp fmt { name; _ } = pp_name fmt name

let to_string { name; _ } = Fmt.str "%a" pp_name name

let to_json { name; ty; _ } =
  `Assoc
    [ ( Fmt.str "%a" pp_name name
      , `Assoc [ ("ty", `String (Fmt.str "%a" Ty.pp ty)) ] )
    ]

let type_of { ty; _ } = ty

module Map = Map.Make (struct
  type nonrec t = t

  let compare = compare
end)

module Smtlib = struct
  let pp fmt { ty; name; namespace } =
    match namespace with
    | Term -> pp_name fmt name
    | Sort -> Ty.Smtlib.pp fmt ty
    | Var -> assert false
    | Attr -> assert false
end

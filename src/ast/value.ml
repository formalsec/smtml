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

open Ty

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

let rec compare (v1 : t) (v2 : t) : int =
  match (v1, v2) with
  | True, True | False, False | Unit, Unit | Nothing, Nothing -> 0
  | False, True -> -1
  | True, False -> 1
  | Int x1, Int x2 -> Int.compare x1 x2
  | Real x1, Real x2 -> Float.compare x1 x2
  | Str x1, Str x2 -> String.compare x1 x2
  | Num x1, Num x2 -> Num.compare x1 x2
  | List l1, List l2 -> List.compare compare l1 l2
  | App (`Op op1, vs1), App (`Op op2, vs2) ->
    let c = String.compare op1 op2 in
    if c = 0 then List.compare compare vs1 vs2 else c
  | ( ( True | False | Unit | Int _ | Real _ | Str _ | Num _ | List _ | App _
      | Nothing )
    , _ ) ->
    assert false

let rec equal (v1 : t) (v2 : t) : bool =
  match (v1, v2) with
  | True, True | False, False | Unit, Unit | Nothing, Nothing -> true
  | Int x1, Int x2 -> Int.equal x1 x2
  | Real x1, Real x2 -> Float.equal x1 x2
  | Str x1, Str x2 -> String.equal x1 x2
  | Num x1, Num x2 -> Num.equal x1 x2
  | List l1, List l2 -> List.equal equal l1 l2
  | App (`Op op1, vs1), App (`Op op2, vs2) ->
    String.equal op1 op2 && List.equal equal vs1 vs2
  | ( ( True | False | Unit | Int _ | Real _ | Str _ | Num _ | List _ | App _
      | Nothing )
    , _ ) ->
    false

let map v f = match v with Nothing -> Nothing | _ -> f v

let ( let+ ) = map

let rec pp (fmt : Fmt.formatter) (v : t) : unit =
  match v with
  | True -> Fmt.string fmt "true"
  | False -> Fmt.string fmt "false"
  | Unit -> Fmt.string fmt "unit"
  | Int x -> Fmt.int fmt x
  | Real x -> Fmt.pf fmt "%F" x
  | Num x -> Num.pp fmt x
  | Str x -> Fmt.pf fmt "%S" x
  | List l -> (Fmt.hovbox ~indent:1 (Fmt.list ~sep:Fmt.comma pp)) fmt l
  | App (`Op op, vs) ->
    Fmt.pf fmt "@[<hov 1>%s(%a)@]" op (Fmt.list ~sep:Fmt.comma pp) vs
  | Nothing -> Fmt.string fmt "none"
  | App _ -> assert false

let to_string (v : t) : string = Fmt.str "%a" pp v

let rec to_json (v : t) : Yojson.Basic.t =
  match v with
  | True -> `Bool true
  | False -> `Bool false
  | Unit -> `String "unit"
  | Int int -> `Int int
  | Real real -> `Float real
  | Str str -> `String str
  | Num n -> Num.to_json n
  | List l -> `List (List.map to_json l)
  | Nothing -> `Null
  | App _ -> assert false

let type_of (v : t) : Ty.t =
  match v with
  | True | False -> Ty_bool
  | Unit -> Ty_unit
  | Int _ -> Ty_int
  | Real _ -> Ty_real
  | Str _ -> Ty_str
  | Num n -> Num.type_of n
  | List _ -> Ty_list
  | App _ -> Ty_app
  | Nothing -> Ty_none

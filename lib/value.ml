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
  | Int of int
  | Real of float
  | Str of string
  | Num of Num.t
  | List of t list
  | App : [> `Op of string ] * t list -> t

let rec equal (v1 : t) (v2 : t) : Bool.t =
  match (v1, v2) with
  | True, True | False, False -> true
  | Int x1, Int x2 -> Int.equal x1 x2
  | Real x1, Real x2 -> x1 = x2
  | Str x1, Str x2 -> String.equal x1 x2
  | Num x1, Num x2 -> Num.equal x1 x2
  | List l1, List l2 -> List.equal equal l1 l2
  | _ -> false

let rec compare v1 v2 =
  match (v1, v2) with
  | True, True | False, False -> 0
  | False, True -> -1
  | True, False -> 1
  | Int x1, Int x2 -> Int.compare x1 x2
  | Real x1, Real x2 -> Float.compare x1 x2
  | Str x1, Str x2 -> String.compare x1 x2
  | Num x1, Num x2 -> Num.compare x1 x2
  | List l1, List l2 -> List.compare compare l1 l2
  | _ -> compare v1 v2

let type_of (v : t) : Ty.t =
  match v with
  | True | False -> Ty_bool
  | Int _ -> Ty_int
  | Real _ -> Ty_real
  | Str _ -> Ty_str
  | Num n -> Num.type_of n
  | List _ -> Ty_list
  | App _ -> Ty_app

let rec pp fmt (v : t) =
  let open Format in
  match v with
  | True -> pp_print_string fmt "true"
  | False -> pp_print_string fmt "false"
  | Int x -> pp_print_int fmt x
  | Real x -> fprintf fmt "%F" x
  | Num x -> Num.pp fmt x
  | Str x -> Format.fprintf fmt "%S" x
  | List l ->
    fprintf fmt "[%a]"
      (pp_print_list ~pp_sep:(fun fmt () -> pp_print_string fmt ", ") pp)
      l
  | App (`Op op, vs) ->
    fprintf fmt "%s(%a)" op
      (pp_print_list ~pp_sep:(fun fmt () -> pp_print_string fmt ", ") pp)
      vs
  | _ -> assert false

let pp_num fmt (v : t) =
  match v with Num x -> Num.pp_hex fmt x | _ -> pp fmt v

let to_string v = Format.asprintf "%a" pp v

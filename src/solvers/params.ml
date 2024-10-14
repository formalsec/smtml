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

type _ param =
  | Timeout : int param
  | Model : bool param
  | Unsat_core : bool param
  | Ematching : bool param
  | Parallel : bool param
  | Num_threads : int param

let discr : type a. a param -> int = function
  | Timeout -> 0
  | Model -> 1
  | Unsat_core -> 2
  | Ematching -> 3
  | Parallel -> 4
  | Num_threads -> 5

module Key = struct
  type t = K : 'a param -> t

  let v v = K v

  let compare (K a) (K b) = compare (discr a) (discr b)
end

module Pmap = Map.Make (Key)

type param' = P : 'a param * 'a -> param'

let p k v = P (k, v)

type t = param' Pmap.t

let default_timeout = 2147483647

let default_model = true

let default_unsat_core = false

let default_ematching = true

let default_parallel = false

let default_num_threads = 1

let default_value (type a) (param : a param) : a =
  match param with
  | Timeout -> default_timeout
  | Model -> default_model
  | Unsat_core -> default_unsat_core
  | Ematching -> default_ematching
  | Parallel -> default_parallel
  | Num_threads -> default_num_threads

let default () =
  Pmap.empty
  |> Pmap.add (Key.v Timeout) (p Timeout default_timeout)
  |> Pmap.add (Key.v Model) (p Model default_model)
  |> Pmap.add (Key.v Unsat_core) (p Unsat_core default_unsat_core)
  |> Pmap.add (Key.v Ematching) (p Ematching default_ematching)

let set (type a) (params : t) (param : a param) (value : a) : t =
  Pmap.add (Key.v param) (p param value) params

let opt (type a) (params : t) (param : a param) (opt_value : a option) : t =
  Option.fold ~none:params ~some:(set params param) opt_value

let ( $ ) (type a) (params : t) ((param, value) : a param * a) : t =
  set params param value

let get (type a) (params : t) (param : a param) : a =
  match (param, Pmap.find (Key.v param) params) with
  | Timeout, P (Timeout, v) -> v
  | Model, P (Model, v) -> v
  | Unsat_core, P (Unsat_core, v) -> v
  | Ematching, P (Ematching, v) -> v
  | Parallel, P (Parallel, v) -> v
  | Num_threads, P (Num_threads, v) -> v
  | (Timeout | Model | Unsat_core | Ematching | Parallel | Num_threads), _ ->
    assert false

let to_list params = List.map snd @@ Pmap.bindings params

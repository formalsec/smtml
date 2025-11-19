(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2024 formalsec *)
(* Written by the Smtml programmers *)

type _ param =
  | Timeout : int param
  | Model : bool param
  | Unsat_core : bool param
  | Ematching : bool param
  | Parallel : bool param
  | Num_threads : int param
  | Debug : bool param
  | Random_seed : int param

let discr : type a. a param -> int = function
  | Timeout -> 0
  | Model -> 1
  | Unsat_core -> 2
  | Ematching -> 3
  | Parallel -> 4
  | Num_threads -> 5
  | Debug -> 6
  | Random_seed -> 7

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

let default_debug = false

let default_random_seed = 0

let default_value (type a) (param : a param) : a =
  match param with
  | Timeout -> default_timeout
  | Model -> default_model
  | Unsat_core -> default_unsat_core
  | Ematching -> default_ematching
  | Parallel -> default_parallel
  | Num_threads -> default_num_threads
  | Debug -> default_debug
  | Random_seed -> default_random_seed

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
  match (param, Pmap.find_opt (Key.v param) params) with
  | _, None -> assert false
  | Timeout, Some (P (Timeout, v)) -> v
  | Model, Some (P (Model, v)) -> v
  | Unsat_core, Some (P (Unsat_core, v)) -> v
  | Ematching, Some (P (Ematching, v)) -> v
  | Parallel, Some (P (Parallel, v)) -> v
  | Num_threads, Some (P (Num_threads, v)) -> v
  | Debug, Some (P (Debug, v)) -> v
  | Random_seed, Some (P (Random_seed, v)) -> v
  | ( ( Timeout | Model | Unsat_core | Ematching | Parallel | Num_threads
      | Debug | Random_seed )
    , _ ) ->
    assert false

let to_list params = List.map snd @@ Pmap.bindings params

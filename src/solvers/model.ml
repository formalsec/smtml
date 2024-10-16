(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2024 formalsec *)
(* Written by the Smtml programmers *)

type t = (Symbol.t, Value.t) Hashtbl.t

let iter f model = Hashtbl.iter (fun a b -> f (a, b)) model

let get_symbols (model : t) : Symbol.t List.t =
  Hashtbl.to_seq_keys model |> List.of_seq |> List.sort Symbol.compare

let compare_bindings (s1, v1) (s2, v2) =
  let compare_symbol = Symbol.compare s1 s2 in
  if compare_symbol = 0 then Value.compare v1 v2 else compare_symbol

let get_bindings (model : t) : (Symbol.t * Value.t) list =
  Hashtbl.to_seq model |> List.of_seq |> List.sort compare_bindings

let evaluate (model : t) (symb : Symbol.t) : Value.t option =
  Hashtbl.find_opt model symb

let pp_bindings fmt ?(no_values = false) model =
  Fmt.list
    ~sep:(fun fmt () -> Fmt.pf fmt "@\n")
    (fun fmt (key, data) ->
      if not no_values then Fmt.pf fmt "(%a %a)" Symbol.pp key Value.pp data
      else
        let t = Symbol.type_of key in
        Fmt.pf fmt "(%a %a)" Symbol.pp key Ty.pp t )
    fmt (get_bindings model)

let pp ?(no_values = false) fmt model =
  Fmt.pf fmt "(model@\n  @[<v>%a@])" (pp_bindings ~no_values) model

let to_string (model : t) : string = Fmt.str "%a" (pp ~no_values:false) model

let to_json (model : t) : Yojson.t =
  let combine = Yojson.Basic.Util.combine in
  let add_assignment sym value assignments =
    let assignment =
      match Symbol.to_json sym with
      | `Assoc [ (name, props) ] ->
        let value = `Assoc [ ("value", Value.to_json value) ] in
        `Assoc [ (name, combine props value) ]
      | _ -> Fmt.failwith "Model: Symbol.to_json returned something impossible"
    in
    combine assignments assignment
  in
  let model :> Yojson.t = Hashtbl.fold add_assignment model (`Assoc []) in
  `Assoc [ ("model", model) ]

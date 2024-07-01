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
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
    (fun fmt (key, data) ->
      if not no_values then
        Format.fprintf fmt "(%a %a)" Symbol.pp key Value.pp data
      else
        let t = Symbol.type_of key in
        Format.fprintf fmt "(%a %a)" Symbol.pp key Ty.pp t )
    fmt (get_bindings model)

let pp fmt ?(no_values = false) model =
  Format.fprintf fmt "(model@\n  @[<v>%a@])" (pp_bindings ~no_values) model

let to_string (model : t) : string =
  Format.asprintf "%a" (pp ~no_values:false) model

let to_json (model : t) : Yojson.t =
  let model :> Yojson.t list =
    Hashtbl.fold
      (fun s v acc ->
        let s = Symbol.to_json s in
        let v = `Assoc [ ("value", Value.to_json v) ] in
        Yojson.Basic.Util.combine s v :: acc )
      model []
  in
  `Assoc [ ("model", `List model) ]

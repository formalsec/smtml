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
      let t = Symbol.type_of key in
      if not no_values then
        Fmt.pf fmt "(%a %a %a)" Symbol.pp key Ty.pp t Value.pp data
      else Fmt.pf fmt "(%a %a)" Symbol.pp key Ty.pp t )
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

(** {b Example}: Model in the json format:

    {@json[
      {
        "model" : {
          "x_0" : { "ty" : "int", "value" : 42 },
          "x_1" : { "ty" : "bool", "value" : true },
          "x_2" : { "ty" : "f32", "value" : 42.42 }
        }
      }
    ]} *)
let to_json_string model =
  let model = to_json model in
  Fmt.str "%a" (Yojson.pretty_print ~std:true) model

let to_scfg ~no_value model =
  let open Scfg.Types in
  let children =
    let bindings = get_bindings model in
    List.map
      (fun (symbol, value) ->
        let p0 = Symbol.to_string symbol in
        let p1 = Symbol.type_of symbol |> Ty.string_of_type in
        let params =
          if no_value then [ p0; p1 ]
          else
            let p2 = Value.to_string value in
            [ p0; p1; p2 ]
        in
        { name = "symbol"; params; children = [] } )
      bindings
  in
  [ { name = "model"; params = []; children } ]

(** {b Example}: Model in the scfg format:

    {@scfg[
      model {
        symbol x_0 int 42
        symbol x_1 bool true
        symbol x_2 f32 42.42
      }
    ]} *)
let to_scfg_string ~no_value model =
  let model = to_scfg ~no_value model in
  Fmt.str "%a" Scfg.Pp.config model

let to_smtlib _model = assert false

(** {b Example}: TODO *)
let to_smtlib_string model =
  let _model = to_smtlib model in
  assert false

module Parse = struct
  module Json = struct
    open Result.Syntax
    module Json = Yojson.Basic

    let from_json json =
      let symbols = Json.Util.member "model" json |> Json.Util.to_assoc in
      let tbl = Hashtbl.create 16 in
      let* () =
        Result.list_iter
          (fun (symbol, json) ->
            let ty = Json.Util.member "ty" json |> Json.Util.to_string in
            let* ty = Ty.of_string ty in
            let value =
              (* FIXME: this is a bit hackish in order to reuse the previous code *)
              match Json.Util.member "value" json with
              | `Bool x -> Bool.to_string x
              | `Float x -> Float.to_string x
              | `Int x -> Int.to_string x
              | `String x -> x
              | _ -> assert false
            in
            let+ value = Value.of_string ty value in
            let key = Symbol.make ty symbol in
            Hashtbl.add tbl key value )
          symbols
      in
      Ok tbl

    let from_string s = Json.from_string s |> from_json

    let from_channel chan = Json.from_channel chan |> from_json

    let from_file file =
      let file = Fpath.to_string file in
      Json.from_file ~fname:file file |> from_json
  end

  module Scfg = struct
    open Result.Syntax

    let from_scfg v =
      match Scfg.Query.get_dir "model" v with
      | None ->
        Fmt.error_msg
          "can not find the directive `model` while parsing the scfg config"
      | Some model ->
        let symbols = Scfg.Query.get_dirs "symbol" model.children in
        let tbl = Hashtbl.create 16 in
        let* () =
          Result.list_iter
            (fun symbol ->
              let* name = Scfg.Query.get_param 0 symbol in
              let* ty = Scfg.Query.get_param 1 symbol in
              let* ty = Ty.of_string ty in
              let* value = Scfg.Query.get_param 2 symbol in
              let+ value = Value.of_string ty value in
              let key = Symbol.make ty name in
              Hashtbl.add tbl key value )
            symbols
        in
        Ok tbl

    let from_string s =
      let* model = Scfg.Parse.from_string s in
      from_scfg model

    let from_channel chan =
      let* model = Scfg.Parse.from_channel chan in
      from_scfg model

    let from_file file =
      let* model = Scfg.Parse.from_file file in
      from_scfg model
  end

  module Smtlib = struct
    let from_string _s = assert false

    let from_channel _chan = assert false

    let from_file _file = assert false
  end
end

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

let to_json_string model =
  let model = to_json model in
  Fmt.str "%a" Yojson.pp model

let to_scfg model =
  let open Scfg.Types in
  let children =
    let bindings = get_bindings model in
    List.map
      (fun (symbol, value) ->
        let p0 = Symbol.to_string symbol in
        let p1 = Symbol.type_of symbol |> Ty.string_of_type in
        let p2 = Value.to_string value in
        let params = [ p0; p1; p2 ] in
        { name = "symbol"; params; children = [] } )
      bindings
  in
  [ { name = "model"; params = []; children } ]

let to_scfg_string model =
  let model = to_scfg model in
  Fmt.str "%a" Scfg.Pp.config model

let to_smtlib _model = assert false

let to_smtlib_string model =
  let _model = to_smtlib model in
  assert false

module Parse = struct
  module Json = struct
    let from_string _s = assert false

    let from_channel _chan = assert false

    let from_file _file = assert false
  end

  module Scfg = struct
    let ( let* ) = Result.bind

    let from_scfg v =
      match Scfg.Query.get_dir "model" v with
      | None ->
        Error "can not find the directive `model` while parsing the scfg config"
      | Some model ->
        let symbols = Scfg.Query.get_dirs "symbol" model.children in
        let tbl = Hashtbl.create 16 in
        let* () =
          List.fold_left
            (fun acc symbol ->
              let* () = acc in
              let* name = Scfg.Query.get_param 0 symbol in
              let* ty = Scfg.Query.get_param 1 symbol in
              let* ty = Ty.of_string ty in
              let* value = Scfg.Query.get_param 2 symbol in
              let* value =
                (* TODO: expose a Value.of_string : ty -> s -> Value.t instead ? *)
                match ty with
                | Ty_bitv _n -> begin
                  match int_of_string value with
                  | None -> Error "invalid symbol value, expected integer"
                  | Some n -> Ok (Value.Int n)
                end
                | _ -> assert false
              in
              let key = Symbol.make ty name in
              Hashtbl.add tbl key value;
              Ok () )
            (Ok ()) symbols
        in
        Ok tbl

    let from_string s =
      let* model = Scfg.Parse.from_string s in
      from_scfg model

    let from_channel chan =
      let* model = Scfg.Parse.from_channel chan in
      from_scfg model

    let from_file file =
      let* model = Scfg.Parse.from_file (Fpath.to_string file) in
      from_scfg model
  end

  module Smtlib = struct
    let from_string _s = assert false

    let from_channel _chan = assert false

    let from_file _file = assert false
  end
end

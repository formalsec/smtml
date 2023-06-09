open Core

type t = (Symbol.t, Value.t) Hashtbl.t

let get_symbols (model : t) : Symbol.t List.t = Hashtbl.keys model

let get_bindings (model : t) : (Symbol.t * Value.t) List.t =
  Hashtbl.to_alist model

let evaluate (model : t) (symb : Symbol.t) : Value.t Option.t =
  Hashtbl.find model symb

let to_string (model : t) : String.t =
  let bindings =
    Hashtbl.fold model ~init:"" ~f:(fun ~key ~data accum ->
        let x = Symbol.to_string key
        and t = Types.string_of_type (Symbol.type_of key)
        and v = Value.to_string data in
        sprintf "%s  (%s %s %s)\n" accum x t v)
  in
  sprintf "(model\n%s)" bindings

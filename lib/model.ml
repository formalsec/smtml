open Core

type t = (Symbol.t, Value.t) Hashtbl.t

let get_symbols (model : t) : Symbol.t List.t = Hashtbl.keys model

let get_bindings (model : t) : (Symbol.t * Value.t) List.t =
  Hashtbl.to_alist model

let evaluate (model : t) (symb : Symbol.t) : Value.t Option.t =
  Hashtbl.find model symb

let pp_bindings fmt model =
  Hashtbl.iteri model ~f:(fun ~key ~data ->
    let t = Symbol.type_of key in
    Format.fprintf fmt "(%a %a %a)@\n" Symbol.pp key Types.pp_type t Value.pp
      data )

let pp fmt model = Format.fprintf fmt "(model@\n%a)" pp_bindings model
let to_string (model : t) : String.t = Format.asprintf "%a" pp model

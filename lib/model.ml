type t = (Symbol.t, Value.t) Hashtbl.t

let get_symbols (model : t) : Symbol.t List.t =
  Hashtbl.to_seq_keys model |> List.of_seq

let get_bindings (model : t) : (Symbol.t * Value.t) List.t =
  Hashtbl.to_seq model |> List.of_seq

let evaluate (model : t) (symb : Symbol.t) : Value.t Option.t =
  Hashtbl.find_opt model symb

let pp_bindings fmt model =
  Hashtbl.iter
    (fun key data ->
      let t = Symbol.type_of key in
      Format.fprintf fmt "(%a %a %a)@\n" Symbol.pp key Types.pp_type t Value.pp
        data )
    model

let pp fmt model = Format.fprintf fmt "(model@\n%a)" pp_bindings model
let to_string (model : t) : String.t = Format.asprintf "%a" pp model

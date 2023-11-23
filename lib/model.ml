type t = (Symbol.t, Value.t) Hashtbl.t

let get_symbols (model : t) : Symbol.t List.t =
  Hashtbl.to_seq_keys model |> List.of_seq

let get_bindings (model : t) : (Symbol.t * Value.t) List.t =
  Hashtbl.to_seq model |> List.of_seq

let evaluate (model : t) (symb : Symbol.t) : Value.t Option.t =
  Hashtbl.find_opt model symb

let pp_print_hashtbl ~pp_sep pp_v fmt v =
  Format.pp_print_seq ~pp_sep pp_v fmt (Hashtbl.to_seq v)

let pp_bindings fmt model =
  pp_print_hashtbl
    ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
    (fun fmt (key, data) ->
      let t = Symbol.type_of key in
      Format.fprintf fmt "(%a %a %a)" Symbol.pp key Ty.pp t Value.pp data )
    fmt model

let pp fmt model = Format.fprintf fmt "(model@\n  @[<v>%a@])" pp_bindings model
let to_string (model : t) : String.t = Format.asprintf "%a" pp model

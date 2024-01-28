type t = (Symbol.t, Value.t) Hashtbl.t

let get_symbols (model : t) : Symbol.t List.t =
  Hashtbl.to_seq_keys model |> List.of_seq |> List.sort Symbol.compare

let compare_bindings (s1, v1) (s2, v2) =
  let compare_symbol = Symbol.compare s1 s2 in
  if compare_symbol = 0 then Value.compare v1 v2 else compare_symbol

let get_bindings (model : t) : (Symbol.t * Value.t) List.t =
  Hashtbl.to_seq model |> List.of_seq |> List.sort compare_bindings

let evaluate (model : t) (symb : Symbol.t) : Value.t Option.t =
  Hashtbl.find_opt model symb

let pp_print_hashtbl ~pp_sep pp_v fmt v =
  let l = Hashtbl.to_seq v |> List.of_seq |> List.sort compare_bindings in
  Format.pp_print_list ~pp_sep pp_v fmt l

let pp_bindings fmt model =
  pp_print_hashtbl
    ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
    (fun fmt (key, data) ->
      Format.fprintf fmt "(%a %a)" Symbol.pp key Value.pp data )
    fmt model

let pp fmt model = Format.fprintf fmt "(model@\n  @[<v>%a@])" pp_bindings model

let to_string (model : t) : String.t = Format.asprintf "%a" pp model

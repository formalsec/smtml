type t =
  | Assert of term
  | Check_sat
  | Push
  | Pop of int
  | Let_const of Symbol.t
  | Get_model

and term =
  | E of Expr.t
  | Let of binding list * term

and binding = string * term

let rec pp_term fmt = function
  | E e -> Expr.pp fmt e
  | Let (binds, t) ->
    Format.fprintf fmt "(let (%a) %a)"
      (Format.pp_print_list ~pp_sep:Format.pp_print_space pp_binding)
      binds pp_term t

and pp_binding fmt (x, t) = Format.fprintf fmt "(%s %a)" x pp_term t

let pp fmt (instr : t) =
  match instr with
  | Assert t -> Format.fprintf fmt "(assert @[<h 2>%a@])" pp_term t
  | Check_sat -> Format.pp_print_string fmt "(check-sat)"
  | Push -> Format.pp_print_string fmt "(push)"
  | Pop n -> Format.fprintf fmt "(pop %d)" n
  | Let_const s ->
    let ty = Symbol.type_of s in
    Format.fprintf fmt "(let-const %a %a)" Symbol.pp s Ty.pp ty
  | Get_model -> Format.pp_print_string fmt "(get-model)"

let to_string (instr : t) : string = Format.asprintf "%a" pp instr

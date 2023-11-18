type t =
  | Declare of Symbol.t
  | Assert of Expr.t
  | CheckSat
  | GetModel

let pp fmt (instr : t) =
  match instr with
  | Declare s ->
    let ty = Symbol.type_of s in
    Format.fprintf fmt "(declare-fun %a %a)" Symbol.pp s Ty.pp ty
  | Assert e ->
      Format.fprintf fmt "(assert @[<h 2>%a@])" Expr.pp e
  | CheckSat -> Format.pp_print_string fmt "(check-sat)"
  | GetModel -> Format.pp_print_string fmt "(get-model)"

let to_string (instr : t) : string =
  Format.asprintf "%a" pp instr

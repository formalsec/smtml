type t =
  | Assert of Expr.t
  | Check_sat
  | Push
  | Pop of int
  | Let_const of Symbol.t
  | Get_model
  | Set_logic of Solver_intf.logic

let pp fmt (instr : t) =
  match instr with
  | Assert e -> Format.fprintf fmt "(assert @[<h 2>%a@])" Expr.pp e
  | Check_sat -> Format.pp_print_string fmt "(check-sat)"
  | Push -> Format.pp_print_string fmt "(push)"
  | Pop n -> Format.fprintf fmt "(pop %d)" n
  | Let_const s ->
    let ty = Symbol.type_of s in
    Format.fprintf fmt "(let-const %a %a)" Symbol.pp s Ty.pp ty
  | Get_model -> Format.pp_print_string fmt "(get-model)"
  | Set_logic _logic -> ()

let to_string (instr : t) : string = Format.asprintf "%a" pp instr

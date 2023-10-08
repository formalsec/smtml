type t =
  | Declare of Symbol.t
  | Assert of Expression.t
  | CheckSat
  | GetModel

let to_string (instr : t) : String.t =
  match instr with
  | Declare s ->
    let symb = Symbol.to_string s
    and t = Types.string_of_type (Symbol.type_of s) in
    Printf.sprintf "(declare-fun %s %s)" symb t
  | Assert e -> Printf.sprintf "(assert %s)" (Expression.to_string e)
  | CheckSat -> "(check-sat)"
  | GetModel -> "(get-model)"

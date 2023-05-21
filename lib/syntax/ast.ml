open Core

type t =
  | Decl of Symbol.t
  | Assert of Expression.t
  | CheckSat

let to_string (instr : t) : String.t =
  match instr with
  | Decl s ->
      let symb = Symbol.to_string s
      and t = Types.string_of_type (Symbol.type_of s) in
      sprintf "(decl %s %s)" symb t
  | Assert e -> sprintf "(assert %s)" (Expression.to_string e)
  | CheckSat -> "(check-sat)"

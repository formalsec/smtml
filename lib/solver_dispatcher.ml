(* TODO: put this in some other more appropriate module? *)
type solver_type =
  | Z3_solver
  | Cvc5_solver
  | Colibri2_solver
  | Bitwuzla_solver

let mappings_of_solver : solver_type -> (module Mappings_intf.S) = function
  | Z3_solver -> (module Z3_mappings)
  | Cvc5_solver -> (module Cvc5_mappings)
  | Colibri2_solver -> (module Colibri2_mappings)
  | Bitwuzla_solver -> (module Bitwuzla_mappings)

let solver_type_of_string (s : string) :
  (solver_type, [> `Msg of string ]) result =
  match String.map Char.lowercase_ascii s with
  | "z3" -> Ok Z3_solver
  | "colibri2" -> Ok Colibri2_solver
  | "bitwuzla" -> Ok Bitwuzla_solver
  | "cvc5" -> Ok Cvc5_solver
  | s -> Error (`Msg (Format.sprintf "unknown solver %s" s))

let pp_solver_type fmt = function
  | Z3_solver -> Format.fprintf fmt "Z3"
  | Cvc5_solver -> Format.fprintf fmt "CVC5"
  | Colibri2_solver -> Format.fprintf fmt "Colibri2"
  | Bitwuzla_solver -> Format.fprintf fmt "Bitwuzla"

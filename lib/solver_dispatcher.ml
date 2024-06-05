(* TODO: put this in some other more appropriate module? *)
type solver_type =
  | Z3_solver
  | Z3_solver2
  | Cvc5_solver
  | Colibri2_solver
  | Bitwuzla_solver

let mappings_of_solver : solver_type -> (module Mappings_intf.S) = function
  | Z3_solver -> (module Z3_mappings)
  | Z3_solver2 -> (module Z3_mappings2)
  | Cvc5_solver -> (module Cvc5_mappings)
  | Colibri2_solver -> (module Colibri2_mappings)
  | Bitwuzla_solver -> (module Bitwuzla_mappings)

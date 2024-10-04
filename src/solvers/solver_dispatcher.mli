type solver_type =
  | Z3_solver
  | Bitwuzla_solver
  | Colibri2_solver
  | Cvc5_solver

val is_available : solver_type -> bool

(** List of all available solvers. Can be empty if no solver installed. *)
val available_solvers : solver_type list

(** Returns first available solver or errors when none exist *)
val solver : ((module Mappings.S_with_fresh), [> `Msg of string ]) result

val mappings_of_solver : solver_type -> (module Mappings.S_with_fresh)

val solver_type_of_string : string -> (solver_type, [> `Msg of string ]) result

val pp_solver_type : Fmt.formatter -> solver_type -> unit

(** A module for creating "batch" solvers *)

exception Unknown

module Make (M : Mappings_intf.S) : Solver_intf.S

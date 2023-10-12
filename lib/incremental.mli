(** A module for creating "incremental" solvers *)

exception Unknown

module Make (M : Mappings_intf.S) : Solver_intf.S

(** The Encoding module defines two types of solvers: {!module:Batch} and
    {!module:Incremental}. The generic definition of these solvers is presented
    here, and they are parametric on the mappings of the underlying SMT solver.
    This design allows for the creation of portable solvers that can be used
    with various SMT solvers implementing {!module-type:Mappings_intf.S}.

    {1 Batch Mode}

    In this module, constraints are handled in a 'batch' mode, meaning that the
    solver delays all interactions with the underlying SMT solver until it
    becomes necessary. It essentially communicates with the underlying solver
    only when a call to {!val:Solver_intf.S.check},
    {!val:Solver_intf.S.get_value}, or {!val:Solver_intf.S.model} is made. *)

(** {!module:Batch} is parameterized by the mapping module [M] implementing
    {!module-type:Mappings_intf.S}. In this mode, the solver delays all
    interactions with the underlying SMT solver until it becomes necessary. *)
module Batch (M : Mappings_intf.S) : Solver_intf.S

(** {!module:Z3_batch} is a concrete instantiation of {!module:Batch} with
    {!module:Z3_mappings}, providing a solver specifically tailored for the Z3
    SMT solver. *)
module Z3_batch : Solver_intf.S

(** {1 Incremental Model}

    In the Incremental module, constraints are managed incrementally, signifying
    that upon their addition to the solver, this module promptly communicates
    with the underlying SMT solver. Unlike the batch solver, nearly every
    interaction with this solver prompts a corresponding interaction with the
    underlying SMT solver. *)

(** The {!module:Incremental} module, akin to {!module:Batch}, presents a solver
    parameterized by the mapping module [M]. In this mode, the Incremental
    solver engages with the underlying SMT solver in nearly every interaction. *)
module Incremental (M : Mappings_intf.S) : Solver_intf.S

(** {!module:Z3_incremental} is a specific instantiation of
    {!module:Incremental} with {!module:Z3_mappings}, creating an incremental
    solver designed for the Z3 SMT solver.*)
module Z3_incremental : Solver_intf.S

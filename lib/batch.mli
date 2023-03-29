open Base
open Z3
open Types

type t = { solver : s; pc : Formula.t ref }
and s = Solver.solver

val solver_time : float ref
val solver_count : int ref
val create : unit -> t

val clone : t -> t
(** [clone solver] makes a copy of the current [solver] *)

val interrupt : unit -> unit
(** [interrupt ()] sends interrupt signal to SMT solver *)

val add : t -> Expression.t -> unit
(** [add solver e] adds assertion [e] to [solver] *)

val add_formula : t -> Formula.t -> unit
(** [add solver f] adds formula [f] to [solver] *)

val ccheck : t -> Formula.t -> bool
(** [ccheck solver [e1; ...; en]] checks the satisfiability of [e1, ..., en]
    without adding the expressions as assertions to the solver *)

val check : t -> Expression.t option -> bool
(** [check solver [e1; ...; en]] checks the satisfiability of the
    existing pc with [e1, ..., en] but without adding the expressions
    as assertions to the solver *)

val fork : t -> Expression.t -> bool * bool
(** [fork solver e] checks the satisfiability of the fork on the condition [e] *)

val value_binds : t -> (string * expr_type) list -> (string * Num.t) list

val string_binds :
  t -> (string * expr_type) list -> (string * string * string) list

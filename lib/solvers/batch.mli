open Base
open Z3
open Types

exception Unknown

type t = { solver : s; pc : Expression.t ref }
and s = Solver.solver

val solver_time : float ref
val solver_count : int ref
val create : unit -> t

val clone : t -> t
(** [clone solver] makes a copy of the current [solver] *)

val interrupt : unit -> unit
(** [interrupt ()] sends interrupt signal to SMT solver *)

val set_default_axioms : Solver.solver -> unit
(** add default axioms to solver *)

val add : t -> Expression.t -> unit
(** [add solver e] adds assertion [e] to [solver] *)

val check_sat : t -> Expression.t list -> bool
(** [check_sat solver [e1; ...; en]] checks the satisfiability of the
    existing pc with [e1, ..., en] but without adding the expressions
    as assertions to the solver *)

val check : t -> Expression.t option -> bool
(** [check solver e] *)

val eval : t -> Expression.t -> Expression.t list -> Value.t option
(** [eval solver e es] evaluates a possible value of the const [e] in the 
    the context of the assertions [es] *)

val fork : t -> Expression.t -> bool * bool
(** [fork solver e] checks the satisfiability of the fork on the condition [e] *)

val value_binds : t -> (string * expr_type) list -> (string * Value.t) list
val string_binds : t -> (string * string * string) list

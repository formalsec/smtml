exception Unknown

type t

val solver_time : float ref
val solver_count : int ref
val create : unit -> t

val interrupt : unit -> unit
(** [interrupt ()] sends interrupt signal to SMT solver *)

val clone : t -> t
(** [clone solver] makes a copy of the current [solver] *)

val add : t -> Expression.t -> unit
(** [add solver e] adds assertion [e] to [solver] *)

val check : t -> Expression.t option -> bool
(** [check solver e] *)

val fork : t -> Expression.t -> bool * bool
(** [fork solver e] checks the satisfiability of the fork on the condition [e] *)

val value_binds : t -> Symbol.t list -> (Symbol.t * Value.t) list
val string_binds : t -> (string * string * string) list

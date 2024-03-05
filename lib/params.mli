type _ param =
  | Timeout : int param
  | Model : bool param
  | Unsat_core : bool param
  | Ematching : bool param

type t

val default_value : 'a param -> 'a

val default : unit -> t

val ( $ ) : t -> 'a param * 'a -> t

val set : t -> 'a param -> 'a -> t

val get : t -> 'a param -> 'a

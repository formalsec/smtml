open Core

type t = (Symbol.t, Value.t) Hashtbl.t

val get_symbols : t -> Symbol.t List.t
val get_bindings : t -> (Symbol.t * Value.t) List.t
val evaluate : t -> Symbol.t -> Value.t Option.t
val to_string : t -> String.t

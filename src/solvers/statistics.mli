type entry =
  [ `Int of int
  | `Float of float
  ]

module Map : Map.S with type key = string

type t = entry Map.t

val sum_entries : entry -> entry -> entry

val merge : t -> t -> t

val pp_entry : entry Fmt.t

val pp : t Fmt.t

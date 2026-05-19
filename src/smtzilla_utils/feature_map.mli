type t

type feat = string

val empty : t

val union : t -> t -> t

val incr_feat : feat -> t -> t

val get_feat : feat -> t -> int

val add_time : int -> t -> t

val add_depth : int -> t -> t

val get_depth : t -> int

val add_nb_queries : int -> t -> t

val add_mean_depth : int -> t -> t

val rename_depth_to_max_depth : t -> t

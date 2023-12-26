val ( let+ ) : 'a option -> ('a -> 'b) -> 'b option

val ( let* ) :
  ('a, 'e) Result.t -> ('a -> ('b, 'e) Result.t) -> ('b, 'e) Result.t

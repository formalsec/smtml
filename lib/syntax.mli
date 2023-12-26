module Option : sig
  val ( let+ ) : 'a option -> ('a -> 'b) -> 'b option
  val ( let* ) : 'a option -> ('a -> 'b option) -> 'b option
end

module Result : sig
  val ( let+ ) : ('a, 'e) Result.t -> ('a -> 'b) -> ('b, 'e) Result.t

  val ( let* ) :
    ('a, 'e) Result.t -> ('a -> ('b, 'e) Result.t) -> ('b, 'e) Result.t

  val list_map :
    ('a -> ('b, string) Result.t) -> 'a list -> ('b list, string) Result.t
end

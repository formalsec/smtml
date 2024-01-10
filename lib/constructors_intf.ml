module type Bitv = sig
  type t
  type elt

  val v : elt -> t
  val sym : string -> t
  val not : t -> t
  val ( = ) : t -> t -> t
  val ( > ) : t -> t -> t
  val ( >= ) : t -> t -> t
  val ( < ) : t -> t -> t
  val ( <= ) : t -> t -> t
end

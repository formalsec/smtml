module type Infix = sig
  type t
  type elt

  val v : elt -> t
  val sym : string -> t
  val ( ~- ) : t -> t
  val ( = ) : t -> t -> t
  val ( != ) : t -> t -> t
  val ( > ) : t -> t -> t
  val ( >= ) : t -> t -> t
  val ( < ) : t -> t -> t
  val ( <= ) : t -> t -> t
end

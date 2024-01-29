module type Bitv_sig = sig
  type t

  type elt

  val v : elt -> t

  val sym : string -> t

  val ( ~- ) : t -> t

  val clz : t -> t

  val add : t -> t -> t

  val sub : t -> t -> t

  val mul : t -> t -> t

  val div : t -> t -> t

  val div_u : t -> t -> t

  val rem : t -> t -> t

  val rem_u : t -> t -> t

  val logand : t -> t -> t

  val logor : t -> t -> t

  val logxor : t -> t -> t

  val shl : t -> t -> t

  val shr_s : t -> t -> t

  val shr_u : t -> t -> t

  val rotl : t -> t -> t

  val rotr : t -> t -> t

  val ( = ) : t -> t -> t

  val ( != ) : t -> t -> t

  val ( > ) : t -> t -> t

  val gt_u : t -> t -> t

  val ( >= ) : t -> t -> t

  val ge_u : t -> t -> t

  val ( < ) : t -> t -> t

  val lt_u : t -> t -> t

  val ( <= ) : t -> t -> t

  val le_u : t -> t -> t

  val bits_of_float : t -> t
end

module type Fp_sig = sig
  type t

  type elt

  val v : elt -> t

  val sym : string -> t

  val abs : t -> t

  val ( ~- ) : t -> t

  val sqrt : t -> t

  val ceil : t -> t

  val floor : t -> t

  val trunc : t -> t

  val nearest : t -> t

  val add : t -> t -> t

  val sub : t -> t -> t

  val mul : t -> t -> t

  val div : t -> t -> t

  val min : t -> t -> t

  val max : t -> t -> t

  val ( = ) : t -> t -> t

  val ( != ) : t -> t -> t

  val ( > ) : t -> t -> t

  val ( >= ) : t -> t -> t

  val ( < ) : t -> t -> t

  val ( <= ) : t -> t -> t

  val float_of_bits : t -> t
end

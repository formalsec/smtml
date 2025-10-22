type t

val make : Z.t -> int -> t

val of_int8 : int -> t

val of_int16 : int -> t

val of_int32 : Int32.t -> t

val of_int64 : Int64.t -> t

val view : t -> Z.t

val to_int32 : t -> Int32.t

val to_int64 : t -> Int64.t

val numbits : t -> int

val equal : t -> t -> bool

val compare : t -> t -> int

(** Representation options for value printing. *)
type printer =
  [ `Pretty  (** Human-readable format. *)
  | `WithType  (** Print with type info. *)
  ]

(** [set_default_printer p] sets the default printer format for displaying
    values. *)
val set_default_printer : printer -> unit

val pp : t Fmt.t

val neg : t -> t

val lognot : t -> t

val clz : t -> t

val ctz : t -> t

val popcnt : t -> t

val add : t -> t -> t

val sub : t -> t -> t

val mul : t -> t -> t

val div : t -> t -> t

val div_u : t -> t -> t

val logand : t -> t -> t

val logor : t -> t -> t

val logxor : t -> t -> t

val shl : t -> t -> t

val ashr : t -> t -> t

val lshr : t -> t -> t

val rem : t -> t -> t

val rem_u : t -> t -> t

val rotate_left : t -> t -> t

val rotate_right : t -> t -> t

val lt : t -> t -> bool

val lt_u : t -> t -> bool

val gt : t -> t -> bool

val gt_u : t -> t -> bool

val le : t -> t -> bool

val le_u : t -> t -> bool

val ge : t -> t -> bool

val ge_u : t -> t -> bool

val eqz : t -> bool

val eq_one : t -> bool

val concat : t -> t -> t

val extract : t -> high:int -> low:int -> t

val zero_extend : int -> t -> t

val sign_extend : int -> t -> t

val to_json : t -> Yojson.Basic.t

val to_string : t -> string

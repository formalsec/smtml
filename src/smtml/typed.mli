type 'a ty = private Ty.t

type 'a expr = private Expr.t

type real

type bitv8

type bitv16

type bitv32

type bitv64

type bitv128

type float32

type float64

module Unsafe : sig
  val wrap : Expr.t -> 'a expr

  val unwrap : 'a expr -> Expr.t
end

val view : 'a expr -> Expr.expr

val simplify : 'a expr -> 'a expr

val symbol : 'a ty -> string -> 'a expr

val get_symbols : 'a expr list -> Symbol.t list

val ptr : int32 -> bitv32 expr -> bitv32 expr

module Types : sig
  (** [int] represents the integer type. *)
  val int : int ty

  (** [real] represents the real number type. *)
  val real : real ty

  (** [bool] represents the Boolean type. *)
  val bool : bool ty

  (** [string] represents the string type. *)
  val string : string ty

  val bitv8 : bitv8 ty

  val bitv16 : bitv16 ty

  val bitv32 : bitv32 ty

  val bitv64 : bitv64 ty

  val bitv128 : bitv128 ty

  val float32 : float32 ty

  val float64 : float64 ty

  val pp : 'a ty Fmt.t

  (** [to_ty ty] converts the type [ty] to an smtml type representation. *)
  val to_ty : 'a ty -> Ty.t
end

module Bool : sig
  type t = bool expr

  val true_ : t

  val false_ : t

  val of_bool : bool -> t

  val symbol : Symbol.t -> t

  val not : t -> t

  val and_ : t -> t -> t

  val or_ : t -> t -> t

  val logand : t list -> t

  val logor : t list -> t

  val xor : t -> t -> t

  val implies : t -> t -> t

  val eq : 'a expr -> 'a expr -> t

  val ite : t -> 'a expr -> 'a expr -> 'a expr

  val split_conjunctions : t -> Expr.Set.t

  val pp : t Fmt.t
end

module Int : sig
  type t = int expr

  val v : int -> t

  val symbol : Symbol.t -> t

  (** [neg t] constructs the negation of the integer term [t]. *)
  val neg : t -> t

  (** [add t1 t2] constructs the sum of the integer terms [t1] and [t2]. *)
  val add : t -> t -> t

  (** [sub t1 t2] constructs the difference of the integer terms [t1] and [t2].
  *)
  val sub : t -> t -> t

  (** [mul t1 t2] constructs the product of the integer terms [t1] and [t2]. *)
  val mul : t -> t -> t

  (** [div t1 t2] constructs the quotient of the integer terms [t1] and [t2]. *)
  val div : t -> t -> t

  (** [rem t1 t2] constructs the remainder of the integer terms [t1] and [t2].
  *)
  val rem : t -> t -> t

  (** [mod t1 t2] constructs the modules operator *)
  val mod_ : t -> t -> t

  (** [pow t1 t2] constructs the power of the integer terms [t1] and [t2]. *)
  val pow : t -> t -> t

  val eq : t -> t -> bool expr

  (** [lt t1 t2] constructs the less-than relation between [t1] and [t2]. *)
  val lt : t -> t -> bool expr

  (** [le t1 t2] constructs the less-than-or-equal relation between [t1] and
      [t2]. *)
  val le : t -> t -> bool expr

  (** [gt t1 t2] constructs the greater-than relation between [t1] and [t2]. *)
  val gt : t -> t -> bool expr

  (** [ge t1 t2] constructs the greater-than-or-equal relation between [t1] and
      [t2]. *)
  val ge : t -> t -> bool expr

  (** [to_real t] converts the integer term [t] to a real number int t. *)
  val to_real : t -> real expr

  val pp : t Fmt.t
end

module Real : sig
  type t = real expr

  val v : float -> t

  val symbol : Symbol.t -> t

  (** [neg t] constructs the negation of the real number term [t]. *)
  val neg : t -> t

  (** [add t1 t2] constructs the sum of the real number terms [t1] and [t2]. *)
  val add : t -> t -> t

  (** [sub t1 t2] constructs the difference of the real number terms [t1] and
      [t2]. *)
  val sub : t -> t -> t

  (** [mul t1 t2] constructs the product of the real number terms [t1] and [t2].
  *)
  val mul : t -> t -> t

  (** [div t1 t2] constructs the quotient of the real number terms [t1] and
      [t2]. *)
  val div : t -> t -> t

  (** [pow t1 t2] constructs the power of the real number terms [t1] and [t2].
  *)
  val pow : t -> t -> t

  val eq : t -> t -> bool expr

  (** [lt t1 t2] constructs the less-than relation between [t1] and [t2]. *)
  val lt : t -> t -> bool expr

  (** [le t1 t2] constructs the less-than-or-equal relation between [t1] and
      [t2]. *)
  val le : t -> t -> bool expr

  (** [gt t1 t2] constructs the greater-than relation between [t1] and [t2]. *)
  val gt : t -> t -> bool expr

  (** [ge t1 t2] constructs the greater-than-or-equal relation between [t1] and
      [t2]. *)
  val ge : t -> t -> bool expr

  (** [to_int t] converts the real number term [t] to an integer term. *)
  val to_int : t -> int expr

  val pp : t Fmt.t
end

module String : sig
  type t = string expr

  (** [v s] constructs a string term from the string [s]. *)
  val v : string -> t

  val symbol : Symbol.t -> t

  (** [length t] constructs the length of the string term [t]. *)
  val length : t -> int expr

  (** [to_code t] constructs the Unicode code point of the first character in
      the string term [t]. *)
  val to_code : t -> int expr

  (** [of_code t] constructs a string term from the Unicode code point [t]. *)
  val of_code : int expr -> t

  (** [at t ~pos] constructs the character at position [pos] in the string term
      [t]. *)
  val at : t -> pos:int expr -> t

  (** [concat ts] constructs the concatenation of a list of string terms [ts].
  *)
  val concat : t list -> t

  (** [contains t ~sub] checks if the string term [t] contains the substring
      [sub]. *)
  val contains : t -> sub:t -> bool expr

  (** [is_prefix t ~prefix] checks if the string term [t] starts with the prefix
      [prefix]. *)
  val is_prefix : t -> prefix:t -> bool expr

  (** [is_suffix t ~suffix] checks if the string term [t] ends with the suffix
      [suffix]. *)
  val is_suffix : t -> suffix:t -> bool expr

  val eq : t -> t -> bool expr

  (** [lt t1 t2] constructs the less-than relation between string terms [t1] and
      [t2]. *)
  val lt : t -> t -> bool expr

  (** [le t1 t2] constructs the less-than-or-equal relation between string terms
      [t1] and [t2]. *)
  val le : t -> t -> bool expr

  (** [sub t ~pos ~len] constructs the substring of [t] starting at [pos] with
      length [len]. *)
  val sub : t -> pos:int expr -> len:int expr -> t

  (** [index_of t ~sub ~pos] constructs the index of the first occurrence of
      [sub] in [t] starting at [pos]. *)
  val index_of : t -> sub:t -> pos:int expr -> int expr

  (** [replace t ~pattern ~with_] constructs the string term resulting from
      replacing [pattern] with [with_] in [t]. *)
  val replace : t -> pattern:t -> with_:t -> t

  (** [replace_all t ~pattern ~with_] constructs the string term resulting from
      replacing all occurrences of [pattern] with [with_] in [t]. *)
  val replace_all : t -> pattern:t -> with_:t -> t

  (** [to_int t] converts the string term [t] to an integer term. *)
  val to_int : t -> int expr

  (** [of_int t] converts the integer term [t] to a string term. *)
  val of_int : int expr -> t

  val pp : t Fmt.t
end

module Bitv : sig
  module type Width = sig
    type w

    val ty : Ty.t
  end

  module type S = sig
    type w

    type t = w expr

    val ty : w ty

    val zero : t

    val one : t

    val v : Bitvector.t -> t

    val of_int : int -> t

    val symbol : Symbol.t -> t

    val clz : t -> t

    val ctz : t -> t

    val popcnt : t -> t

    val neg : t -> t

    val lognot : t -> t

    val to_int : signed:bool -> int expr -> t

    val add : t -> t -> t

    val sub : t -> t -> t

    val mul : t -> t -> t

    val div : t -> t -> t

    val unsigned_div : t -> t -> t

    val logor : t -> t -> t

    val logand : t -> t -> t

    val logxor : t -> t -> t

    val shl : t -> t -> t

    val ashr : t -> t -> t

    val lshr : t -> t -> t

    val rem : t -> t -> t

    val unsigned_rem : t -> t -> t

    val rotate_left : t -> t -> t

    val rotate_right : t -> t -> t

    val eq : t -> t -> bool expr

    val ne : t -> t -> bool expr

    val lt : t -> t -> bool expr

    val lt_u : t -> t -> bool expr

    val le : t -> t -> bool expr

    val le_u : t -> t -> bool expr

    val gt : t -> t -> bool expr

    val gt_u : t -> t -> bool expr

    val ge : t -> t -> bool expr

    val ge_u : t -> t -> bool expr

    val concat : 'a expr -> 'b expr -> 'c expr

    val extract : t -> high:int -> low:int -> 'a expr

    val zero_extend : int -> t -> 'a expr

    val sign_extend : int -> t -> 'a expr

    val to_bool : t -> bool expr

    val of_bool : bool expr -> t

    val pp : t Fmt.t
  end

  module Make (W : Width) : S with type w = W.w
end

module Bitv8 : Bitv.S with type w = bitv8

module Bitv16 : Bitv.S with type w = bitv16

module Bitv32 : sig
  include Bitv.S with type w = bitv32

  val of_int32 : Int32.t -> t

  val of_int8_s : bitv8 expr -> t

  val of_int8_u : bitv8 expr -> t

  val of_int16_s : bitv16 expr -> t

  val of_int16_u : bitv16 expr -> t

  val to_bytes : t -> bitv8 expr list

  val trunc_f32_s_exn : float32 expr -> t

  val trunc_f32_u_exn : float32 expr -> t

  val trunc_f64_s_exn : float64 expr -> t

  val trunc_f64_u_exn : float64 expr -> t

  val trunc_f32_s :
    float32 expr -> (t, [> `Integer_overflow | `Conversion_to_integer ]) result

  val trunc_f32_u :
    float32 expr -> (t, [> `Integer_overflow | `Conversion_to_integer ]) result

  val trunc_f64_s :
    float64 expr -> (t, [> `Integer_overflow | `Conversion_to_integer ]) result

  val trunc_f64_u :
    float64 expr -> (t, [> `Integer_overflow | `Conversion_to_integer ]) result

  val trunc_sat_f32_s : float32 expr -> t

  val trunc_sat_f32_u : float32 expr -> t

  val trunc_sat_f64_s : float64 expr -> t

  val trunc_sat_f64_u : float64 expr -> t

  val reinterpret_f32 : float32 expr -> t

  val wrap_i64 : bitv64 expr -> t

  val extend_s : int -> t -> t
end

module Bitv64 : sig
  include Bitv.S with type w = bitv64

  val of_int64 : Int64.t -> t

  val of_int32 : bitv32 expr -> t

  val to_int32 : t -> bitv32 expr

  val to_bytes : t -> bitv8 expr list

  val trunc_f32_s_exn : float32 expr -> t

  val trunc_f32_u_exn : float32 expr -> t

  val trunc_f64_s_exn : float64 expr -> t

  val trunc_f64_u_exn : float64 expr -> t

  val trunc_f32_s :
    float32 expr -> (t, [> `Integer_overflow | `Conversion_to_integer ]) result

  val trunc_f32_u :
    float32 expr -> (t, [> `Integer_overflow | `Conversion_to_integer ]) result

  val trunc_f64_s :
    float64 expr -> (t, [> `Integer_overflow | `Conversion_to_integer ]) result

  val trunc_f64_u :
    float64 expr -> (t, [> `Integer_overflow | `Conversion_to_integer ]) result

  val trunc_sat_f32_s : float32 expr -> t

  val trunc_sat_f32_u : float32 expr -> t

  val trunc_sat_f64_s : float64 expr -> t

  val trunc_sat_f64_u : float64 expr -> t

  val reinterpret_f64 : float64 expr -> t

  val extend_s : int -> t -> t

  val extend_i32_s : bitv32 expr -> t

  val extend_i32_u : bitv32 expr -> t
end

module Bitv128 : sig
  include Bitv.S with type w = bitv128

  val of_i32x4 : bitv32 expr -> bitv32 expr -> bitv32 expr -> bitv32 expr -> t

  val to_i32x4 : t -> bitv32 expr * bitv32 expr * bitv32 expr * bitv32 expr

  val of_int64x2 : Int64.t -> Int64.t -> t

  val of_i64x2 : bitv64 expr -> bitv64 expr -> t

  val to_i64x2 : t -> bitv64 expr * bitv64 expr
end

module Float32 : sig
  type t = float32 expr

  val zero : t

  val v : int32 -> t

  val of_float : float -> t

  (** alias of [v] *)
  val of_int32_bits : int32 -> t

  val symbol : Symbol.t -> t

  (** [neg t] constructs the negation of the floating-point term [t]. *)
  val neg : t -> t

  (** [abs t] constructs the absolute value of the floating-point term [t]. *)
  val abs : t -> t

  (** [sqrt ~rm t] constructs the square root of the floating-point term [t]
      using the rounding mode [rm]. *)
  val sqrt : t -> t

  val is_normal : t -> bool expr

  val is_subnormal : t -> bool expr

  val is_negative : t -> bool expr

  val is_positive : t -> bool expr

  val is_infinite : t -> bool expr

  val is_zero : t -> bool expr

  (** [is_nan t] checks if the floating-point term [t] is NaN. *)
  val is_nan : t -> bool expr

  val ceil : t -> t

  val floor : t -> t

  val trunc : t -> t

  val nearest : t -> t

  (** [add ~rm t1 t2] constructs the sum of the floating-point terms [t1] and
      [t2] using the rounding mode [rm]. *)
  val add : t -> t -> t

  (** [sub ~rm t1 t2] constructs the difference of the floating-point terms [t1]
      and [t2] using the rounding mode [rm]. *)
  val sub : t -> t -> t

  (** [mul ~rm t1 t2] constructs the product of the floating-point terms [t1]
      and [t2] using the rounding mode [rm]. *)
  val mul : t -> t -> t

  (** [div ~rm t1 t2] constructs the quotient of the floating-point terms [t1]
      and [t2] using the rounding mode [rm]. *)
  val div : t -> t -> t

  (** [min t1 t2] constructs the minimum of the floating-point terms [t1] and
      [t2]. *)
  val min : t -> t -> t

  (** [max t1 t2] constructs the maximum of the floating-point terms [t1] and
      [t2]. *)
  val max : t -> t -> t

  (** [rem t1 t2] constructs the remainder of the floating-point terms [t1] and
      [t2]. *)
  val rem : t -> t -> t

  val copy_sign : t -> t -> t

  (** [eq t1 t2] constructs the equality of the floating-point terms [t1] and
      [t2]. *)
  val eq : t -> t -> bool expr

  val ne : t -> t -> bool expr

  (** [lt t1 t2] constructs the less-than relation between floating-point terms
      [t1] and [t2]. *)
  val lt : t -> t -> bool expr

  (** [le t1 t2] constructs the less-than-or-equal relation between
      floating-point terms [t1] and [t2]. *)
  val le : t -> t -> bool expr

  (** [gt t1 t2] constructs the greater-than relation between floating-point
      terms [t1] and [t2]. *)
  val gt : t -> t -> bool expr

  (** [ge t1 t2] constructs the greater-than-or-equal relation between
      floating-point terms [t1] and [t2]. *)
  val ge : t -> t -> bool expr

  val convert_i32_s : bitv32 expr -> t

  val convert_i32_u : bitv32 expr -> t

  val convert_i64_s : bitv64 expr -> t

  val convert_i64_u : bitv64 expr -> t

  val demote_f64 : float64 expr -> t

  val reinterpret_i32 : bitv32 expr -> t

  val to_bv : t -> bitv32 expr

  val pp : t Fmt.t
end

module Float64 : sig
  type t = float64 expr

  val zero : t

  (** [v f e s] constructs a floating-point term from the float [f] with
      exponent width [e] and significand width [s]. *)
  val v : int64 -> t

  val of_float : float -> t

  val symbol : Symbol.t -> t

  (** [neg t] constructs the negation of the floating-point term [t]. *)
  val neg : t -> t

  (** [abs t] constructs the absolute value of the floating-point term [t]. *)
  val abs : t -> t

  (** [sqrt ~rm t] constructs the square root of the floating-point term [t]
      using the rounding mode [rm]. *)
  val sqrt : t -> t

  val is_normal : t -> bool expr

  val is_subnormal : t -> bool expr

  val is_negative : t -> bool expr

  val is_positive : t -> bool expr

  val is_infinite : t -> bool expr

  val is_zero : t -> bool expr

  (** [is_nan t] checks if the floating-point term [t] is NaN. *)
  val is_nan : t -> bool expr

  val ceil : t -> t

  val floor : t -> t

  val trunc : t -> t

  val nearest : t -> t

  (** [add ~rm t1 t2] constructs the sum of the floating-point terms [t1] and
      [t2] using the rounding mode [rm]. *)
  val add : t -> t -> t

  (** [sub ~rm t1 t2] constructs the difference of the floating-point terms [t1]
      and [t2] using the rounding mode [rm]. *)
  val sub : t -> t -> t

  (** [mul ~rm t1 t2] constructs the product of the floating-point terms [t1]
      and [t2] using the rounding mode [rm]. *)
  val mul : t -> t -> t

  (** [div ~rm t1 t2] constructs the quotient of the floating-point terms [t1]
      and [t2] using the rounding mode [rm]. *)
  val div : t -> t -> t

  (** [min t1 t2] constructs the minimum of the floating-point terms [t1] and
      [t2]. *)
  val min : t -> t -> t

  (** [max t1 t2] constructs the maximum of the floating-point terms [t1] and
      [t2]. *)
  val max : t -> t -> t

  (** [rem t1 t2] constructs the remainder of the floating-point terms [t1] and
      [t2]. *)
  val rem : t -> t -> t

  val copy_sign : t -> t -> t

  (** [eq t1 t2] constructs the equality of the floating-point terms [t1] and
      [t2]. *)
  val eq : t -> t -> bool expr

  val ne : t -> t -> bool expr

  (** [lt t1 t2] constructs the less-than relation between floating-point terms
      [t1] and [t2]. *)
  val lt : t -> t -> bool expr

  (** [le t1 t2] constructs the less-than-or-equal relation between
      floating-point terms [t1] and [t2]. *)
  val le : t -> t -> bool expr

  (** [gt t1 t2] constructs the greater-than relation between floating-point
      terms [t1] and [t2]. *)
  val gt : t -> t -> bool expr

  (** [ge t1 t2] constructs the greater-than-or-equal relation between
      floating-point terms [t1] and [t2]. *)
  val ge : t -> t -> bool expr

  val convert_i32_s : bitv32 expr -> t

  val convert_i32_u : bitv32 expr -> t

  val convert_i64_s : bitv64 expr -> t

  val convert_i64_u : bitv64 expr -> t

  val promote_f32 : float32 expr -> t

  val reinterpret_i64 : bitv64 expr -> t

  val to_bv : t -> bitv64 expr

  val pp : t Fmt.t
end

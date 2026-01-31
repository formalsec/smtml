(** The type of a type witness. *)
type 'a ty = private Ty.t

(** The type of a typed expression (term). *)
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
  (** [wrap e] promotes a raw untyped expression [e] to a typed expression.
      {b Warning.} This is unsafe because it bypasses the type checker. *)
  val wrap : Expr.t -> 'a expr

  (** [unwrap e] extracts the raw untyped expression from a typed expression
      [e]. *)
  val unwrap : 'a expr -> Expr.t
end

(** [view e] typed alias for {!Expr.view}. Allows retrieving the underlying
    structure of the expression [e]. *)
val view : 'a expr -> Expr.expr

(** [simplify e] typed alias for {!Expr.simplify}. Performs algebraic
    simplifications on the expression [e]. *)
val simplify : 'a expr -> 'a expr

(** [symbol ty name] creates a symbolic constant of type [ty] with the given
    [name]. *)
val symbol : 'a ty -> string -> 'a expr

(** [get_symbols es] returns a list of all unique symbols appearing in the list
    of expressions [es]. *)
val get_symbols : 'a expr list -> Symbol.t list

(** [ptr value base] creates a 32-bit pointer expression by adding [value] to
    the [base] address. *)
val ptr : int32 -> bitv32 expr -> bitv32 expr

module Types : sig
  (** [int] represents the integer type. *)
  val int : int ty

  (** [real] represents the real number type. *)
  val real : real ty

  (** [bool] represents the bool type. *)
  val bool : bool ty

  (** [string] represents the string type. *)
  val string : string ty

  (** [bitv8] represents the type of a bitvector with 8-bits. *)
  val bitv8 : bitv8 ty

  (** [bitv16] represents the type of a bitvector with 16-bits. *)
  val bitv16 : bitv16 ty

  (** [bitv32] represents the type of a bitvector with 32-bits. *)
  val bitv32 : bitv32 ty

  (** [bitv64] represents the type of a bitvector with 64-bits. *)
  val bitv64 : bitv64 ty

  (** [bitv128] represents the type of a bitvector with 128-bits. *)
  val bitv128 : bitv128 ty

  (** [float32] represents the type of a float with 32-bits. *)
  val float32 : float32 ty

  (** [float64] represents the type of a float with 64-bits. *)
  val float64 : float64 ty

  val pp : 'a ty Fmt.t

  (** [to_ty ty] converts the type [ty] to an smtml type representation. *)
  val to_ty : 'a ty -> Ty.t
end

module Bool : sig
  type t = bool expr

  (** The boolean literal [true]. *)
  val true_ : t

  (** The boolean literal [false]. *)
  val false_ : t

  val of_bool : bool -> t

  val symbol : Symbol.t -> t

  (** [not t] is the logical negation of [t]. *)
  val not : t -> t

  (** [and_ t1 t2] is the logical conjunction of [t1] and [t2]. *)
  val and_ : t -> t -> t

  (** [or_ t1 t2] is the logical disjunction of [t1] and [t2]. *)
  val or_ : t -> t -> t

  val logand : t list -> t

  val logor : t list -> t

  (** [xor t1 t2] is the exclusive-or of [t1] and [t2]. *)
  val xor : t -> t -> t

  (** [implies t1 t2] constructs the implication [t1 => t2]. *)
  val implies : t -> t -> t

  (** [eq t1 t2] returns true if [t1] and [t2] are structurally equal. *)
  val eq : 'a expr -> 'a expr -> t

  (** [ite cond then_ else_] constructs an if-then-else expression. Returns
      [then_] if [cond] evaluates to true, and [else_] otherwise. *)
  val ite : t -> 'a expr -> 'a expr -> 'a expr

  (** [split_conjunctions t] breaks a conjunction term into a set of its
      top-level conjuncts. *)
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

  (** Alias for {!Bool.eq}. *)
  val eq : t -> t -> bool expr

  (** [lt t1 t2] constructs the less-than relation between [t1] and [t2]. *)
  val lt : t -> t -> bool expr

  (** [le t1 t2] constructs the less-than-or-equal relation between [t1] and
      [t2]. *)
  val le : t -> t -> bool expr

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

  (** Alias for {!Bool.eq}. *)
  val eq : t -> t -> bool expr

  (** [lt t1 t2] constructs the less-than relation between [t1] and [t2]. *)
  val lt : t -> t -> bool expr

  (** [le t1 t2] constructs the less-than-or-equal relation between [t1] and
      [t2]. *)
  val le : t -> t -> bool expr

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

  (** Alias for {!Bool.eq}. *)
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

    (** The bitvector value 0. *)
    val zero : t

    (** The bitvector value 1. *)
    val one : t

    val v : Bitvector.t -> t

    (** [of_int i] creates a bitvector term from an integer [i]. *)
    val of_int : int -> t

    val symbol : Symbol.t -> t

    (** [clz t] counts the leading zeros in [t]. *)
    val clz : t -> t

    (** [ctz t] counts the trailing zeros in [t]. *)
    val ctz : t -> t

    (** [popcnt t] counts the number of set bits (population count) in [t]. *)
    val popcnt : t -> t

    (** [neg t] computes the two's complement negation of [t]. *)
    val neg : t -> t

    (** [lognot t] computes the bitwise NOT of [t]. *)
    val lognot : t -> t

    (** [to_int ~signed t] converts the bitvector [t] to an integer term. If
        [signed] is true, [t] is treated as a 2's complement signed value. *)
    val to_int : signed:bool -> int expr -> t

    (** [add t1 t2] computes the bitwise addition [t1 + t2]. *)
    val add : t -> t -> t

    (** [sub t1 t2] computes the bitwise subtraction [t1 - t2]. *)
    val sub : t -> t -> t

    (** [mul t1 t2] computes the bitwise multiplication [t1 * t2]. *)
    val mul : t -> t -> t

    (** [div t1 t2] is integer division. *)
    val div : t -> t -> t

    (** [unsigned_div t1 t2] is explicitly unsigned integer division. *)
    val unsigned_div : t -> t -> t

    val logor : t -> t -> t

    val logand : t -> t -> t

    val logxor : t -> t -> t

    (** [shl t amount] shifts [t] left by [amount] bits. *)
    val shl : t -> t -> t

    (** [ashr t amount] performs an arithmetic shift right (preserving sign). *)
    val ashr : t -> t -> t

    (** [lshr t amount] performs a logical shift right (inserting zeros). *)
    val lshr : t -> t -> t

    (** [rem t1 t2] computes the signed remainder of [t1 / t2]. *)
    val rem : t -> t -> t

    (** [unsigned_rem t1 t2] computes the unsigned remainder of [t1 / t2]. *)
    val unsigned_rem : t -> t -> t

    (** [rotate_left t amount] rotates the bits of [t] to the left by [amount].
    *)
    val rotate_left : t -> t -> t

    (** [rotate_right t amount] rotates the bits of [t] to the right by
        [amount]. *)
    val rotate_right : t -> t -> t

    (** Alias for {!Bool.eq}. *)
    val eq : t -> t -> bool expr

    (** Negation of {!eq}. *)
    val ne : t -> t -> bool expr

    (** [lt t1 t2] is signed less-than. *)
    val lt : t -> t -> bool expr

    (** [lt_u t1 t2] is unsigned less-than. *)
    val lt_u : t -> t -> bool expr

    (** [le t1 t2] is signed less-than-or-equal. *)
    val le : t -> t -> bool expr

    (** [le t1 t2] is unsigned less-than-or-equal. *)
    val le_u : t -> t -> bool expr

    (** [concat t1 t2] concatenates [t1] (high bits) and [t2] (low bits).

        Example: [concat (v8 0xAA) (v8 0xBB)] results in [0xAABB] (16-bit). *)
    val concat : 'a expr -> 'b expr -> 'c expr

    (** [extract t ~high ~low] extracts the bytes from index [high] down to
        [low] (inclusive).

        Example: [extract (i32 0xAABBCCDD) ~high:2 ~low:1] results in [0xCC]
        (1-byte). *)
    val extract : t -> high:int -> low:int -> 'a expr

    (** [zero_extend n t] extends [t] to a width of [width(t) + n] by padding
        with zeros. *)
    val zero_extend : int -> t -> 'a expr

    (** [sign_extend n t] extends [t] to a width of [width(t) + n] by
        replicating the sign bit. *)
    val sign_extend : int -> t -> 'a expr

    (** [to_bool t] returns true if [t] is non-zero, false otherwise. *)
    val to_bool : t -> bool expr

    (** [of_bool b] converts true to 1 and false to 0. *)
    val of_bool : bool expr -> t

    val pp : t Fmt.t
  end

  module Make (W : Width) : S with type w = W.w
end

module Bitv8 : Bitv.S with type w = bitv8

module Bitv16 : Bitv.S with type w = bitv16

module Bitv32 : sig
  include Bitv.S with type w = bitv32

  (** [of_int32 i] creates a 32-bit vector from an OCaml [Int32.t]. *)
  val of_int32 : Int32.t -> t

  (** [of_int8_s t] sign-extends an 8-bit vector to 32 bits. *)
  val of_int8_s : bitv8 expr -> t

  (** [of_int8_u t] zero-extends an 8-bit vector to 32 bits. *)
  val of_int8_u : bitv8 expr -> t

  (** [of_int16_s t] sign-extends a 16-bit vector to 32 bits. *)
  val of_int16_s : bitv16 expr -> t

  (** [of_int16_u t] zero-extends a 16-bit vector to 32 bits. *)
  val of_int16_u : bitv16 expr -> t

  (** [to_bytes t] splits the 32-bit vector into 4 bytes (little-endian). *)
  val to_bytes : t -> bitv8 expr list

  (** Truncate float to signed integer (raises exception on overflow/NaN). *)

  (** [trunc_f32_s_exn f] truncates a float32 to a signed integer. Raises
      {!Eval.Eval_error} exception on failure. *)
  val trunc_f32_s_exn : float32 expr -> t

  (** [trunc_f32_u_exn f] truncates a float32 to an unsigned integer. Raises
      {!Eval.Eval_error} exception on failure. *)
  val trunc_f32_u_exn : float32 expr -> t

  (** [trunc_f64_s_exn f] truncates a float64 to a signed integer. Raises
      {!Eval.Eval_error} exception on failure. *)
  val trunc_f64_s_exn : float64 expr -> t

  (** [trunc_f64_u_exn f] truncates a float64 to an unsigned integer. Raises
      {!Eval.Eval_error} exception on failure. *)
  val trunc_f64_u_exn : float64 expr -> t

  (** Truncate float to signed integer (returns result/error). *)

  (** [trunc_f32_s f] attempts to truncate a float32 to a signed integer. *)
  val trunc_f32_s :
    float32 expr -> (t, [> `Integer_overflow | `Conversion_to_integer ]) result

  (** [trunc_f32_u f] attempts to truncate a float32 to an unsigned integer. *)
  val trunc_f32_u :
    float32 expr -> (t, [> `Integer_overflow | `Conversion_to_integer ]) result

  (** [trunc_f64_s f] attempts to truncate a float64 to a signed integer. *)
  val trunc_f64_s :
    float64 expr -> (t, [> `Integer_overflow | `Conversion_to_integer ]) result

  (** [trunc_f64_u f] attempts to truncate a float64 to an unsigned integer. *)
  val trunc_f64_u :
    float64 expr -> (t, [> `Integer_overflow | `Conversion_to_integer ]) result

  (** Truncate with saturation (clamps to min/max on overflow, 0 on NaN). *)

  (** [trunc_sat_f32_s f] truncates a float32 to signed integer with saturation.
      (Clamps to min/max on overflow, 0 on NaN). *)
  val trunc_sat_f32_s : float32 expr -> t

  (** [trunc_sat_f32_u f] truncates a float32 to unsigned integer with
      saturation. (Clamps to min/max on overflow, 0 on NaN). *)
  val trunc_sat_f32_u : float32 expr -> t

  (** [trunc_sat_f64_s f] truncates a float64 to signed integer with saturation.
      (Clamps to min/max on overflow, 0 on NaN). *)
  val trunc_sat_f64_s : float64 expr -> t

  (** [trunc_sat_f64_u f] truncates a float64 to unsigned integer with
      saturation. (Clamps to min/max on overflow, 0 on NaN). *)
  val trunc_sat_f64_u : float64 expr -> t

  (** [reinterpret_f32 t] interprets the bits of a float32 as a bitv32. *)
  val reinterpret_f32 : float32 expr -> t

  (** [wrap_i64 t] discards the upper 32 bits of a 64-bit integer [t]. *)
  val wrap_i64 : bitv64 expr -> t

  (** [extend_s n t] sign-extends [t] by [n] bits. *)
  val extend_s : int -> t -> t
end

module Bitv64 : sig
  include Bitv.S with type w = bitv64

  (** [of_int64 i] creates a 64-bit vector from an OCaml [Int64.t]. *)
  val of_int64 : Int64.t -> t

  (** [of_int32 t] zero-extends a 32-bit vector to 64 bits. *)
  val of_int32 : bitv32 expr -> t

  (** [to_int32 t] extracts the lower 32 bits of [t]. *)
  val to_int32 : t -> bitv32 expr

  (** [to_bytes t] splits the 32-bit vector into 4 bytes (little-endian). *)
  val to_bytes : t -> bitv8 expr list

  (** Truncate float to signed integer (raises exception on overflow/NaN). *)

  (** [trunc_f32_s_exn f] truncates a float32 to a signed integer. Raises
      {!Eval.Eval_error} exception on failure. *)
  val trunc_f32_s_exn : float32 expr -> t

  (** [trunc_f32_u_exn f] truncates a float32 to an unsigned integer. Raises
      {!Eval.Eval_error} exception on failure. *)
  val trunc_f32_u_exn : float32 expr -> t

  (** [trunc_f64_s_exn f] truncates a float64 to a signed integer. Raises
      {!Eval.Eval_error} exception on failure. *)
  val trunc_f64_s_exn : float64 expr -> t

  (** [trunc_f64_u_exn f] truncates a float64 to an unsigned integer. Raises
      {!Eval.Eval_error} exception on failure. *)
  val trunc_f64_u_exn : float64 expr -> t

  (** Truncate float to signed integer (returns result/error). *)

  (** [trunc_f32_s f] attempts to truncate a float32 to a signed integer. *)
  val trunc_f32_s :
    float32 expr -> (t, [> `Integer_overflow | `Conversion_to_integer ]) result

  (** [trunc_f32_u f] attempts to truncate a float32 to an unsigned integer. *)
  val trunc_f32_u :
    float32 expr -> (t, [> `Integer_overflow | `Conversion_to_integer ]) result

  (** [trunc_f64_s f] attempts to truncate a float64 to a signed integer. *)
  val trunc_f64_s :
    float64 expr -> (t, [> `Integer_overflow | `Conversion_to_integer ]) result

  (** [trunc_f64_u f] attempts to truncate a float64 to an unsigned integer. *)
  val trunc_f64_u :
    float64 expr -> (t, [> `Integer_overflow | `Conversion_to_integer ]) result

  (** Truncate with saturation (clamps to min/max on overflow, 0 on NaN). *)

  (** [trunc_sat_f32_s f] truncates a float32 to signed integer with saturation.
      (Clamps to min/max on overflow, 0 on NaN). *)
  val trunc_sat_f32_s : float32 expr -> t

  (** [trunc_sat_f32_u f] truncates a float32 to unsigned integer with
      saturation. (Clamps to min/max on overflow, 0 on NaN). *)
  val trunc_sat_f32_u : float32 expr -> t

  (** [trunc_sat_f64_s f] truncates a float64 to signed integer with saturation.
      (Clamps to min/max on overflow, 0 on NaN). *)
  val trunc_sat_f64_s : float64 expr -> t

  (** [trunc_sat_f64_u f] truncates a float64 to unsigned integer with
      saturation. (Clamps to min/max on overflow, 0 on NaN). *)
  val trunc_sat_f64_u : float64 expr -> t

  (** [reinterpret_f64 f] reinterprets the raw bits of [f] as a 64-bit integer.
  *)
  val reinterpret_f64 : float64 expr -> t

  val extend_s : int -> t -> t

  (** [extend_i32_s t] sign-extends a 32-bit vector to 64 bits. *)
  val extend_i32_s : bitv32 expr -> t

  (** [extend_i32_u t] zero-extends a 32-bit vector to 64 bits. *)
  val extend_i32_u : bitv32 expr -> t
end

module Bitv128 : sig
  include Bitv.S with type w = bitv128

  (** [of_i32x4 a b c d] constructs a 128-bit vector from four 32-bit lanes. *)
  val of_i32x4 : bitv32 expr -> bitv32 expr -> bitv32 expr -> bitv32 expr -> t

  (** [to_i32x4 t] splits the 128-bit vector into four 32-bit lanes. *)
  val to_i32x4 : t -> bitv32 expr * bitv32 expr * bitv32 expr * bitv32 expr

  (** [of_int64x2 high low] constructs a 128-bit vector from two 64-bit
      integers. *)
  val of_int64x2 : Int64.t -> Int64.t -> t

  (** [of_i64x2 a b] constructs a 128-bit vector from two 64-bit lanes. *)
  val of_i64x2 : bitv64 expr -> bitv64 expr -> t

  (** [to_i64x2 t] splits the 128-bit vector into two 64-bit terms. *)
  val to_i64x2 : t -> bitv64 expr * bitv64 expr
end

module Float32 : sig
  type t = float32 expr

  val zero : t

  (** [v bits] creates a float32 from the raw integer bits. *)
  val v : int32 -> t

  (** [of_float f] creates a float32 from an OCaml float value. *)
  val of_float : float -> t

  (** [of_int32_bits bits] is an alias for {!v}. *)
  val of_int32_bits : int32 -> t

  val symbol : Symbol.t -> t

  (** [neg t] constructs the negation of the floating-point term [t]. *)
  val neg : t -> t

  (** [abs t] constructs the absolute value of the floating-point term [t]. *)
  val abs : t -> t

  (** [sqrt t] constructs the square root of the floating-point term [t]. *)
  val sqrt : t -> t

  (** [is_normal t] checks if [t] is a normal floating point number (not zero,
      subnormal, infinite, or NaN). *)
  val is_normal : t -> bool expr

  (** [is_subnormal t] checks if [t] is a subnormal (denormal) number. *)
  val is_subnormal : t -> bool expr

  (** [is_negative t] checks if the sign bit of [t] is set. *)
  val is_negative : t -> bool expr

  (** [is_positive t] checks if the sign bit of [t] is unset. *)
  val is_positive : t -> bool expr

  (** [is_infinite t] checks if [t] is +Inf or -Inf. *)
  val is_infinite : t -> bool expr

  (** [is_zero t] checks if [t] is +0.0 or -0.0. *)
  val is_zero : t -> bool expr

  (** [is_nan t] checks if [t] is Not-a-Number (NaN). *)
  val is_nan : t -> bool expr

  (** [ceil t] rounds [t] to the nearest integer towards positive infinity. *)
  val ceil : t -> t

  (** [floor t] rounds [t] to the nearest integer towards negative infinity. *)
  val floor : t -> t

  (** [trunc t] rounds [t] to the nearest integer towards zero. *)
  val trunc : t -> t

  (** [nearest t] rounds [t] to the nearest integer (ties to even). *)
  val nearest : t -> t

  (** [add t1 t2] computes [t1 + t2]. *)
  val add : t -> t -> t

  (** [sub t1 t2] computes [t1 - t2]. *)
  val sub : t -> t -> t

  (** [mul t1 t2] computes [t1 * t2]. *)
  val mul : t -> t -> t

  (** [div t1 t2] computes [t1 / t2]. *)
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

  (** [copy_sign x y] returns [x] with the sign of [y]. *)
  val copy_sign : t -> t -> t

  (** [eq t1 t2] performs IEEE-754 equality comparison. *)
  val eq : t -> t -> bool expr

  (** [ne t1 t2] performs IEEE-754 inequality comparison. *)
  val ne : t -> t -> bool expr

  (** [lt t1 t2] performs IEEE-754 less-than comparison. *)
  val lt : t -> t -> bool expr

  (** [le t1 t2] performs IEEE-754 less-than-or-equal comparison. *)
  val le : t -> t -> bool expr

  (** [convert_i32_s t] converts a signed 32-bit integer to float32. *)
  val convert_i32_s : bitv32 expr -> t

  (** [convert_i32_u t] converts an unsigned 32-bit integer to float32. *)
  val convert_i32_u : bitv32 expr -> t

  (** [convert_i64_s t] converts a signed 64-bit integer to float32. *)
  val convert_i64_s : bitv64 expr -> t

  (** [convert_i64_u t] converts an unsigned 64-bit integer to float32. *)
  val convert_i64_u : bitv64 expr -> t

  (** [demote_f64 t] converts a float64 to float32 (may lose precision). *)
  val demote_f64 : float64 expr -> t

  (** [reinterpret_i32 i] reinterprets the bits of a 32-bit integer as a
      float32. *)
  val reinterpret_i32 : bitv32 expr -> t

  (** [to_bv t] reinterprets the float32 [t] as a raw 32-bit bitvector. *)
  val to_bv : t -> bitv32 expr

  val pp : t Fmt.t
end

module Float64 : sig
  type t = float64 expr

  val zero : t

  (** [v bits] creates a float64 from the raw integer bits. *)
  val v : int64 -> t

  (** [of_float f] creates a float64 from an OCaml float value. *)
  val of_float : float -> t

  val symbol : Symbol.t -> t

  (** [neg t] constructs the negation of the floating-point term [t]. *)
  val neg : t -> t

  (** [abs t] constructs the absolute value of the floating-point term [t]. *)
  val abs : t -> t

  (** [sqrt t] constructs the square root of the floating-point term [t]. *)
  val sqrt : t -> t

  (** [is_normal t] checks if [t] is a normal floating point number (not zero,
      subnormal, infinite, or NaN). *)
  val is_normal : t -> bool expr

  (** [is_subnormal t] checks if [t] is a subnormal (denormal) number. *)
  val is_subnormal : t -> bool expr

  (** [is_negative t] checks if the sign bit of [t] is set. *)
  val is_negative : t -> bool expr

  (** [is_positive t] checks if the sign bit of [t] is unset. *)
  val is_positive : t -> bool expr

  (** [is_infinite t] checks if [t] is +Inf or -Inf. *)
  val is_infinite : t -> bool expr

  (** [is_zero t] checks if [t] is +0.0 or -0.0. *)
  val is_zero : t -> bool expr

  (** [is_nan t] checks if [t] is Not-a-Number (NaN). *)
  val is_nan : t -> bool expr

  (** [ceil t] rounds [t] to the nearest integer towards positive infinity. *)
  val ceil : t -> t

  (** [floor t] rounds [t] to the nearest integer towards negative infinity. *)
  val floor : t -> t

  (** [trunc t] rounds [t] to the nearest integer towards zero. *)
  val trunc : t -> t

  (** [nearest t] rounds [t] to the nearest integer (ties to even). *)
  val nearest : t -> t

  (** [add t1 t2] computes [t1 + t2]. *)
  val add : t -> t -> t

  (** [sub t1 t2] computes [t1 - t2]. *)
  val sub : t -> t -> t

  (** [mul t1 t2] computes [t1 * t2]. *)
  val mul : t -> t -> t

  (** [div t1 t2] computes [t1 / t2]. *)
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

  (** [copy_sign x y] returns [x] with the sign of [y]. *)
  val copy_sign : t -> t -> t

  (** [eq t1 t2] performs IEEE-754 equality comparison. *)
  val eq : t -> t -> bool expr

  (** [ne t1 t2] performs IEEE-754 inequality comparison. *)
  val ne : t -> t -> bool expr

  (** [lt t1 t2] performs IEEE-754 less-than comparison. *)
  val lt : t -> t -> bool expr

  (** [le t1 t2] performs IEEE-754 less-than-or-equal comparison. *)
  val le : t -> t -> bool expr

  (** [convert_i32_s t] converts a signed 32-bit integer to float64. *)
  val convert_i32_s : bitv32 expr -> t

  (** [convert_i32_u t] converts an unsigned 32-bit integer to float64. *)
  val convert_i32_u : bitv32 expr -> t

  (** [convert_i64_s t] converts a signed 64-bit integer to float64. *)
  val convert_i64_s : bitv64 expr -> t

  (** [convert_i64_u t] converts an unsigned 64-bit integer to float64. *)
  val convert_i64_u : bitv64 expr -> t

  (** [promote_f32 t] converts a float32 to float64 (exact conversion). *)
  val promote_f32 : float32 expr -> t

  (** [reinterpret_i64 i] reinterprets the bits of a 64-bit integer as a
      float64. *)
  val reinterpret_i64 : bitv64 expr -> t

  (** [to_bv t] reinterprets the float64 [t] as a raw 64-bit bitvector. *)
  val to_bv : t -> bitv64 expr

  val pp : t Fmt.t
end

(** Typed function builders for SMT usage.

    This module provides a GADT-based combinator library to define SMT functions
    with OCaml-native typing. It uses right-associative operators to preserve
    argument ordering. *)
module Func : sig
  (** [('fn, 'ret) t] represents a function signature specification.

      - ['fn]: The resulting OCaml function type (e.g.,
        [int expr -> bool expr]).
      - ['ret]: The SMT return type (e.g., [bool]). *)
  type ('fn, 'ret) t

  (** [ret ty] defines the end of a signature and specifies the return type.

      Example: [ret Types.bool] creates a signature that results in a
      [bool expr]. *)
  val ret : 'r ty -> ('r expr, 'r) t

  (** [arg_ty @-> next_sig] prepends an argument to the function signature.

      This operator is {b Right-Associative}.

      Example: [Types.int @-> Types.real @-> returning Types.bool]

      Creates a signature for: [int expr -> real expr -> bool expr]. *)
  val ( @-> ) : 'a ty -> ('fn, 'r) t -> ('a expr -> 'fn, 'r) t

  (** [make name signature] constructs the typed OCaml function.

      It accepts a name for the SMT function and a signature specification. It
      returns a curried OCaml function that builds the SMT application term.

      Usage:
      {@ocaml[
        let f = Func.make "f" @@ Types.int @-> returning Types.bool

        let x = symbol Types.int "x"

        let res : bool expr = f x
      ]} *)
  val make : string -> ('fn, 'r) t -> 'fn
end

(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2024 formalsec *)
(* Written by the Smtml programmers *)

(** Type Module. This module defines types and operations for working with SMT
    types, including unary, binary, relational, ternary, conversion, and n-ary
    operations. It also provides utilities for type comparison, pretty-printing,
    and parsing. *)

(** The type [_ cast] represents type casts for integers of different bit
    widths. *)
type _ cast =
  | C8 : int cast  (** Cast to an 8-bit integer. *)
  | C32 : int32 cast  (** Cast to a 32-bit integer. *)
  | C64 : int64 cast  (** Cast to a 64-bit integer. *)

(** {1 Type Definitions} *)

(** The type [t] represents smtml types. *)
type t =
  | Ty_app  (** Application type. *)
  | Ty_bitv of int  (** Bitvector type with a specified bit width. *)
  | Ty_bool  (** Boolean type. *)
  | Ty_fp of int  (** Floating-point type with a specified bit width. *)
  | Ty_int  (** Integer type. *)
  | Ty_list  (** List type. *)
  | Ty_none  (** None type. *)
  | Ty_real  (** Real number type. *)
  | Ty_str  (** String type. *)
  | Ty_unit  (** Unit type. *)
  | Ty_regexp  (** Regular expression type. *)

(** {1 Type Comparison} *)

(** [compare t1 t2] performs a total order comparison of types [t1] and [t2]. *)
val compare : t -> t -> int

(** [equal t1 t2] checks if types [t1] and [t2] are equal. *)
val equal : t -> t -> bool

(** {1 Pretty Printing} *)

(** [pp fmt t] pretty-prints the type [t] using the formatter [fmt]. *)
val pp : t Fmt.t

(** {1 String Conversion} *)

(** [string_of_type t] converts the type [t] to a string representation. *)
val string_of_type : t -> string

(** [of_string s] attempts to parse the string [s] into a type. Returns [Ok t]
    if successful, or an error message otherwise. *)
val of_string : string -> (t, [> `Msg of string ]) Result.t

(** {1 Type Size} *)

(** [size t] returns the size (in bits) of the type [t], if applicable. *)
val size : t -> int

(** {1 Unary Operations} *)

module Unop : sig
  (** The type [t] represents unary operations. *)
  type t =
    | Neg  (** Negation. *)
    | Not  (** Logical NOT. *)
    | Clz  (** Count leading zeros. *)
    | Ctz  (** Count trailing zeros. *)
    | Popcnt  (** Count bits set to 1. *)
    (* Float operations *)
    | Abs  (** Absolute value. *)
    | Sqrt  (** Square root. *)
    | Is_nan  (** Check if NaN. *)
    | Ceil  (** Ceiling. *)
    | Floor  (** Floor. *)
    | Trunc  (** Truncate. *)
    | Nearest  (** Round to nearest integer. *)
    | Head  (** Get the head of a list. *)
    | Tail  (** Get the tail of a list. *)
    | Reverse  (** Reverse a list. *)
    | Length  (** Get the length of a list. *)
    (* String operations *)
    | Trim  (** Trim whitespace (uninterpreted). *)
    (* Regexp operations *)
    | Regexp_star  (** Kleene star. *)
    | Regexp_loop of (int * int)  (** Loop with a range. *)
    | Regexp_plus  (** Kleene plus. *)
    | Regexp_opt  (** Optional. *)
    | Regexp_comp  (** Complement. *)

  (** [equal op1 op2] checks if unary operations [op1] and [op2] are equal. *)
  val equal : t -> t -> bool

  (** [pp fmt op] pretty-prints the unary operation [op] using the formatter
      [fmt]. *)
  val pp : t Fmt.t
end

(** {1 Binary Operations} *)

module Binop : sig
  (** The type [t] represents binary operations. *)
  type t =
    | Add  (** Addition. *)
    | Sub  (** Subtraction. *)
    | Mul  (** Multiplication. *)
    | Div  (** Division. *)
    | DivU  (** Unsigned division. *)
    | Rem  (** Remainder. *)
    | RemU  (** Unsigned remainder. *)
    | Shl  (** Shift left. *)
    | ShrA  (** Arithmetic shift right. *)
    | ShrL  (** Logical shift right. *)
    | And  (** Bitwise AND. *)
    | Or  (** Bitwise OR. *)
    | Xor  (** Bitwise XOR. *)
    | Pow  (** Exponentiation. *)
    | Min  (** Minimum. *)
    | Max  (** Maximum. *)
    | Copysign  (** Copy sign. *)
    | Rotl  (** Rotate left. *)
    | Rotr  (** Rotate right. *)
    | At  (** List indexing. *)
    | List_cons  (** List construction. *)
    | List_append  (** List concatenation. *)
    (* String operations *)
    | String_prefix
      (** Check if a string is a prefix. (str.prefixof String String Bool) *)
    | String_suffix
      (** Check if a string is a suffix. (str.suffixof String String Bool) *)
    | String_contains
      (** Check if a string contains another. (str.contains String String Bool)
      *)
    | String_last_index  (** Find the last index of a substring. *)
    | String_in_re  (** Check if a string matches a regular expression. *)
    (* Regexp operations *)
    | Regexp_range  (** Range of characters. *)

  (** [equal op1 op2] checks if binary operations [op1] and [op2] are equal. *)
  val equal : t -> t -> bool

  (** [pp fmt op] pretty-prints the binary operation [op] using the formatter
      [fmt]. *)
  val pp : t Fmt.t
end

(** {1 Relational Operations} *)

module Relop : sig
  (** The type [t] represents relational operations. *)
  type t =
    | Eq  (** Equality. *)
    | Ne  (** Inequality. *)
    | Lt  (** Less than. *)
    | LtU  (** Unsigned less than. *)
    | Gt  (** Greater than. *)
    | GtU  (** Unsigned greater than. *)
    | Le  (** Less than or equal. *)
    | LeU  (** Unsigned less than or equal. *)
    | Ge  (** Greater than or equal. *)
    | GeU  (** Unsigned greater than or equal. *)

  (** [equal op1 op2] checks if relational operations [op1] and [op2] are equal.
  *)
  val equal : t -> t -> bool

  (** [pp fmt op] pretty-prints the relational operation [op] using the
      formatter [fmt]. *)
  val pp : t Fmt.t
end

(** {1 Ternary Operations} *)

module Triop : sig
  (** The type [t] represents ternary operations. *)
  type t =
    | Ite  (** If-then-else. *)
    | List_set  (** Set an element in a list. *)
    (* String operations *)
    | String_extract
      (** Extract a substring. (str.substr String Int Int String) *)
    | String_replace
      (** Replace a substring. (str.replace String String String String) *)
    | String_index
      (** Find the index of a substring. (str.indexof String String Int Int) *)

  (** [equal op1 op2] checks if ternary operations [op1] and [op2] are equal. *)
  val equal : t -> t -> bool

  (** [pp fmt op] pretty-prints the ternary operation [op] using the formatter
      [fmt]. *)
  val pp : t Fmt.t
end

(** {1 Conversion Operations} *)

module Cvtop : sig
  (** The type [t] represents conversion operations. *)
  type t =
    | ToString  (** Convert to string. *)
    | OfString  (** Convert from string. *)
    | ToBool  (** Convert to boolean. *)
    | OfBool  (** Convert from boolean. *)
    | Reinterpret_int  (** Reinterpret as integer. *)
    | Reinterpret_float  (** Reinterpret as float. *)
    | DemoteF64  (** Demote 64-bit float to 32-bit. *)
    | PromoteF32  (** Promote 32-bit float to 64-bit. *)
    | ConvertSI32  (** Convert signed to 32-bit integer. *)
    | ConvertUI32  (** Convert unsigned to 32-bit integer. *)
    | ConvertSI64  (** Convert signed to 64-bit integer. *)
    | ConvertUI64  (** Convert unsigned to 64-bit integer. *)
    | TruncSF32  (** Truncate signed 32-bit float. *)
    | TruncUF32  (** Truncate unsigned 32-bit float. *)
    | TruncSF64  (** Truncate signed 64-bit float. *)
    | TruncUF64  (** Truncate unsigned 64-bit float. *)
    | Trunc_sat_f32_s
    | Trunc_sat_f32_u
    | Trunc_sat_f64_s
    | Trunc_sat_f64_u
    | WrapI64  (** Wrap 64-bit integer. *)
    | Sign_extend of int  (** Sign-extend to a specified bit width. *)
    | Zero_extend of int  (** Zero-extend to a specified bit width. *)
    (* String operations *)
    | String_to_code
      (** Convert string to Unicode code point. (str.to_code String Int) *)
    | String_from_code
      (** Convert Unicode code point to string. (str.to_int String Int) *)
    | String_to_int  (** Convert string to integer. (str.from_int Int String) *)
    | String_from_int  (** Convert integer to string. *)
    | String_to_float  (** Convert string to float. *)
    | String_to_re  (** Convert string to regular expression. *)

  (** [equal op1 op2] checks if conversion operations [op1] and [op2] are equal.
  *)
  val equal : t -> t -> bool

  (** [pp fmt op] pretty-prints the conversion operation [op] using the
      formatter [fmt]. *)
  val pp : t Fmt.t
end

(** {1 N-ary Operations} *)

module Naryop : sig
  (** The type [t] represents n-ary operations. *)
  type t =
    | Logand  (** Logical AND. *)
    | Logor  (** Logical OR. *)
    | Concat  (** Concatenation. *)
    | Regexp_union  (** Union of regular expressions. *)

  (** [equal op1 op2] checks if n-ary operations [op1] and [op2] are equal. *)
  val equal : t -> t -> bool

  (** [pp fmt op] pretty-prints the n-ary operation [op] using the formatter
      [fmt]. *)
  val pp : t Fmt.t
end

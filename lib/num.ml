open Base
open Types

type t = (Int.t, Int32.t, Int64.t, Int32.t, Int64.t) num

let ( = ) (n1 : t) (n2 : t) : bool =
  match (n1, n2) with
  | Int i1, Int i2 -> Int.(i1 = i2)
  | I32 i1, I32 i2 -> Int32.(i1 = i2)
  | I64 i1, I64 i2 -> Int64.(i1 = i2)
  | F32 i1, F32 i2 -> Int32.(i1 = i2)
  | F64 i1, F64 i2 -> Int64.(i1 = i2)
  | _, _ -> false

let default_value (t : num_type) : t =
  match t with
  | IntType -> Int 0
  | I32Type -> I32 0l
  | I64Type -> I64 0L
  | F32Type -> F32 (Int32.bits_of_float 0.0)
  | F64Type -> F64 (Int64.bits_of_float 0.0)

let string_of_num (n : t) : string =
  match n with
  | Int i -> Int.to_string i
  | I32 i -> Int32.to_string i ^ "l"
  | I64 i -> Int64.to_string i ^ "L"
  | F32 f -> Float.to_string (Int32.float_of_bits f)
  | F64 f -> Float.to_string (Int64.float_of_bits f)

let num_of_bool (b : bool) : t = I32 (if b then 1l else 0l)

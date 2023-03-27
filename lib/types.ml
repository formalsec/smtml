open Base

type ('i, 'i32, 'i64, 'f32, 'f64) num =
  | Int of 'i
  | I32 of 'i32
  | I64 of 'i64
  | F32 of 'f32
  | F64 of 'f64

type ('i, 'i32, 'i64, 'f32, 'f64) op =
  | Int of 'i
  | I32 of 'i32
  | I64 of 'i64
  | F32 of 'f32
  | F64 of 'f64

module I = IntOp
module I32 = BvOp
module I64 = BvOp
module F32 = FloatOp
module F64 = FloatOp

type binop = (I.binop, I32.binop, I64.binop, F32.binop, F64.binop) op
type unop = (I.unop, I32.unop, I64.unop, F32.unop, F64.unop) op
type relop = (I.relop, I32.relop, I64.relop, F32.relop, F64.relop) op
type cvtop = (I.cvtop, I32.cvtop, I64.cvtop, F32.cvtop, F64.cvtop) op
type num_type = IntType | I32Type | I64Type | F32Type | F64Type

let type_of_num (n : ('i, 'i32, 'i64, 'f32, 'f64) num) =
  match n with
  | Int _ -> IntType
  | I32 _ -> I32Type
  | I64 _ -> I64Type
  | F32 _ -> F32Type
  | F64 _ -> F64Type

let type_of op =
  match op with
  | Int _ -> IntType
  | I32 _ -> I32Type
  | I64 _ -> I64Type
  | F32 _ -> F32Type
  | F64 _ -> F64Type

let size (t : num_type) : int =
  match t with I32Type | F32Type -> 4 | IntType | I64Type | F64Type -> 8

let string_of_num_type (t : num_type) : string =
  match t with
  | IntType -> "IntType"
  | I32Type -> "I32Type"
  | I64Type -> "I64Type"
  | F32Type -> "F32Type"
  | F64Type -> "F64Type"

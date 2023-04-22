type ('i32, 'i64, 'f32, 'f64) num =
  | I32 of 'i32
  | I64 of 'i64
  | F32 of 'f32
  | F64 of 'f64

type ('i, 'r, 'b, 'str, 'i32, 'i64, 'f32, 'f64) op =
  | Int of 'i
  | Real of 'r
  | Bool of 'b
  | Str of 'str
  | I32 of 'i32
  | I64 of 'i64
  | F32 of 'f32
  | F64 of 'f64

module I = IntOp
module B = BoolOp
module S = StrOp
module R = FloatOp
module I32 = BvOp
module I64 = BvOp
module F32 = FloatOp
module F64 = FloatOp

type triop =
  ( I.triop,
    R.triop,
    B.triop,
    S.triop,
    I32.triop,
    I64.triop,
    F32.triop,
    F64.triop )
  op

type binop =
  ( I.binop,
    R.binop,
    B.binop,
    S.binop,
    I32.binop,
    I64.binop,
    F32.binop,
    F64.binop )
  op

type unop =
  (I.unop, R.unop, B.unop, S.unop, I32.unop, I64.unop, F32.unop, F64.unop) op

type relop =
  ( I.relop,
    R.relop,
    B.relop,
    S.relop,
    I32.relop,
    I64.relop,
    F32.relop,
    F64.relop )
  op

type cvtop =
  ( I.cvtop,
    R.cvtop,
    B.cvtop,
    S.cvtop,
    I32.cvtop,
    I64.cvtop,
    F32.cvtop,
    F64.cvtop )
  op

type num_type = [ `I32Type | `I64Type | `F32Type | `F64Type ]
type expr_type = [ num_type | `IntType | `RealType | `BoolType | `StrType ]

let type_of_num (n : ('i32, 'i64, 'f32, 'f64) num) =
  match n with
  | I32 _ -> `I32Type
  | I64 _ -> `I64Type
  | F32 _ -> `F32Type
  | F64 _ -> `F64Type

let type_of op =
  match op with
  | Int _ -> `IntType
  | Real _ -> `RealType
  | Bool _ -> `BoolType
  | Str _ -> `StrType
  | I32 _ -> `I32Type
  | I64 _ -> `I64Type
  | F32 _ -> `F32Type
  | F64 _ -> `F64Type

let size_of_num_type (t : num_type) : int =
  match t with `I32Type | `F32Type -> 4 | `I64Type | `F64Type -> 8

let size (t : expr_type) : int =
  match t with
  | #num_type as t' -> size_of_num_type t'
  | `IntType | `RealType | `BoolType | `StrType -> assert false

let string_of_num_type (t : num_type) : string =
  match t with
  | `I32Type -> "I32Type"
  | `I64Type -> "I64Type"
  | `F32Type -> "F32Type"
  | `F64Type -> "F64Type"

let string_of_type (t : expr_type) : string =
  match t with
  | #num_type as t' -> string_of_num_type t'
  | `IntType -> "IntType"
  | `RealType -> "FltType"
  | `BoolType -> "BoolType"
  | `StrType -> "StrType"

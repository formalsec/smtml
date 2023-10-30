open Types

type t = (Int32.t, Int64.t, Int32.t, Int64.t) Types.num

let ( = ) (n1 : t) (n2 : t) : bool =
  match (n1, n2) with
  | I32 i1, I32 i2 -> i1 = i2
  | I64 i1, I64 i2 -> i1 = i2
  | F32 i1, F32 i2 -> i1 = i2
  | F64 i1, F64 i2 -> i1 = i2
  | _ -> false

let type_of (n : t) =
  match n with
  | I32 _ -> `I32Type
  | I64 _ -> `I64Type
  | F32 _ -> `F32Type
  | F64 _ -> `F64Type

let default_value (t : num_type) : t =
  match t with
  | `I32Type -> I32 0l
  | `I64Type -> I64 0L
  | `F32Type -> F32 (Int32.bits_of_float 0.0)
  | `F64Type -> F64 (Int64.bits_of_float 0.0)

let pp fmt (n : t) =
  match n with
  | I32 i -> Format.fprintf fmt "(i32 %ld)" i
  | I64 i -> Format.fprintf fmt "(i64 %Ld)" i
  | F32 f -> Format.fprintf fmt "(f32 %f)" (Int32.float_of_bits f)
  | F64 f -> Format.fprintf fmt "(f64 %f)" (Int64.float_of_bits f)

let pp_hex fmt (n : t) =
  match n with
  | I32 i | F32 i -> Format.fprintf fmt "0x%lx" i
  | I64 i | F64 i -> Format.fprintf fmt "0x%Ld" i

let to_string (n : t) : string = Format.asprintf "%a" pp n
let num_of_bool (b : bool) : t = I32 (if b then 1l else 0l)

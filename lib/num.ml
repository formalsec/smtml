type t =
  | I8 of int
  | I16 of int
  | I32 of int32
  | I64 of int64
  | F16 of int
  | F32 of int32
  | F64 of int64

let ( = ) n1 n2 =
  match (n1, n2) with
  | I8 i1, I8 i2 -> i1 = i2
  | I16 i1, I16 i2 -> i1 = i2
  | I32 i1, I32 i2 -> i1 = i2
  | I64 i1, I64 i2 -> i1 = i2
  | F16 i1, F16 i2 -> i1 = i2
  | F32 i1, F32 i2 -> i1 = i2
  | F64 i1, F64 i2 -> i1 = i2
  | _ -> false

let compare n1 n2 =
  match (n1, n2) with
  | I8 i1, I8 i2 -> compare i1 i2
  | I16 i1, I16 i2 -> compare i1 i2
  | I32 i1, I32 i2 -> compare i1 i2
  | I64 i1, I64 i2 -> compare i1 i2
  | F16 i1, F16 i2 -> compare i1 i2
  | F32 i1, F32 i2 -> compare i1 i2
  | F64 i1, F64 i2 -> compare i1 i2
  | _ -> compare n1 n2

let ty : t -> Ty.t = function
  | I8 _ -> Ty_bitv 8
  | I16 _ -> Ty_bitv 16
  | I32 _ -> Ty_bitv 32
  | I64 _ -> Ty_bitv 64
  | F16 _ -> Ty_fp 16
  | F32 _ -> Ty_fp 32
  | F64 _ -> Ty_fp 64

let pp fmt = function
  | I8 i -> Format.fprintf fmt "(i8 %d)" i
  | I16 i -> Format.fprintf fmt "(i16 %d)" i
  | I32 i -> Format.fprintf fmt "(i32 %ld)" i
  | I64 i -> Format.fprintf fmt "(i64 %Ld)" i
  | F16 i -> Format.fprintf fmt "(f16 %F)" Int32.(float_of_bits (of_int i))
  | F32 f -> Format.fprintf fmt "(f32 %F)" (Int32.float_of_bits f)
  | F64 f -> Format.fprintf fmt "(f64 %F)" (Int64.float_of_bits f)

let pp_hex fmt = function
  | I8 i | I16 i | F16 i -> Format.fprintf fmt "0x%x" i
  | I32 i | F32 i -> Format.fprintf fmt "0x%lx" i
  | I64 i | F64 i -> Format.fprintf fmt "0x%Lx" i

let to_string n = Format.asprintf "%a" pp n

let of_bool b = I32 (if b then 1l else 0l)

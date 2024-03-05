type t =
  | I8 of int
  | I32 of int32
  | I64 of int64
  | F32 of int32
  | F64 of int64

let ( = ) (n1 : t) (n2 : t) : bool =
  match (n1, n2) with
  | I8 i1, I8 i2 -> i1 = i2
  | I32 i1, I32 i2 -> i1 = i2
  | I64 i1, I64 i2 -> i1 = i2
  | F32 i1, F32 i2 -> Int32.float_of_bits i1 = Int32.float_of_bits i2
  | F64 i1, F64 i2 -> Int64.float_of_bits i1 = Int64.float_of_bits i2
  | I8 _, _ | I32 _, _ | I64 _, _ | F32 _, _ | F64 _, _ -> false

let compare n1 n2 =
  match (n1, n2) with
  | I8 i1, I8 i2 -> compare i1 i2
  | I32 i1, I32 i2 -> compare i1 i2
  | I64 i1, I64 i2 -> compare i1 i2
  | F32 i1, F32 i2 -> compare (Int32.float_of_bits i1) (Int32.float_of_bits i2)
  | F64 i1, F64 i2 -> compare (Int64.float_of_bits i1) (Int64.float_of_bits i2)
  (*
     Stdlib.compare guarantees that elements of a same variant will be "ordered
     together"
  *)
  | I8 _, _ | I32 _, _ | I64 _, _ | F32 _, _ | F64 _, _ -> compare n1 n2

let type_of (n : t) =
  match n with
  | I8 _ -> Ty.(Ty_bitv 8)
  | I32 _ -> Ty.(Ty_bitv 32)
  | I64 _ -> Ty.(Ty_bitv 64)
  | F32 _ -> Ty.(Ty_fp 32)
  | F64 _ -> Ty.(Ty_fp 64)

let pp fmt (n : t) =
  match n with
  | I8 i -> Format.fprintf fmt "(i8 %d)" i
  | I32 i -> Format.fprintf fmt "(i32 %ld)" i
  | I64 i -> Format.fprintf fmt "(i64 %Ld)" i
  | F32 f -> Format.fprintf fmt "(f32 %F)" (Int32.float_of_bits f)
  | F64 f -> Format.fprintf fmt "(f64 %F)" (Int64.float_of_bits f)

let pp_hex fmt (n : t) =
  match n with
  | I8 i -> Format.fprintf fmt "0x%x" i
  | I32 i | F32 i -> Format.fprintf fmt "0x%lx" i
  | I64 i | F64 i -> Format.fprintf fmt "0x%Lx" i

let to_string (n : t) : string = Format.asprintf "%a" pp n

let num_of_bool (b : bool) : t = I32 (if b then 1l else 0l)

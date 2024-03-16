(* Adapted from: *)
(* - https://github.com/WebAssembly/spec/blob/main/interpreter/exec/ixx.ml, *)
(* - https://github.com/WebAssembly/spec/blob/main/interpreter/exec/fxx.ml, and *)
(* - https://github.com/WebAssembly/spec/blob/main/interpreter/exec *)

(* TODO: This module should be eval_concrete or a part of the reducer *)

open Ty

exception Value of Ty.t

exception TypeError of int * Value.t * Ty.t

exception DivideByZero

exception ConversionToInteger

exception IntegerOverflow

let of_arg f n v = try f v with Value t -> raise (TypeError (n, v, t))
[@@inline]

module Int = struct
  let to_value (i : int) : Value.t = Int i [@@inline]

  let of_value (n : int) (v : Value.t) : int =
    of_arg (function Int i -> i | _ -> raise (Value Ty_int)) n v
  [@@inline]

  let unop (op : unop) (v : Value.t) : Value.t =
    let f =
      match op with
      | Neg -> ( ~- )
      | _ -> Log.err {|eval_unop: Unsupported int operator "%a"|} Ty.pp_unop op
    in
    to_value (f (of_value 1 v))

  let binop (op : binop) (v1 : Value.t) (v2 : Value.t) : Value.t =
    let f =
      match op with
      | Add -> ( + )
      | Sub -> ( - )
      | Mul -> ( * )
      | Div -> ( / )
      | Rem -> ( mod )
      | Pow -> fun x y -> int_of_float (float_of_int x ** float_of_int y)
      | Min -> min
      | Max -> max
      | _ ->
        Log.err {|eval_binop: Unsupported int operator "%a"|} Ty.pp_binop op
    in
    to_value (f (of_value 1 v1) (of_value 2 v2))

  let relop (op : relop) (v1 : Value.t) (v2 : Value.t) : bool =
    let f =
      match op with
      | Eq -> ( = )
      | Ne -> ( <> )
      | Lt -> ( < )
      | Le -> ( <= )
      | Gt -> ( > )
      | Ge -> ( >= )
      | _ ->
        Log.err {|eval_relop: Unsupported int operator "%a"|} Ty.pp_relop op
    in
    f (of_value 1 v1) (of_value 2 v2)
end

module Real = struct
  let to_value (v : float) : Value.t = Real v [@@inline]

  let of_value (n : int) (v : Value.t) : float =
    of_arg (function Real v -> v | _ -> raise (Value Ty_int)) n v
  [@@inline]

  let unop (op : unop) (v : Value.t) : Value.t =
    let v = of_value 1 v in
    match op with
    | Neg -> to_value @@ Float.neg v
    | Abs -> to_value @@ Float.abs v
    | Sqrt -> to_value @@ Float.sqrt v
    | Nearest -> to_value @@ Float.round v
    | Ceil -> to_value @@ Float.ceil v
    | Floor -> to_value @@ Float.floor v
    | Trunc -> to_value @@ Float.trunc v
    | Is_nan -> if Float.is_nan v then Value.True else Value.False
    | _ -> Log.err {|eval_unop: Unsupported real operator "%a"|} Ty.pp_unop op

  let binop (op : binop) (v1 : Value.t) (v2 : Value.t) : Value.t =
    let f =
      match op with
      | Add -> Float.add
      | Sub -> Float.sub
      | Mul -> Float.mul
      | Div -> Float.div
      | Rem -> Float.rem
      | Min -> Float.min
      | Max -> Float.max
      | _ ->
        Log.err {|eval_binop: Unsupported real operator "%a"|} Ty.pp_binop op
    in
    to_value (f (of_value 1 v1) (of_value 2 v2))

  let relop (op : relop) (v1 : Value.t) (v2 : Value.t) : bool =
    let f =
      match op with
      | Eq -> ( = )
      | Ne -> ( <> )
      | Lt -> ( < )
      | Le -> ( <= )
      | Gt -> ( > )
      | Ge -> ( >= )
      | _ ->
        Log.err {|eval_relop: Unsupported real operator "%a"|} Ty.pp_relop op
    in
    f (of_value 1 v1) (of_value 2 v2)

  let cvtop (op : cvtop) (v : Value.t) : Value.t =
    match op with
    | ToString -> Str (Float.to_string (of_value 1 v))
    | OfString ->
      let v = match v with Str v -> v | _ -> raise (Value Ty_str) in
      to_value (Float.of_string v)
    | Reinterpret_int ->
      let v = match v with Int v -> v | _ -> raise (Value Ty_int) in
      to_value (Float.of_int v)
    | _ -> Log.err {|eval_cvtop: Unsupported real operator "%a"|} Ty.pp_cvtop op
end

module I32 = struct
  let to_value (i : int32) : Value.t = Num (I32 i) [@@inline]

  let of_value (n : int) (v : Value.t) : int32 =
    of_arg (function Num (I32 i) -> i | _ -> raise (Value (Ty_bitv 32))) n v
  [@@inline]

  let cmp_u x op y = op Int32.(add x min_int) Int32.(add y min_int) [@@inline]

  let lt_u x y = cmp_u x ( < ) y [@@inline]

  let le_u x y = cmp_u x ( <= ) y [@@inline]

  let gt_u x y = cmp_u x ( > ) y [@@inline]

  let ge_u x y = cmp_u x ( >= ) y [@@inline]

  let shift f x y = f x Int32.(to_int (logand y 31l)) [@@inline]

  let shl x y = shift Int32.shift_left x y [@@inline]

  let shr_s x y = shift Int32.shift_right x y [@@inline]

  let shr_u x y = shift Int32.shift_right_logical x y [@@inline]

  (* Stolen rotl and rotr from: *)
  (* https://github.com/OCamlPro/owi/blob/main/src/int32.ml *)
  (* We must mask the count to implement rotates via shifts. *)
  let clamp_rotate_count n = Int32.(to_int (logand n 31l)) [@@inline]

  let rotl x y =
    let n = clamp_rotate_count y in
    Int32.logor (shl x (Int32.of_int n)) (shr_u x (Int32.of_int (32 - n)))
  [@@inline]

  let rotr x y =
    let n = clamp_rotate_count y in
    Int32.logor (shr_u x (Int32.of_int n)) (shl x (Int32.of_int (32 - n)))
  [@@inline]

  let unop (op : unop) (v : Value.t) : Value.t =
    let f =
      match op with
      | Neg -> Int32.neg
      | Not -> Int32.lognot
      | Clz ->
        fun n -> Int32.of_int (Ocaml_intrinsics.Int32.count_leading_zeros n)
      | Ctz ->
        fun n -> Int32.of_int (Ocaml_intrinsics.Int32.count_trailing_zeros n)
      | _ -> Log.err {|eval_unop: Unsupported i32 operator "%a"|} Ty.pp_unop op
    in
    to_value (f (of_value 1 v))

  let binop (op : binop) (v1 : Value.t) (v2 : Value.t) : Value.t =
    let f =
      match op with
      | Add -> Int32.add
      | Sub -> Int32.sub
      | Mul -> Int32.mul
      | Div -> Int32.div
      | DivU -> Int32.unsigned_div
      | Rem -> Int32.rem
      | RemU -> Int32.unsigned_rem
      | And -> Int32.logand
      | Or -> Int32.logor
      | Xor -> Int32.logxor
      | Shl -> shl
      | ShrL -> shr_u
      | ShrA -> shr_s
      | Rotl -> rotl
      | Rotr -> rotr
      | _ ->
        Log.err {|eval_binop: Unsupported i32 operator "%a"|} Ty.pp_binop op
    in
    to_value (f (of_value 1 v1) (of_value 2 v2))

  let relop (op : relop) (v1 : Value.t) (v2 : Value.t) : bool =
    let f =
      match op with
      | Eq -> ( = )
      | Ne -> ( <> )
      | Lt -> ( < )
      | LtU -> lt_u
      | Le -> ( <= )
      | LeU -> le_u
      | Gt -> ( > )
      | GtU -> gt_u
      | Ge -> ( >= )
      | GeU -> ge_u
    in
    f (of_value 1 v1) (of_value 2 v2)
end

module I64 = struct
  let to_value (i : int64) : Value.t = Num (I64 i) [@@inline]

  let of_value (n : int) (v : Value.t) : int64 =
    of_arg (function Num (I64 i) -> i | _ -> raise (Value (Ty_bitv 64))) n v
  [@@inline]

  let cmp_u x op y = op Int64.(add x min_int) Int64.(add y min_int) [@@inline]

  let lt_u x y = cmp_u x ( < ) y [@@inline]

  let le_u x y = cmp_u x ( <= ) y [@@inline]

  let gt_u x y = cmp_u x ( > ) y [@@inline]

  let ge_u x y = cmp_u x ( >= ) y [@@inline]

  let shift f x y = f x Int64.(to_int (logand y 63L)) [@@inline]

  let shl x y = shift Int64.shift_left x y [@@inline]

  let shr_s x y = shift Int64.shift_right x y [@@inline]

  let shr_u x y = shift Int64.shift_right_logical x y [@@inline]

  (* Stolen rotl and rotr from: *)
  (* https://github.com/OCamlPro/owi/blob/main/src/int64.ml *)
  (* We must mask the count to implement rotates via shifts. *)
  let clamp_rotate_count n = Int64.(to_int (logand n (of_int 63))) [@@inline]

  let rotl x y =
    let n = clamp_rotate_count y in
    Int64.logor (shl x (Int64.of_int n)) (shr_u x (Int64.of_int (64 - n)))
  [@@inline]

  let rotr x y =
    let n = clamp_rotate_count y in
    Int64.logor (shr_u x (Int64.of_int n)) (shl x (Int64.of_int (64 - n)))
  [@@inline]

  let unop (op : unop) (v : Value.t) : Value.t =
    let f =
      match op with
      | Neg -> Int64.neg
      | Not -> Int64.lognot
      | Clz ->
        fun n -> Int64.of_int (Ocaml_intrinsics.Int64.count_leading_zeros n)
      | Ctz ->
        fun n -> Int64.of_int (Ocaml_intrinsics.Int64.count_trailing_zeros n)
      | _ -> Log.err {|eval_unop: Unsupported i64 operator "%a"|} Ty.pp_unop op
    in
    to_value (f (of_value 1 v))

  let binop (op : binop) (v1 : Value.t) (v2 : Value.t) : Value.t =
    let f =
      match op with
      | Add -> Int64.add
      | Sub -> Int64.sub
      | Mul -> Int64.mul
      | Div -> Int64.div
      | DivU -> Int64.unsigned_div
      | Rem -> Int64.rem
      | RemU -> Int64.unsigned_rem
      | And -> Int64.logand
      | Or -> Int64.logor
      | Xor -> Int64.logxor
      | Shl -> shl
      | ShrL -> shr_u
      | ShrA -> shr_s
      | Rotl -> rotl
      | Rotr -> rotr
      | _ ->
        Log.err {|eval_binop: Unsupported i64 operator "%a"|} Ty.pp_binop op
    in
    to_value (f (of_value 1 v1) (of_value 2 v2))

  let relop (op : relop) (v1 : Value.t) (v2 : Value.t) : bool =
    let f =
      match op with
      | Eq -> ( = )
      | Ne -> ( <> )
      | Lt -> ( < )
      | LtU -> lt_u
      | Le -> ( <= )
      | LeU -> le_u
      | Gt -> ( > )
      | GtU -> gt_u
      | Ge -> ( >= )
      | GeU -> ge_u
    in
    f (of_value 1 v1) (of_value 2 v2)
end

module F32 = struct
  let to_float (v : int32) : float = Int32.float_of_bits v [@@inline]

  let of_float (v : float) : int32 = Int32.bits_of_float v [@@inline]

  let to_value (f : int32) : Value.t = Num (F32 f) [@@inline]

  let to_value' (f : float) : Value.t = to_value @@ of_float f [@@inline]

  let of_value (i : int) (v : Value.t) : int32 =
    of_arg (function Num (F32 f) -> f | _ -> raise (Value (Ty_fp 32))) i v
  [@@inline]

  let of_value' (i : int) (v : Value.t) : float = of_value i v |> to_float
  [@@inline]

  let unop (op : unop) (v : Value.t) : Value.t =
    let v = to_float @@ of_value 1 v in
    match op with
    | Neg -> to_value' @@ Float.neg v
    | Abs -> to_value' @@ Float.abs v
    | Sqrt -> to_value' @@ Float.sqrt v
    | Nearest -> to_value' @@ Float.round v
    | Ceil -> to_value' @@ Float.ceil v
    | Floor -> to_value' @@ Float.floor v
    | Trunc -> to_value' @@ Float.trunc v
    | Is_nan -> if Float.is_nan v then Value.True else Value.False
    | Not | Clz | Ctz | Trim | Seq_length ->
      Log.err {|eval_unop: Unsupported f32 operator "%a"|} Ty.pp_unop op

  let binop (op : binop) (v1 : Value.t) (v2 : Value.t) : Value.t =
    let f =
      match op with
      | Add -> Float.add
      | Sub -> Float.sub
      | Mul -> Float.mul
      | Div -> Float.div
      | Rem -> Float.rem
      | Min -> Float.min
      | Max -> Float.max
      | _ ->
        Log.err {|eval_binop: Unsupported f32 operator "%a"|} Ty.pp_binop op
    in
    to_value' (f (of_value' 1 v1) (of_value' 2 v2))

  let relop (op : relop) (v1 : Value.t) (v2 : Value.t) : bool =
    let f =
      match op with
      | Eq -> ( = )
      | Ne -> ( <> )
      | Lt -> ( < )
      | Le -> ( <= )
      | Gt -> ( > )
      | Ge -> ( >= )
      | _ ->
        Log.err {|eval_relop: Unsupported f32 operator "%a"|} Ty.pp_relop op
    in
    f (of_value' 1 v1) (of_value' 2 v2)
end

module F64 = struct
  let to_float (v : int64) : float = Int64.float_of_bits v [@@inline]

  let of_float (v : float) : int64 = Int64.bits_of_float v [@@inline]

  let to_value (f : int64) : Value.t = Num (F64 f) [@@inline]

  let to_value' (f : float) : Value.t = to_value @@ of_float f [@@inline]

  let of_value (i : int) (v : Value.t) : int64 =
    of_arg (function Num (F64 f) -> f | _ -> raise (Value (Ty_fp 64))) i v
  [@@inline]

  let of_value' (i : int) (v : Value.t) : float = of_value i v |> to_float
  [@@inline]

  let unop (op : unop) (v : Value.t) : Value.t =
    let v = of_value' 1 v in
    match op with
    | Neg -> to_value' @@ Float.neg v
    | Abs -> to_value' @@ Float.abs v
    | Sqrt -> to_value' @@ Float.sqrt v
    | Nearest -> to_value' @@ Float.round v
    | Ceil -> to_value' @@ Float.ceil v
    | Floor -> to_value' @@ Float.floor v
    | Trunc -> to_value' @@ Float.trunc v
    | Is_nan -> if Float.is_nan v then Value.True else Value.False
    | Not | Clz | Ctz | Trim | Seq_length ->
      Log.err {|eval_unop: Unsupported f32 operator "%a"|} Ty.pp_unop op

  let binop (op : binop) (v1 : Value.t) (v2 : Value.t) : Value.t =
    let f =
      match op with
      | Add -> Float.add
      | Sub -> Float.sub
      | Mul -> Float.mul
      | Div -> Float.mul
      | Rem -> Float.rem
      | Min -> Float.min
      | Max -> Float.max
      | _ ->
        Log.err {|eval_binop: Unsupported f32 operator "%a"|} Ty.pp_binop op
    in
    to_value' (f (of_value' 1 v1) (of_value' 2 v2))

  let relop (op : relop) (v1 : Value.t) (v2 : Value.t) : bool =
    let f =
      match op with
      | Eq -> ( = )
      | Ne -> ( <> )
      | Lt -> ( < )
      | Le -> ( <= )
      | Gt -> ( > )
      | Ge -> ( >= )
      | _ ->
        Log.err {|eval_relop: Unsupported f32 operator "%a"|} Ty.pp_relop op
    in
    f (of_value' 1 v1) (of_value' 2 v2)
end

module IntCvtOp = struct
  let of_bool : Value.t -> int = function
    | True -> 1
    | False -> 0
    | Int i -> i
    | _ -> assert false
  [@@inline]

  let cvtop (op : cvtop) (v : Value.t) : Value.t =
    match op with
    | OfBool -> Int.to_value (of_bool v)
    | _ -> Log.err {|eval_cvtop: Unsupported int operator "%a"|} Ty.pp_cvtop op
end

module I32CvtOp = struct
  let extend_s (n : int) (x : int32) : int32 =
    let shift = 32 - n in
    Int32.(shift_right (shift_left x shift) shift)

  let trunc_f32_s (x : int32) =
    if x <> x then raise ConversionToInteger
    else
      let xf = F32.to_float x in
      if xf >= -.Int32.(to_float min_int) || xf < Int32.(to_float min_int) then
        raise IntegerOverflow
      else Int32.of_float xf

  let trunc_f32_u (x : int32) =
    if x <> x then raise ConversionToInteger
    else
      let xf = F32.to_float x in
      if xf >= -.Int32.(to_float min_int) *. 2.0 || xf <= -1.0 then
        raise IntegerOverflow
      else Int32.of_float xf

  let trunc_f64_s (x : int64) =
    if x <> x then raise ConversionToInteger
    else
      let xf = F64.to_float x in
      if xf >= -.Int64.(to_float min_int) || xf < Int64.(to_float min_int) then
        raise IntegerOverflow
      else Int32.of_float xf

  let trunc_f64_u (x : int64) =
    if x <> x then raise ConversionToInteger
    else
      let xf = F64.to_float x in
      if xf >= -.Int64.(to_float min_int) *. 2.0 || xf <= -1.0 then
        raise IntegerOverflow
      else Int32.of_float xf

  let cvtop (op : cvtop) (v : Value.t) : Value.t =
    match op with
    | WrapI64 -> I32.to_value (Int64.to_int32 (I64.of_value 1 v))
    | TruncSF32 -> I32.to_value (trunc_f32_s (F32.of_value 1 v))
    | TruncUF32 -> I32.to_value (trunc_f32_u (F32.of_value 1 v))
    | TruncSF64 -> I32.to_value (trunc_f64_s (F64.of_value 1 v))
    | TruncUF64 -> I32.to_value (trunc_f64_u (F64.of_value 1 v))
    | Reinterpret_float -> I32.to_value (F32.of_value 1 v)
    | ExtS n -> I32.to_value (extend_s n (I32.of_value 1 v))
    | ExtU _n -> I32.to_value (I32.of_value 1 v)
    | OfBool -> v (* already a num here *)
    | ToBool | _ ->
      Log.err {|eval_cvtop: Unsupported i32 operator "%a"|} Ty.pp_cvtop op
end

module I64CvtOp = struct
  (* let extend_s n x = *)
  (*   let shift = 64 - n in *)
  (*   Int64.(shift_right (shift_left x shift) shift) *)

  let extend_i32_u (x : int32) =
    Int64.(logand (of_int32 x) 0x0000_0000_ffff_ffffL)

  let trunc_f32_s (x : int32) =
    if x <> x then raise ConversionToInteger
    else
      let xf = F32.to_float x in
      if xf >= -.Int64.(to_float min_int) || xf < Int64.(to_float min_int) then
        raise IntegerOverflow
      else Int64.of_float xf

  let trunc_f32_u x =
    if x <> x then raise ConversionToInteger
    else
      let xf = F32.to_float x in
      if xf >= -.Int64.(to_float min_int) *. 2.0 || xf <= -1.0 then
        raise IntegerOverflow
      else if xf >= -.Int64.(to_float min_int) then
        Int64.(logxor (of_float (xf -. 0x1p63)) min_int)
      else Int64.of_float xf

  let trunc_f64_s (x : int64) =
    if x <> x then raise ConversionToInteger
    else
      let xf = F64.to_float x in
      if xf >= -.Int64.(to_float min_int) || xf < Int64.(to_float min_int) then
        raise IntegerOverflow
      else Int64.of_float xf

  let trunc_f64_u x =
    if x <> x then raise ConversionToInteger
    else
      let xf = F64.to_float x in
      if xf >= -.Int64.(to_float min_int) *. 2.0 || xf <= -1.0 then
        raise IntegerOverflow
      else if xf >= -.Int64.(to_float min_int) then
        Int64.(logxor (of_float (xf -. 0x1p63)) min_int)
      else Int64.of_float xf

  let cvtop (op : cvtop) (v : Value.t) : Value.t =
    match op with
    | ExtS 32 -> I64.to_value (Int64.of_int32 (I32.of_value 1 v))
    | ExtU 32 -> I64.to_value (extend_i32_u (I32.of_value 1 v))
    | TruncSF32 -> I64.to_value (trunc_f32_s (F32.of_value 1 v))
    | TruncUF32 -> I64.to_value (trunc_f32_u (F32.of_value 1 v))
    | TruncSF64 -> I64.to_value (trunc_f64_s (F64.of_value 1 v))
    | TruncUF64 -> I64.to_value (trunc_f64_u (F64.of_value 1 v))
    | Reinterpret_float -> I64.to_value (F64.of_value 1 v)
    | WrapI64 -> raise (TypeError (1, v, Ty_bitv 64))
    | ToBool | OfBool | _ ->
      Log.err {|eval_cvtop: Unsupported i64 operator "%a"|} Ty.pp_cvtop op
end

module F32CvtOp = struct
  let demote_f64 x =
    let xf = F64.to_float x in
    if xf = xf then F32.of_float xf
    else
      let nan64bits = x in
      let sign_field =
        Int64.(shift_left (shift_right_logical nan64bits 63) 31)
      in
      let significand_field =
        Int64.(shift_right_logical (shift_left nan64bits 12) 41)
      in
      let fields = Int64.logor sign_field significand_field in
      Int32.logor 0x7fc0_0000l (Int64.to_int32 fields)

  let convert_i32_s x = F32.of_float (Int32.to_float x)

  let convert_i32_u x =
    F32.of_float
      Int32.(
        if x >= 0l then to_float x
        else to_float (logor (shift_right_logical x 1) (logand x 1l)) *. 2.0 )

  let convert_i64_s x =
    F32.of_float
      Int64.(
        if abs x < 0x10_0000_0000_0000L then to_float x
        else
          let r = if logand x 0xfffL = 0L then 0L else 1L in
          to_float (logor (shift_right x 12) r) *. 0x1p12 )

  let convert_i64_u x =
    F32.of_float
      Int64.(
        if I64.lt_u x 0x10_0000_0000_0000L then to_float x
        else
          let r = if logand x 0xfffL = 0L then 0L else 1L in
          to_float (logor (shift_right_logical x 12) r) *. 0x1p12 )

  let cvtop (op : cvtop) (v : Value.t) : Value.t =
    match op with
    | DemoteF64 -> F32.to_value (demote_f64 (F64.of_value 1 v))
    | ConvertSI32 -> F32.to_value (convert_i32_s (I32.of_value 1 v))
    | ConvertUI32 -> F32.to_value (convert_i32_u (I32.of_value 1 v))
    | ConvertSI64 -> F32.to_value (convert_i64_s (I64.of_value 1 v))
    | ConvertUI64 -> F32.to_value (convert_i64_u (I64.of_value 1 v))
    | Reinterpret_int -> F32.to_value (I32.of_value 1 v)
    | PromoteF32 -> raise (TypeError (1, v, Ty_fp 32))
    | ToString | OfString | _ ->
      Log.err {|eval_cvtop: Unsupported f32 operator "%a"|} Ty.pp_cvtop op
end

module F64CvtOp = struct
  let promote_f32 x =
    let xf = F32.to_float x in
    if xf = xf then F64.of_float xf
    else
      let nan32bits = I64CvtOp.extend_i32_u x in
      let sign_field =
        Int64.(shift_left (shift_right_logical nan32bits 31) 63)
      in
      let significand_field =
        Int64.(shift_right_logical (shift_left nan32bits 41) 12)
      in
      let fields = Int64.logor sign_field significand_field in
      Int64.logor 0x7ff8_0000_0000_0000L fields

  let convert_i32_s x = F64.of_float (Int32.to_float x)

  (*
   * Unlike the other convert_u functions, the high half of the i32 range is
   * within the range where f32 can represent odd numbers, so we can't do the
   * shift. Instead, we can use int64 signed arithmetic.
   *)
  let convert_i32_u x =
    F64.of_float Int64.(to_float (logand (of_int32 x) 0x0000_0000_ffff_ffffL))

  let convert_i64_s x = F64.of_float (Int64.to_float x)

  (*
   * Values in the low half of the int64 range can be converted with a signed
   * conversion. The high half is beyond the range where f64 can represent odd
   * numbers, so we can shift the value right, adjust the least significant
   * bit to round correctly, do a conversion, and then scale it back up.
   *)
  let convert_i64_u (x : int64) =
    F64.of_float
      Int64.(
        if x >= 0L then to_float x
        else to_float (logor (shift_right_logical x 1) (logand x 1L)) *. 2.0 )

  let cvtop (op : cvtop) v : Value.t =
    match op with
    | PromoteF32 -> F64.to_value (promote_f32 (F32.of_value 1 v))
    | ConvertSI32 -> F64.to_value (convert_i32_s (I32.of_value 1 v))
    | ConvertUI32 -> F64.to_value (convert_i32_u (I32.of_value 1 v))
    | ConvertSI64 -> F64.to_value (convert_i64_s (I64.of_value 1 v))
    | ConvertUI64 -> F64.to_value (convert_i64_u (I64.of_value 1 v))
    | Reinterpret_int -> F64.to_value (I64.of_value 1 v)
    | DemoteF64 -> raise (TypeError (1, v, Ty_bitv 64))
    | ToString | OfString | _ ->
      Log.err {|eval_cvtop: Unsupported f64 operator "%a"|} Ty.pp_cvtop op
end

(* Dispatch *)

let op int real i32 i64 f32 f64 ty op =
  match ty with
  | Ty_int -> int op
  | Ty_real -> real op
  | Ty_bitv 32 -> i32 op
  | Ty_bitv 64 -> i64 op
  | Ty_fp 32 -> f32 op
  | Ty_fp 64 -> f64 op
  | Ty_bool | Ty_str | _ -> assert false

let eval_unop = op Int.unop Real.unop I32.unop I64.unop F32.unop F64.unop

let eval_binop = op Int.binop Real.binop I32.binop I64.binop F32.binop F64.binop

let eval_relop = op Int.relop Real.relop I32.relop I64.relop F32.relop F64.relop

let eval_cvtop =
  op IntCvtOp.cvtop Real.cvtop I32CvtOp.cvtop I64CvtOp.cvtop F32CvtOp.cvtop
    F64CvtOp.cvtop

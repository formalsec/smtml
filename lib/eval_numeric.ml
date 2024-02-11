(* Adapted from: *)
(* - https://github.com/WebAssembly/spec/blob/main/interpreter/exec/ixx.ml, *)
(* - https://github.com/WebAssembly/spec/blob/main/interpreter/exec/fxx.ml, and *)
(* - https://github.com/WebAssembly/spec/blob/main/interpreter/exec *)

(* TODO: This module should be eval_concrete or a part of the reducer *)

open Ty

exception Num of Ty.t

exception Type_error of int * Num.t * Ty.t

exception Integer_overflow

exception Conversion_to_integer

let of_arg f n v = try f v with Num t -> raise (Type_error (n, v, t))
[@@inline]

module I32Op = struct
  type t = int32

  let to_value i : Num.t = I32 i [@@inline]

  let of_value n v : t =
    of_arg (function I32 i -> i | _ -> raise (Num (Ty_bitv 32))) n v
  [@@inline]

  let cmp_u x op y = op Int32.(add x min_int) Int32.(add y min_int)

  let lt_u x y = cmp_u x ( < ) y

  let le_u x y = cmp_u x ( <= ) y

  let gt_u x y = cmp_u x ( > ) y

  let ge_u x y = cmp_u x ( >= ) y

  let shift f x y = f x Int32.(to_int (logand y 31l))

  let shl x y = shift Int32.shift_left x y

  let shr_s x y = shift Int32.shift_right x y

  let shr_u x y = shift Int32.shift_right_logical x y

  let unop (op : unop) : Num.t -> Num.t =
    let f =
      match op with
      | Neg -> Int32.neg
      | Not -> Int32.lognot
      | Clz ->
        fun n -> Int32.of_int (Ocaml_intrinsics.Int32.count_leading_zeros n)
      | _ -> Log.err {|eval_unop: Unsupported i32 operator "%a"|} Ty.pp_unop op
    in
    fun v -> to_value (f (of_value 1 v))

  let binop (op : binop) : Num.t -> Num.t -> Num.t =
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
      | Rotl | Rotr | _ ->
        Log.err {|eval_binop: Unsupported i32 operator "%a"|} Ty.pp_binop op
    in
    fun v1 v2 -> to_value (f (of_value 1 v1) (of_value 2 v2))

  let relop (op : relop) : Num.t -> Num.t -> bool =
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
    fun v1 v2 -> f (of_value 1 v1) (of_value 2 v2)
end

module I64Op = struct
  let to_value i : Num.t = I64 i [@@inline]

  let of_value n v : int64 =
    of_arg (function I64 i -> i | _ -> raise (Num (Ty_bitv 64))) n v
  [@@inline]

  let cmp_u x op y = op Int64.(add x min_int) Int64.(add y min_int)

  let lt_u x y = cmp_u x ( < ) y

  let le_u x y = cmp_u x ( <= ) y

  let gt_u x y = cmp_u x ( > ) y

  let ge_u x y = cmp_u x ( >= ) y

  let shift f x y = f x Int64.(to_int (logand y 63L))

  let shl x y = shift Int64.shift_left x y

  let shr_s x y = shift Int64.shift_right x y

  let shr_u x y = shift Int64.shift_right_logical x y

  let unop (op : unop) : Num.t -> Num.t =
    let f =
      match op with
      | Neg -> Int64.neg
      | Not -> Int64.lognot
      | Clz ->
        fun n -> Int64.of_int (Ocaml_intrinsics.Int64.count_leading_zeros n)
      | _ -> Log.err {|eval_unop: Unsupported i64 operator "%a"|} Ty.pp_unop op
    in
    fun v -> to_value (f (of_value 1 v))

  let binop (op : binop) : Num.t -> Num.t -> Num.t =
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
      | Rotl | Rotr | _ ->
        Log.err {|eval_binop: Unsupported i64 operator "%a"|} Ty.pp_binop op
    in
    fun v1 v2 -> to_value (f (of_value 1 v1) (of_value 2 v2))

  let relop (op : relop) : Num.t -> Num.t -> bool =
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
    fun v1 v2 -> f (of_value 1 v1) (of_value 2 v2)
end

module F32Op = struct
  let to_value f : Num.t = F32 f [@@inline]

  let of_value i v =
    of_arg (function F32 f -> f | _ -> raise (Num (Ty_fp 32))) i v
  [@@inline]

  let of_float f = Int32.bits_of_float f [@@inline]

  let to_float f = Int32.float_of_bits f [@@inline]

  let unop (op : unop) =
    let f =
      match op with
      | Neg -> Float.neg
      | Abs -> Float.abs
      | Sqrt -> Float.sqrt
      | Nearest -> Float.round
      | Ceil -> Float.ceil
      | Floor -> Float.floor
      | Is_nan | _ ->
        Log.err {|eval_unop: Unsupported f32 operator "%a"|} Ty.pp_unop op
    in
    fun v -> to_value (of_float (f (to_float (of_value 1 v))))

  let binop (op : binop) =
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
    fun v1 v2 ->
      to_value
        (of_float (f (to_float (of_value 1 v1)) (to_float (of_value 2 v2))))

  let relop (op : relop) =
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
    fun v1 v2 -> f (to_float (of_value 1 v1)) (to_float (of_value 2 v2))
end

module F64Op = struct
  let to_value f : Num.t = F64 f [@@inline]

  let of_value i v =
    of_arg (function F64 f -> f | _ -> raise (Num (Ty_fp 64))) i v
  [@@inline]

  let of_float f = Int64.bits_of_float f [@@inline]

  let to_float f = Int64.float_of_bits f [@@inline]

  let unop (op : unop) =
    let f =
      match op with
      | Neg -> Float.neg
      | Abs -> Float.abs
      | Sqrt -> Float.sqrt
      | Nearest -> Float.round
      | Ceil -> Float.ceil
      | Floor -> Float.floor
      | Is_nan | _ ->
        Log.err {|eval_unop: Unsupported f32 operator "%a"|} Ty.pp_unop op
    in
    fun v -> to_value (of_float (f (to_float (of_value 1 v))))

  let binop (op : binop) =
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
    fun v1 v2 ->
      to_value
        (of_float (f (to_float (of_value 1 v1)) (to_float (of_value 2 v2))))

  let relop (op : relop) =
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
    fun v1 v2 -> f (to_float (of_value 1 v1)) (to_float (of_value 2 v2))
end

module I32CvtOp = struct
  let extend_s n x =
    let shift = 32 - n in
    Int32.(shift_right (shift_left x shift) shift)

  let trunc_f32_s (x : int32) =
    if x <> x then raise Conversion_to_integer
    else
      let xf = F32Op.to_float x in
      if xf >= -.Int32.(to_float min_int) || xf < Int32.(to_float min_int) then
        raise Integer_overflow
      else Int32.of_float xf

  let trunc_f32_u (x : int32) =
    if x <> x then raise Conversion_to_integer
    else
      let xf = F32Op.to_float x in
      if xf >= -.Int32.(to_float min_int) *. 2.0 || xf <= -1.0 then
        raise Integer_overflow
      else Int32.of_float xf

  let trunc_f64_s (x : int64) =
    if x <> x then raise Conversion_to_integer
    else
      let xf = F64Op.to_float x in
      if xf >= -.Int64.(to_float min_int) || xf < Int64.(to_float min_int) then
        raise Integer_overflow
      else Int32.of_float xf

  let trunc_f64_u (x : int64) =
    if x <> x then raise Conversion_to_integer
    else
      let xf = F64Op.to_float x in
      if xf >= -.Int64.(to_float min_int) *. 2.0 || xf <= -1.0 then
        raise Integer_overflow
      else Int32.of_float xf

  let cvtop op v : Num.t =
    match op with
    | WrapI64 -> I32 (Int64.to_int32 (I64Op.of_value 1 v))
    | TruncSF32 -> I32 (trunc_f32_s (F32Op.of_value 1 v))
    | TruncUF32 -> I32 (trunc_f32_u (F32Op.of_value 1 v))
    | TruncSF64 -> I32 (trunc_f64_s (F64Op.of_value 1 v))
    | TruncUF64 -> I32 (trunc_f64_u (F64Op.of_value 1 v))
    | Reinterpret_float -> I32 (F32Op.of_value 1 v)
    | ExtS n -> I32 (extend_s n (I32Op.of_value 1 v))
    | ExtU _n -> I32 (I32Op.of_value 1 v)
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
    if x <> x then raise Conversion_to_integer
    else
      let xf = F32Op.to_float x in
      if xf >= -.Int64.(to_float min_int) || xf < Int64.(to_float min_int) then
        raise Integer_overflow
      else Int64.of_float xf

  let trunc_f32_u x =
    if x <> x then raise Conversion_to_integer
    else
      let xf = F32Op.to_float x in
      if xf >= -.Int64.(to_float min_int) *. 2.0 || xf <= -1.0 then
        raise Integer_overflow
      else if xf >= -.Int64.(to_float min_int) then
        Int64.(logxor (of_float (xf -. 0x1p63)) min_int)
      else Int64.of_float xf

  let trunc_f64_s (x : int64) =
    if x <> x then raise Conversion_to_integer
    else
      let xf = F64Op.to_float x in
      if xf >= -.Int64.(to_float min_int) || xf < Int64.(to_float min_int) then
        raise Integer_overflow
      else Int64.of_float xf

  let trunc_f64_u x =
    if x <> x then raise Conversion_to_integer
    else
      let xf = F64Op.to_float x in
      if xf >= -.Int64.(to_float min_int) *. 2.0 || xf <= -1.0 then
        raise Integer_overflow
      else if xf >= -.Int64.(to_float min_int) then
        Int64.(logxor (of_float (xf -. 0x1p63)) min_int)
      else Int64.of_float xf

  let cvtop (op : cvtop) v : Num.t =
    match op with
    | ExtS 32 -> I64 (Int64.of_int32 (I32Op.of_value 1 v))
    | ExtU 32 -> I64 (extend_i32_u (I32Op.of_value 1 v))
    | TruncSF32 -> I64 (trunc_f32_s (F32Op.of_value 1 v))
    | TruncUF32 -> I64 (trunc_f32_u (F32Op.of_value 1 v))
    | TruncSF64 -> I64 (trunc_f64_s (F64Op.of_value 1 v))
    | TruncUF64 -> I64 (trunc_f64_u (F64Op.of_value 1 v))
    | Reinterpret_float -> I64 (F64Op.of_value 1 v)
    | WrapI64 -> raise (Type_error (1, v, Ty_bitv 64))
    | ToBool | OfBool | _ ->
      Log.err {|eval_cvtop: Unsupported i64 operator "%a"|} Ty.pp_cvtop op
end

module F32CvtOp = struct
  let demote_f64 x =
    let xf = F64Op.to_float x in
    if xf = xf then F32Op.of_float xf
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

  let convert_i32_s x = F32Op.of_float (Int32.to_float x)

  let convert_i32_u x =
    F32Op.of_float
      Int32.(
        if x >= 0l then to_float x
        else to_float (logor (shift_right_logical x 1) (logand x 1l)) *. 2.0 )

  let convert_i64_s x =
    F32Op.of_float
      Int64.(
        if abs x < 0x10_0000_0000_0000L then to_float x
        else
          let r = if logand x 0xfffL = 0L then 0L else 1L in
          to_float (logor (shift_right x 12) r) *. 0x1p12 )

  let convert_i64_u x =
    F32Op.of_float
      Int64.(
        if I64Op.lt_u x 0x10_0000_0000_0000L then to_float x
        else
          let r = if logand x 0xfffL = 0L then 0L else 1L in
          to_float (logor (shift_right_logical x 12) r) *. 0x1p12 )

  let cvtop (op : cvtop) v : Num.t =
    match op with
    | DemoteF64 -> F32 (demote_f64 (F64Op.of_value 1 v))
    | ConvertSI32 -> F32 (convert_i32_s (I32Op.of_value 1 v))
    | ConvertUI32 -> F32 (convert_i32_u (I32Op.of_value 1 v))
    | ConvertSI64 -> F32 (convert_i64_s (I64Op.of_value 1 v))
    | ConvertUI64 -> F32 (convert_i64_u (I64Op.of_value 1 v))
    | Reinterpret_int -> F32 (I32Op.of_value 1 v)
    | PromoteF32 -> raise (Type_error (1, v, Ty_fp 32))
    | ToString | OfString | _ ->
      Log.err {|eval_cvtop: Unsupported f32 operator "%a"|} Ty.pp_cvtop op
end

module F64CvtOp = struct
  let promote_f32 x =
    let xf = F32Op.to_float x in
    if xf = xf then F64Op.of_float xf
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

  let convert_i32_s x = F64Op.of_float (Int32.to_float x)

  (*
   * Unlike the other convert_u functions, the high half of the i32 range is
   * within the range where f32 can represent odd numbers, so we can't do the
   * shift. Instead, we can use int64 signed arithmetic.
   *)
  let convert_i32_u x =
    F64Op.of_float Int64.(to_float (logand (of_int32 x) 0x0000_0000_ffff_ffffL))

  let convert_i64_s x = F64Op.of_float (Int64.to_float x)

  (*
   * Values in the low half of the int64 range can be converted with a signed
   * conversion. The high half is beyond the range where f64 can represent odd
   * numbers, so we can shift the value right, adjust the least significant
   * bit to round correctly, do a conversion, and then scale it back up.
   *)
  let convert_i64_u (x : int64) =
    F64Op.of_float
      Int64.(
        if x >= 0L then to_float x
        else to_float (logor (shift_right_logical x 1) (logand x 1L)) *. 2.0 )

  let cvtop (op : cvtop) v : Num.t =
    match op with
    | PromoteF32 -> F64 (promote_f32 (F32Op.of_value 1 v))
    | ConvertSI32 -> F64 (convert_i32_s (I32Op.of_value 1 v))
    | ConvertUI32 -> F64 (convert_i32_u (I32Op.of_value 1 v))
    | ConvertSI64 -> F64 (convert_i64_s (I64Op.of_value 1 v))
    | ConvertUI64 -> F64 (convert_i64_u (I64Op.of_value 1 v))
    | Reinterpret_int -> F64 (I64Op.of_value 1 v)
    | DemoteF64 -> raise (Type_error (1, v, Ty_bitv 64))
    | ToString | OfString | _ ->
      Log.err {|eval_cvtop: Unsupported f64 operator "%a"|} Ty.pp_cvtop op
end

(* Dispatch *)

let op i32 i64 f32 f64 ty op =
  match ty with
  | Ty_int -> Log.err "eval_numeric: Integer evaluations not supported"
  | Ty_real -> Log.err "eval_numeric: Float evaluations not supported"
  | Ty_bitv 32 -> i32 op
  | Ty_bitv 64 -> i64 op
  | Ty_fp 32 -> f32 op
  | Ty_fp 64 -> f64 op
  | Ty_bool | Ty_str | _ -> assert false

let eval_unop = op I32Op.unop I64Op.unop F32Op.unop F64Op.unop

let eval_binop = op I32Op.binop I64Op.binop F32Op.binop F64Op.binop

let eval_relop = op I32Op.relop I64Op.relop F32Op.relop F64Op.relop

let eval_cvtop = op I32CvtOp.cvtop I64CvtOp.cvtop F32CvtOp.cvtop F64CvtOp.cvtop

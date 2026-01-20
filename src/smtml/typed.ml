type 'a ty = Ty.t

type 'a expr = Expr.t

type real

type bitv8

type bitv16

type bitv32

type bitv64

type bitv128

type float32

type float64

module Unsafe = struct
  let[@inline] wrap (x : Expr.t) : 'a expr = x

  let[@inline] unwrap (x : 'a expr) : Expr.t = x
end

let[@inline] view (x : 'a expr) : Expr.expr = Expr.view x

let[@inline] simplify (x : 'a expr) : 'a expr = Expr.simplify x

let[@inline] symbol (ty : 'a ty) (x : string) : 'a expr =
  Expr.symbol (Symbol.make ty x)

let[@inline] get_symbols (x : 'a expr list) : Symbol.t list = Expr.get_symbols x

let[@inline] ptr (base : int32) (offset : bitv32 expr) : bitv32 expr =
  Expr.ptr base offset

module Bitv = struct
  module type Width = sig
    type w

    val ty : Ty.t
  end

  module type S = sig
    type w

    type t = w expr

    val ty : w ty

    val zero : t

    val v : Bitvector.t -> t

    val symbol : Symbol.t -> t

    val clz : t -> t

    val ctz : t -> t

    val popcnt : t -> t

    val neg : t -> t

    val lognot : t -> t

    val to_int : signed:bool -> int expr -> t

    val add : t -> t -> t

    val sub : t -> t -> t

    val mul : t -> t -> t

    val div : t -> t -> t

    val unsigned_div : t -> t -> t

    val logor : t -> t -> t

    val logand : t -> t -> t

    val logxor : t -> t -> t

    val shl : t -> t -> t

    val ashr : t -> t -> t

    val lshr : t -> t -> t

    val rem : t -> t -> t

    val unsigned_rem : t -> t -> t

    val rotate_left : t -> t -> t

    val rotate_right : t -> t -> t

    val eq : t -> t -> bool expr

    val ne : t -> t -> bool expr

    val lt : t -> t -> bool expr

    val lt_u : t -> t -> bool expr

    val le : t -> t -> bool expr

    val le_u : t -> t -> bool expr

    val gt : t -> t -> bool expr

    val gt_u : t -> t -> bool expr

    val ge : t -> t -> bool expr

    val ge_u : t -> t -> bool expr

    val concat : 'a expr -> 'b expr -> 'c expr

    val extract : t -> high:int -> low:int -> 'a expr

    val zero_extend : int -> t -> 'a expr

    val sign_extend : int -> t -> 'a expr

    val to_bool : t -> bool expr

    val of_bool : bool expr -> t

    val pp : t Fmt.t
  end

  module Make (W : Width) = struct
    type w = W.w

    type t = w expr

    let ty = W.ty

    let zero =
      match ty with
      | Ty.Ty_bitv m -> Expr.value (Bitv (Bitvector.make Z.zero m))
      | _ ->
        (* This would be absurd *)
        assert false

    let v x = Expr.value (Bitv x)

    let symbol x = Expr.symbol x

    let[@inline] clz x = Expr.unop ty Clz x

    let[@inline] ctz x = Expr.unop ty Ctz x

    let[@inline] popcnt x = Expr.unop ty Popcnt x

    let[@inline] neg x = Expr.unop ty Neg x

    let[@inline] lognot x = Expr.unop ty Not x

    let[@inline] to_int ~signed:_ _x = assert false

    let[@inline] add x y = Expr.binop ty Add x y

    let[@inline] sub x y = Expr.binop ty Sub x y

    let[@inline] mul x y = Expr.binop ty Mul x y

    let[@inline] div x y = Expr.binop ty Div x y

    let[@inline] unsigned_div x y = Expr.binop ty DivU x y

    let[@inline] logor x y = Expr.binop ty Or x y

    let[@inline] logand x y = Expr.binop ty And x y

    let[@inline] logxor x y = Expr.binop ty Xor x y

    let[@inline] shl x y = Expr.binop ty Shl x y

    let[@inline] ashr x y = Expr.binop ty ShrA x y

    let[@inline] lshr x y = Expr.binop ty ShrL x y

    let[@inline] rem x y = Expr.binop ty Rem x y

    let[@inline] unsigned_rem x y = Expr.binop ty RemU x y

    let[@inline] rotate_left x y = Expr.binop ty Rotl x y

    let[@inline] rotate_right x y = Expr.binop ty Rotr x y

    let[@inline] eq a b = Expr.relop Ty_bool Eq a b

    let[@inline] ne a b = Expr.relop Ty_bool Ne a b

    let[@inline] lt x y = Expr.relop ty Lt x y

    let[@inline] lt_u x y = Expr.relop ty LtU x y

    let[@inline] le x y = Expr.relop ty Le x y

    let[@inline] le_u x y = Expr.relop ty LeU x y

    let[@inline] gt x y = Expr.relop ty Gt x y

    let[@inline] gt_u x y = Expr.relop ty GtU x y

    let[@inline] ge x y = Expr.relop ty Ge x y

    let[@inline] ge_u x y = Expr.relop ty GeU x y

    let[@inline] concat x y = Expr.concat x y

    let[@inline] extract x ~high ~low = Expr.extract x ~high ~low

    let[@inline] zero_extend m x = Expr.cvtop ty (Zero_extend m) x

    let[@inline] sign_extend m x = Expr.cvtop ty (Sign_extend m) x

    let[@inline] to_bool x = Expr.cvtop ty ToBool x

    let[@inline] of_bool x = Expr.cvtop ty OfBool x

    let pp fmt x = Expr.pp fmt x
  end
end

module Bitv8 = Bitv.Make (struct
  type w = bitv8

  let ty = Ty.Ty_bitv 8
end)

module Bitv16 = Bitv.Make (struct
  type w = bitv16

  let ty = Ty.Ty_bitv 16
end)

module Bitv32 = struct
  include Bitv.Make (struct
    type w = bitv32

    let ty = Ty.Ty_bitv 32
  end)

  let[@inline] of_int32 x = v (Bitvector.of_int32 x)

  let[@inline] of_int8_s x = Expr.cvtop ty (Sign_extend 24) x

  let[@inline] of_int8_u x = Expr.cvtop ty (Zero_extend 24) x

  let[@inline] of_int16_s x = Expr.cvtop ty (Sign_extend 16) x

  let[@inline] of_int16_u x = Expr.cvtop ty (Zero_extend 16) x

  let[@inline] to_bytes x =
    [ extract x ~high:1 ~low:0
    ; extract x ~high:2 ~low:1
    ; extract x ~high:3 ~low:2
    ; extract x ~high:4 ~low:3
    ]

  let[@inline] trunc_f32_s_exn x = Expr.cvtop ty TruncSF32 x

  let[@inline] trunc_f32_u_exn x = Expr.cvtop ty TruncUF32 x

  let[@inline] trunc_f64_s_exn x = Expr.cvtop ty TruncSF64 x

  let[@inline] trunc_f64_u_exn x = Expr.cvtop ty TruncUF64 x

  let[@inline] trunc_f32_s x =
    try Ok (trunc_f32_s_exn x)
    with
    | Eval.Eval_error ((`Integer_overflow | `Conversion_to_integer) as e) ->
      Error e

  let[@inline] trunc_f32_u x =
    try Ok (trunc_f32_u_exn x)
    with
    | Eval.Eval_error ((`Integer_overflow | `Conversion_to_integer) as e) ->
      Error e

  let[@inline] trunc_f64_s x =
    try Ok (trunc_f64_s_exn x)
    with
    | Eval.Eval_error ((`Integer_overflow | `Conversion_to_integer) as e) ->
      Error e

  let[@inline] trunc_f64_u x =
    try Ok (trunc_f64_u_exn x)
    with
    | Eval.Eval_error ((`Integer_overflow | `Conversion_to_integer) as e) ->
      Error e

  let[@inline] trunc_sat_f32_s x = Expr.cvtop ty Trunc_sat_f32_s x

  let[@inline] trunc_sat_f32_u x = Expr.cvtop ty Trunc_sat_f32_u x

  let[@inline] trunc_sat_f64_s x = Expr.cvtop ty Trunc_sat_f64_s x

  let[@inline] trunc_sat_f64_u x = Expr.cvtop ty Trunc_sat_f64_u x

  let[@inline] reinterpret_f32 x = Expr.cvtop ty Reinterpret_float x

  let[@inline] wrap_i64 x = Expr.cvtop ty WrapI64 x

  let[@inline] extend_s n x =
    Expr.cvtop ty (Sign_extend (32 - n)) (Expr.extract x ~high:(n / 8) ~low:0)
end

module Bitv64 = struct
  include Bitv.Make (struct
    type w = bitv64

    let ty = Ty.Ty_bitv 64
  end)

  let[@inline] of_int64 x = v (Bitvector.of_int64 x)

  let[@inline] of_int32 x = Expr.cvtop ty (Sign_extend 32) x

  let[@inline] to_int32 x = Expr.cvtop Bitv32.ty WrapI64 x

  let[@inline] to_bytes x =
    [ extract x ~high:1 ~low:0
    ; extract x ~high:2 ~low:1
    ; extract x ~high:3 ~low:2
    ; extract x ~high:4 ~low:3
    ; extract x ~high:5 ~low:4
    ; extract x ~high:6 ~low:5
    ; extract x ~high:7 ~low:6
    ; extract x ~high:8 ~low:7
    ]

  let[@inline] trunc_f32_s_exn x = Expr.cvtop ty TruncSF32 x

  let[@inline] trunc_f32_u_exn x = Expr.cvtop ty TruncUF32 x

  let[@inline] trunc_f64_s_exn x = Expr.cvtop ty TruncSF64 x

  let[@inline] trunc_f64_u_exn x = Expr.cvtop ty TruncUF64 x

  let[@inline] trunc_f32_s x =
    try Ok (trunc_f32_s_exn x)
    with
    | Eval.Eval_error ((`Integer_overflow | `Conversion_to_integer) as e) ->
      Error e

  let[@inline] trunc_f32_u x =
    try Ok (trunc_f32_u_exn x)
    with
    | Eval.Eval_error ((`Integer_overflow | `Conversion_to_integer) as e) ->
      Error e

  let[@inline] trunc_f64_s x =
    try Ok (trunc_f64_s_exn x)
    with
    | Eval.Eval_error ((`Integer_overflow | `Conversion_to_integer) as e) ->
      Error e

  let[@inline] trunc_f64_u x =
    try Ok (trunc_f64_u_exn x)
    with
    | Eval.Eval_error ((`Integer_overflow | `Conversion_to_integer) as e) ->
      Error e

  let[@inline] trunc_sat_f32_s x = Expr.cvtop ty Trunc_sat_f32_s x

  let[@inline] trunc_sat_f32_u x = Expr.cvtop ty Trunc_sat_f32_u x

  let[@inline] trunc_sat_f64_s x = Expr.cvtop ty Trunc_sat_f64_s x

  let[@inline] trunc_sat_f64_u x = Expr.cvtop ty Trunc_sat_f64_u x

  let[@inline] reinterpret_f64 x = Expr.cvtop ty Reinterpret_float x

  let[@inline] extend_s n x =
    Expr.cvtop ty (Sign_extend (64 - n)) (Expr.extract x ~high:(n / 8) ~low:0)

  let[@inline] extend_i32_s x = Expr.cvtop ty (Sign_extend 32) x

  let[@inline] extend_i32_u x = Expr.cvtop ty (Zero_extend 32) x
end

module Bitv128 = struct
  include Bitv.Make (struct
    type w = bitv128

    let ty = Ty.Ty_bitv 128
  end)

  let of_i32x4 a b c d = Bitv64.concat (Bitv32.concat a b) (Bitv32.concat c d)

  let to_i32x4 v =
    let a = extract v ~low:12 ~high:16 in
    let b = extract v ~low:8 ~high:12 in
    let c = extract v ~low:4 ~high:8 in
    let d = extract v ~low:0 ~high:4 in
    (a, b, c, d)

  let of_i64x2 a b = Bitv64.concat a b

  let to_i64x2 v =
    let a = extract v ~low:8 ~high:16 in
    let b = extract v ~low:0 ~high:8 in
    (a, b)
end

module Types = struct
  let int : int ty = Ty_int

  let real : real ty = Ty_real

  let bool : bool ty = Ty_bool

  let string : string ty = Ty_str

  let bitv32 : Bitv32.w ty = Bitv32.ty

  let bitv64 : Bitv64.w ty = Bitv64.ty

  let bitv128 : Bitv128.w ty = Bitv128.ty

  let float32 : float32 ty = Ty_fp 32

  let float64 : float64 ty = Ty_fp 64

  let pp fmt ty = Ty.pp fmt ty

  let[@inline] to_ty (ty : 'a ty) : Ty.t = ty
end

module Bool = struct
  type t = bool expr

  let true_ = Expr.value True

  let false_ = Expr.value False

  let of_bool x = if x then true_ else false_

  let[@inline] symbol x = Expr.symbol x

  let[@inline] pp fmt x = Expr.pp fmt x

  let[@inline] not e = Expr.Bool.not e

  let[@inline] and_ a b = Expr.Bool.and_ a b

  let[@inline] or_ a b = Expr.Bool.or_ a b

  let[@inline] logand es = Expr.naryop Types.bool Logand es

  let[@inline] logor es = Expr.naryop Types.bool Logor es

  let[@inline] xor a b = Expr.binop Types.bool Xor a b

  let[@inline] implies a b = or_ (not a) b

  let[@inline] eq (a : 'a expr) (b : 'a expr) = Expr.relop Types.bool Eq a b

  let[@inline] ite c (r1 : 'a expr) (r2 : 'a expr) : 'a expr =
    Expr.triop Types.bool Ite c r1 r2

  let[@inline] split_conjunctions x = Expr.split_conjunctions x
end

module Int = struct
  type t = int expr

  let[@inline] v x = Expr.value (Int x)

  let[@inline] symbol x = Expr.symbol x

  let[@inline] pp fmt x = Expr.pp fmt x

  let[@inline] neg x = Expr.unop Types.int Neg x

  let[@inline] to_real x = Expr.cvtop Types.real Reinterpret_int x

  let[@inline] add x y = Expr.binop Types.int Add x y

  let[@inline] sub x y = Expr.binop Types.int Sub x y

  let[@inline] mul x y = Expr.binop Types.int Mul x y

  let[@inline] div x y = Expr.binop Types.int Div x y

  let[@inline] rem x y = Expr.binop Types.int Rem x y

  let[@inline] mod_ _x _y = assert false

  let[@inline] pow x y = Expr.binop Types.int Pow x y

  let[@inline] eq a b = Expr.relop Types.bool Eq a b

  let[@inline] lt x y = Expr.relop Types.int Lt x y

  let[@inline] le x y = Expr.relop Types.int Le x y

  let[@inline] gt x y = Expr.relop Types.int Gt x y

  let[@inline] ge x y = Expr.relop Types.int Ge x y
end

module Real = struct
  type t = real expr

  let[@inline] v x = Expr.value (Real x)

  let[@inline] symbol x = Expr.symbol x

  let[@inline] pp fmt x = Expr.pp fmt x

  let[@inline] neg x = Expr.unop Types.real Neg x

  let[@inline] to_int x = Expr.cvtop Ty_int Reinterpret_float x

  let[@inline] add x y = Expr.binop Types.real Add x y

  let[@inline] sub x y = Expr.binop Types.real Sub x y

  let[@inline] mul x y = Expr.binop Types.real Mul x y

  let[@inline] div x y = Expr.binop Types.real Div x y

  let[@inline] pow x y = Expr.binop Types.real Pow x y

  let[@inline] eq a b = Expr.relop Types.bool Eq a b

  let[@inline] lt x y = Expr.relop Types.real Lt x y

  let[@inline] le x y = Expr.relop Types.real Le x y

  let[@inline] gt x y = Expr.relop Types.real Gt x y

  let[@inline] ge x y = Expr.relop Types.real Ge x y
end

module String = struct
  type t = string expr

  let[@inline] v x = Expr.value (Str x)

  let[@inline] symbol x = Expr.symbol x

  let[@inline] pp fmt x = Expr.pp fmt x

  let[@inline] length x = Expr.unop Types.string Length x

  let[@inline] to_code x = Expr.cvtop Types.string String_to_code x

  let[@inline] of_code x = Expr.cvtop Types.string String_from_code x

  let[@inline] to_int x = Expr.cvtop Types.string String_to_int x

  let[@inline] of_int x = Expr.cvtop Types.string String_from_int x

  let[@inline] at x ~pos = Expr.binop Types.string At x pos

  let[@inline] concat xs = Expr.naryop Types.string Concat xs

  let[@inline] contains x ~sub = Expr.binop Types.string String_contains x sub

  let[@inline] is_prefix x ~prefix =
    Expr.binop Types.string String_prefix x prefix

  let[@inline] is_suffix x ~suffix =
    Expr.binop Types.string String_suffix x suffix

  let[@inline] eq a b = Expr.relop Types.bool Eq a b

  let[@inline] lt x y = Expr.relop Types.string Lt x y

  let[@inline] le x y = Expr.relop Types.string Le x y

  let[@inline] sub x ~pos ~len =
    Expr.triop Types.string String_extract x pos len

  let[@inline] index_of x ~sub ~pos =
    Expr.triop Types.string String_index x sub pos

  let[@inline] replace x ~pattern ~with_ =
    Expr.triop Types.string String_replace x pattern with_

  let[@inline] replace_all x ~pattern ~with_ =
    Expr.triop Types.string String_replace_all x pattern with_
end

module Float32 = struct
  type t = float32 expr

  let[@inline] v f = Expr.value (Num (F32 f))

  let[@inline] of_float x = v (Int32.bits_of_float x)

  let[@inline] of_int32_bits f = v f

  let[@inline] symbol x = Expr.symbol x

  let[@inline] pp fmt x = Expr.pp fmt x

  let[@inline] neg x = Expr.unop Types.float32 Neg x

  let[@inline] abs x = Expr.unop Types.float32 Abs x

  let[@inline] sqrt x = Expr.unop Types.float32 Sqrt x

  let[@inline] is_normal x = Expr.unop Types.float32 Is_normal x

  let[@inline] is_subnormal x = Expr.unop Types.float32 Is_subnormal x

  let[@inline] is_negative x = Expr.unop Types.float32 Is_negative x

  let[@inline] is_positive x = Expr.unop Types.float32 Is_positive x

  let[@inline] is_infinite x = Expr.unop Types.float32 Is_infinite x

  let[@inline] is_zero x = Expr.unop Types.float32 Is_zero x

  let[@inline] is_nan x = Expr.unop Types.float32 Is_nan x

  let[@inline] ceil x = Expr.unop Types.float32 Ceil x

  let[@inline] floor x = Expr.unop Types.float32 Floor x

  let[@inline] trunc x = Expr.unop Types.float32 Trunc x

  let[@inline] nearest x = Expr.unop Types.float32 Nearest x

  let[@inline] add x y = Expr.binop Types.float32 Add x y

  let[@inline] sub x y = Expr.binop Types.float32 Sub x y

  let[@inline] mul x y = Expr.binop Types.float32 Mul x y

  let[@inline] div x y = Expr.binop Types.float32 Div x y

  let[@inline] min x y = Expr.binop Types.float32 Min x y

  let[@inline] max x y = Expr.binop Types.float32 Max x y

  let[@inline] rem x y = Expr.binop Types.float32 Rem x y

  let[@inline] copy_sign x y = Expr.binop Types.float32 Copysign x y

  let[@inline] eq x y = Expr.relop Types.float32 Eq x y

  let[@inline] ne x y = Expr.relop Types.float32 Ne x y

  let[@inline] lt x y = Expr.relop Types.float32 Lt x y

  let[@inline] le x y = Expr.relop Types.float32 Le x y

  let[@inline] gt x y = Expr.relop Types.float32 Gt x y

  let[@inline] ge x y = Expr.relop Types.float32 Ge x y

  let[@inline] convert_i32_s x = Expr.cvtop Types.float32 ConvertSI32 x

  let[@inline] convert_i32_u x = Expr.cvtop Types.float32 ConvertUI32 x

  let[@inline] convert_i64_s x = Expr.cvtop Types.float32 ConvertSI64 x

  let[@inline] convert_i64_u x = Expr.cvtop Types.float32 ConvertUI64 x

  let[@inline] demote_f64 x = Expr.cvtop Types.float32 DemoteF64 x

  let[@inline] reinterpret_i32 x = Expr.cvtop Types.float32 Reinterpret_int x

  let[@inline] to_bv x = Expr.cvtop Types.bitv32 Reinterpret_float x
end

module Float64 = struct
  type t = float64 expr

  let[@inline] v f = Expr.value (Num (F64 f))

  let[@inline] of_float x = v (Int64.bits_of_float x)

  let[@inline] symbol x = Expr.symbol x

  let[@inline] pp fmt x = Expr.pp fmt x

  let[@inline] neg x = Expr.unop Types.float64 Neg x

  let[@inline] abs x = Expr.unop Types.float64 Abs x

  let[@inline] sqrt x = Expr.unop Types.float64 Sqrt x

  let[@inline] is_normal x = Expr.unop Types.float64 Is_normal x

  let[@inline] is_subnormal x = Expr.unop Types.float64 Is_subnormal x

  let[@inline] is_negative x = Expr.unop Types.float64 Is_negative x

  let[@inline] is_positive x = Expr.unop Types.float64 Is_positive x

  let[@inline] is_infinite x = Expr.unop Types.float64 Is_infinite x

  let[@inline] is_zero x = Expr.unop Types.float64 Is_zero x

  let[@inline] is_nan x = Expr.unop Types.float64 Is_nan x

  let[@inline] ceil x = Expr.unop Types.float64 Ceil x

  let[@inline] floor x = Expr.unop Types.float64 Floor x

  let[@inline] trunc x = Expr.unop Types.float64 Trunc x

  let[@inline] nearest x = Expr.unop Types.float64 Nearest x

  let[@inline] add x y = Expr.binop Types.float64 Add x y

  let[@inline] sub x y = Expr.binop Types.float64 Sub x y

  let[@inline] mul x y = Expr.binop Types.float64 Mul x y

  let[@inline] div x y = Expr.binop Types.float64 Div x y

  let[@inline] min x y = Expr.binop Types.float64 Min x y

  let[@inline] max x y = Expr.binop Types.float64 Max x y

  let[@inline] rem x y = Expr.binop Types.float64 Rem x y

  let[@inline] copy_sign x y = Expr.binop Types.float64 Copysign x y

  let[@inline] eq x y = Expr.relop Types.float64 Eq x y

  let[@inline] ne x y = Expr.relop Types.float64 Ne x y

  let[@inline] lt x y = Expr.relop Types.float64 Lt x y

  let[@inline] le x y = Expr.relop Types.float64 Le x y

  let[@inline] gt x y = Expr.relop Types.float64 Gt x y

  let[@inline] ge x y = Expr.relop Types.float64 Ge x y

  let[@inline] convert_i32_s x = Expr.cvtop Types.float64 ConvertSI32 x

  let[@inline] convert_i32_u x = Expr.cvtop Types.float64 ConvertUI32 x

  let[@inline] convert_i64_s x = Expr.cvtop Types.float64 ConvertSI64 x

  let[@inline] convert_i64_u x = Expr.cvtop Types.float64 ConvertUI64 x

  let[@inline] promote_f32 x = Expr.cvtop Types.float64 PromoteF32 x

  let[@inline] reinterpret_i64 x = Expr.cvtop Types.float64 Reinterpret_int x

  let[@inline] to_bv x = Expr.cvtop Types.bitv64 Reinterpret_float x
end

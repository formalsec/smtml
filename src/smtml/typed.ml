type 'a ty = Ty.t

type 'a expr = Expr.t

type real

type regexp

type bitv8

type bitv16

type bitv32

type bitv64

type bitv128

type float32

type float64

module Unsafe = struct
  external wrap : Expr.t -> 'a expr = "%identity"

  external unwrap : 'a expr -> Expr.t = "%identity"
end

let[@inline] view (x : 'a expr) : Expr.expr = Expr.view x

let[@inline] simplify (x : 'a expr) : 'a expr = Expr.simplify x

let[@inline] symbol (ty : 'a ty) (x : string) : 'a expr =
  Expr.symbol (Symbol.make ty x)

let[@inline] get_symbols (x : 'a expr list) : Symbol.t list = Expr.get_symbols x

let[@inline] ptr (base : int32) (offset : bitv32 expr) : bitv32 expr =
  Expr.ptr base offset

let ty_bool : bool ty = Ty_bool

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

  let[@inline] logand es = Expr.naryop ty_bool Logand es

  let[@inline] logor es = Expr.naryop ty_bool Logor es

  let[@inline] xor a b = Expr.binop ty_bool Xor a b

  let[@inline] implies a b = Expr.Bool.implies a b

  let[@inline] eq (a : 'a expr) (b : 'a expr) = Expr.relop ty_bool Eq a b

  let[@inline] distinct (es : 'a expr list) =
    (* Typically this encodes a symbolic constraint: (distinct x y z), so no
       need to waste time trying to simplify. Just use `raw_naryop`. *)
    Expr.raw_naryop ty_bool Distinct es

  let[@inline] ite c (r1 : 'a expr) (r2 : 'a expr) : 'a expr =
    Expr.triop ty_bool Ite c r1 r2

  let[@inline] split_conjunctions x = Expr.split_conjunctions x
end

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

    val one : t

    val v : Bitvector.t -> t

    val of_int : int -> t

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

    val rotate_left : int -> t -> t

    val rotate_right : int -> t -> t

    val ext_rotate_left : t -> t -> t

    val ext_rotate_right : t -> t -> t

    val eq : t -> t -> bool expr

    val ne : t -> t -> bool expr

    val lt : t -> t -> bool expr

    val lt_u : t -> t -> bool expr

    val le : t -> t -> bool expr

    val le_u : t -> t -> bool expr

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

    let one =
      match ty with
      | Ty.Ty_bitv m -> Expr.value (Bitv (Bitvector.make Z.one m))
      | _ ->
        (* This would be absurd *)
        assert false

    let v x = Expr.value (Bitv x)

    let of_int =
      let m =
        match ty with
        | Ty.Ty_bitv m -> m
        | _ ->
          (* This would be absurd *)
          assert false
      in
      fun x -> Expr.value (Bitv (Bitvector.make (Z.of_int x) m))

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

    let[@inline] rotate_left n x = Expr.unop ty (Rotl n) x

    let[@inline] rotate_right n x = Expr.unop ty (Rotr n) x

    let[@inline] ext_rotate_left x y =
      match Expr.view y with
      | Val (Bitv bv) ->
        let n = Bitvector.to_signed bv |> Z.to_int in
        Expr.unop ty (Rotl n) x
      | Val (Int n) -> Expr.unop ty (Rotl n) x
      | _ -> Expr.binop ty Ext_rotl x y

    let[@inline] ext_rotate_right x y =
      match Expr.view y with
      | Val (Bitv bv) ->
        let n = Bitvector.to_signed bv |> Z.to_int in
        Expr.unop ty (Rotr n) x
      | Val (Int n) -> Expr.unop ty (Rotr n) x
      | _ -> Expr.binop ty Ext_rotr x y

    let[@inline] eq a b = Expr.relop Ty_bool Eq a b

    let[@inline] ne a b = Expr.relop Ty_bool Ne a b

    let[@inline] lt x y = Expr.relop ty Lt x y

    let[@inline] lt_u x y = Expr.relop ty LtU x y

    let[@inline] le x y = Expr.relop ty Le x y

    let[@inline] le_u x y = Expr.relop ty LeU x y

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
    [ extract x ~high:7 ~low:0
    ; extract x ~high:15 ~low:8
    ; extract x ~high:23 ~low:16
    ; extract x ~high:31 ~low:24
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
    Expr.cvtop ty (Sign_extend (32 - n)) (Expr.extract x ~high:(n - 1) ~low:0)
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
    [ extract x ~high:7 ~low:0
    ; extract x ~high:15 ~low:8
    ; extract x ~high:23 ~low:16
    ; extract x ~high:31 ~low:24
    ; extract x ~high:39 ~low:32
    ; extract x ~high:47 ~low:40
    ; extract x ~high:55 ~low:48
    ; extract x ~high:63 ~low:56
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
    Expr.cvtop ty (Sign_extend (64 - n)) (Expr.extract x ~high:(n - 1) ~low:0)

  let[@inline] extend_i32_s x = Expr.cvtop ty (Sign_extend 32) x

  let[@inline] extend_i32_u x = Expr.cvtop ty (Zero_extend 32) x
end

module Types_inner = struct
  let int : int ty = Ty_int

  let real : real ty = Ty_real

  let regexp : regexp ty = Ty_regexp

  let bool : bool ty = ty_bool

  let string : string ty = Ty_str

  let bitv8 : Bitv8.w ty = Bitv8.ty

  let bitv16 : Bitv16.w ty = Bitv16.ty

  let bitv32 : Bitv32.w ty = Bitv32.ty

  let bitv64 : Bitv64.w ty = Bitv64.ty

  let float32 : float32 ty = Ty_fp 32

  let float64 : float64 ty = Ty_fp 64

  let pp fmt ty = Ty.pp fmt ty

  let[@inline] to_ty (ty : 'a ty) : Ty.t = ty
end

module Float32 = struct
  type t = float32 expr

  let zero = Expr.value (Num (F32 0l))

  let[@inline] v f = Expr.value (Num (F32 f))

  let[@inline] of_float x = v (Int32.bits_of_float x)

  let[@inline] of_int32_bits f = v f

  let[@inline] symbol x = Expr.symbol x

  let[@inline] pp fmt x = Expr.pp fmt x

  let[@inline] neg x = Expr.unop Types_inner.float32 Neg x

  let[@inline] abs x = Expr.unop Types_inner.float32 Abs x

  let[@inline] sqrt x = Expr.unop Types_inner.float32 Sqrt x

  let[@inline] is_normal x = Expr.unop Types_inner.float32 Is_normal x

  let[@inline] is_subnormal x = Expr.unop Types_inner.float32 Is_subnormal x

  let[@inline] is_negative x = Expr.unop Types_inner.float32 Is_negative x

  let[@inline] is_positive x = Expr.unop Types_inner.float32 Is_positive x

  let[@inline] is_infinite x = Expr.unop Types_inner.float32 Is_infinite x

  let[@inline] is_zero x = Expr.unop Types_inner.float32 Is_zero x

  let[@inline] is_nan x = Expr.unop Types_inner.float32 Is_nan x

  let[@inline] ceil x = Expr.unop Types_inner.float32 Ceil x

  let[@inline] floor x = Expr.unop Types_inner.float32 Floor x

  let[@inline] trunc x = Expr.unop Types_inner.float32 Trunc x

  let[@inline] nearest x = Expr.unop Types_inner.float32 Nearest x

  let[@inline] add x y = Expr.binop Types_inner.float32 Add x y

  let[@inline] sub x y = Expr.binop Types_inner.float32 Sub x y

  let[@inline] mul x y = Expr.binop Types_inner.float32 Mul x y

  let[@inline] div x y = Expr.binop Types_inner.float32 Div x y

  let[@inline] min x y = Expr.binop Types_inner.float32 Min x y

  let[@inline] max x y = Expr.binop Types_inner.float32 Max x y

  let[@inline] rem x y = Expr.binop Types_inner.float32 Rem x y

  let[@inline] copy_sign x y = Expr.binop Types_inner.float32 Copysign x y

  let[@inline] eq x y = Expr.relop Types_inner.float32 Eq x y

  let[@inline] ne x y = Expr.relop Types_inner.float32 Ne x y

  let[@inline] lt x y = Expr.relop Types_inner.float32 Lt x y

  let[@inline] le x y = Expr.relop Types_inner.float32 Le x y

  let[@inline] convert_i32_s x = Expr.cvtop Types_inner.float32 ConvertSI32 x

  let[@inline] convert_i32_u x = Expr.cvtop Types_inner.float32 ConvertUI32 x

  let[@inline] convert_i64_s x = Expr.cvtop Types_inner.float32 ConvertSI64 x

  let[@inline] convert_i64_u x = Expr.cvtop Types_inner.float32 ConvertUI64 x

  let[@inline] demote_f64 x = Expr.cvtop Types_inner.float32 DemoteF64 x

  let[@inline] reinterpret_i32 x =
    Expr.cvtop Types_inner.float32 Reinterpret_int x

  let[@inline] to_bv x = Expr.cvtop Types_inner.bitv32 Reinterpret_float x

  let[@inline] pmin x y = Bool.ite (lt y x) y x

  let[@inline] pmax x y = Bool.ite (lt x y) y x
end

module Float64 = struct
  type t = float64 expr

  let zero = Expr.value (Num (F64 0L))

  let[@inline] v f = Expr.value (Num (F64 f))

  let[@inline] of_float x = v (Int64.bits_of_float x)

  let[@inline] symbol x = Expr.symbol x

  let[@inline] pp fmt x = Expr.pp fmt x

  let[@inline] neg x = Expr.unop Types_inner.float64 Neg x

  let[@inline] abs x = Expr.unop Types_inner.float64 Abs x

  let[@inline] sqrt x = Expr.unop Types_inner.float64 Sqrt x

  let[@inline] is_normal x = Expr.unop Types_inner.float64 Is_normal x

  let[@inline] is_subnormal x = Expr.unop Types_inner.float64 Is_subnormal x

  let[@inline] is_negative x = Expr.unop Types_inner.float64 Is_negative x

  let[@inline] is_positive x = Expr.unop Types_inner.float64 Is_positive x

  let[@inline] is_infinite x = Expr.unop Types_inner.float64 Is_infinite x

  let[@inline] is_zero x = Expr.unop Types_inner.float64 Is_zero x

  let[@inline] is_nan x = Expr.unop Types_inner.float64 Is_nan x

  let[@inline] ceil x = Expr.unop Types_inner.float64 Ceil x

  let[@inline] floor x = Expr.unop Types_inner.float64 Floor x

  let[@inline] trunc x = Expr.unop Types_inner.float64 Trunc x

  let[@inline] nearest x = Expr.unop Types_inner.float64 Nearest x

  let[@inline] add x y = Expr.binop Types_inner.float64 Add x y

  let[@inline] sub x y = Expr.binop Types_inner.float64 Sub x y

  let[@inline] mul x y = Expr.binop Types_inner.float64 Mul x y

  let[@inline] div x y = Expr.binop Types_inner.float64 Div x y

  let[@inline] min x y = Expr.binop Types_inner.float64 Min x y

  let[@inline] max x y = Expr.binop Types_inner.float64 Max x y

  let[@inline] rem x y = Expr.binop Types_inner.float64 Rem x y

  let[@inline] copy_sign x y = Expr.binop Types_inner.float64 Copysign x y

  let[@inline] eq x y = Expr.relop Types_inner.float64 Eq x y

  let[@inline] ne x y = Expr.relop Types_inner.float64 Ne x y

  let[@inline] lt x y = Expr.relop Types_inner.float64 Lt x y

  let[@inline] le x y = Expr.relop Types_inner.float64 Le x y

  let[@inline] convert_i32_s x = Expr.cvtop Types_inner.float64 ConvertSI32 x

  let[@inline] convert_i32_u x = Expr.cvtop Types_inner.float64 ConvertUI32 x

  let[@inline] convert_i64_s x = Expr.cvtop Types_inner.float64 ConvertSI64 x

  let[@inline] convert_i64_u x = Expr.cvtop Types_inner.float64 ConvertUI64 x

  let[@inline] promote_f32 x = Expr.cvtop Types_inner.float64 PromoteF32 x

  let[@inline] reinterpret_i64 x =
    Expr.cvtop Types_inner.float64 Reinterpret_int x

  let[@inline] to_bv x = Expr.cvtop Types_inner.bitv64 Reinterpret_float x

  let[@inline] pmin x y = Bool.ite (lt y x) y x

  let[@inline] pmax x y = Bool.ite (lt x y) y x
end

module Bitv128 = struct
  include Bitv.Make (struct
    type w = bitv128

    let ty = Ty.Ty_bitv 128
  end)

  let[@inline] to_bytes x =
    [ extract x ~high:7 ~low:0
    ; extract x ~high:15 ~low:8
    ; extract x ~high:23 ~low:16
    ; extract x ~high:31 ~low:24
    ; extract x ~high:39 ~low:32
    ; extract x ~high:47 ~low:40
    ; extract x ~high:55 ~low:48
    ; extract x ~high:63 ~low:56
    ; extract x ~high:71 ~low:64
    ; extract x ~high:79 ~low:72
    ; extract x ~high:87 ~low:80
    ; extract x ~high:95 ~low:88
    ; extract x ~high:103 ~low:96
    ; extract x ~high:111 ~low:104
    ; extract x ~high:119 ~low:112
    ; extract x ~high:127 ~low:120
    ]

  let of_i8x16 a b c d e f g h i j k l m n o p =
    Bitv64.concat
      (Bitv32.concat
         (Bitv16.concat (Bitv8.concat a b) (Bitv8.concat c d))
         (Bitv16.concat (Bitv8.concat e f) (Bitv8.concat g h)) )
      (Bitv32.concat
         (Bitv16.concat (Bitv8.concat i j) (Bitv8.concat k l))
         (Bitv16.concat (Bitv8.concat m n) (Bitv8.concat o p)) )

  let to_i8x16 x =
    ( extract x ~high:127 ~low:120
    , extract x ~high:119 ~low:112
    , extract x ~high:111 ~low:104
    , extract x ~high:103 ~low:96
    , extract x ~high:95 ~low:88
    , extract x ~high:87 ~low:80
    , extract x ~high:79 ~low:72
    , extract x ~high:71 ~low:64
    , extract x ~high:63 ~low:56
    , extract x ~high:55 ~low:48
    , extract x ~high:47 ~low:40
    , extract x ~high:39 ~low:32
    , extract x ~high:31 ~low:24
    , extract x ~high:23 ~low:16
    , extract x ~high:15 ~low:8
    , extract x ~high:7 ~low:0 )

  let of_i16x8 a b c d e f g h =
    Bitv64.concat
      (Bitv32.concat (Bitv16.concat a b) (Bitv16.concat c d))
      (Bitv32.concat (Bitv16.concat e f) (Bitv16.concat g h))

  let to_i16x8 x =
    ( extract x ~high:127 ~low:112
    , extract x ~high:111 ~low:96
    , extract x ~high:95 ~low:80
    , extract x ~high:79 ~low:64
    , extract x ~high:63 ~low:48
    , extract x ~high:47 ~low:32
    , extract x ~high:31 ~low:16
    , extract x ~high:15 ~low:0 )

  let of_i32x4 a b c d = Bitv64.concat (Bitv32.concat a b) (Bitv32.concat c d)

  let to_i32x4 v =
    ( extract v ~low:96 ~high:127
    , extract v ~low:64 ~high:95
    , extract v ~low:32 ~high:63
    , extract v ~low:0 ~high:31 )

  let of_int64x2 a b =
    let low = Bitvector.of_int64 b in
    let high = Bitvector.of_int64 a in
    Expr.value (Bitv (Bitvector.concat high low))

  let of_i64x2 a b = Bitv64.concat a b

  let of_f32x4 a b c d =
    Bitv64.concat
      (Bitv32.concat (Float32.to_bv a) (Float32.to_bv b))
      (Bitv32.concat (Float32.to_bv c) (Float32.to_bv d))

  let of_f64x2 a b = Bitv64.concat (Float64.to_bv a) (Float64.to_bv b)

  let to_i64x2 v = (extract v ~low:64 ~high:127, extract v ~low:0 ~high:63)

  let to_f32x4 v =
    let a, b, c, d = to_i32x4 v in
    ( Float32.reinterpret_i32 a
    , Float32.reinterpret_i32 b
    , Float32.reinterpret_i32 c
    , Float32.reinterpret_i32 d )

  let to_f64x2 v =
    let a, b = to_i64x2 v in
    (Float64.reinterpret_i64 a, Float64.reinterpret_i64 b)

  module I8x16 = struct
    let map f v =
      let v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16
          =
        to_i8x16 v
      in
      let v1 = f v1 in
      let v2 = f v2 in
      let v3 = f v3 in
      let v4 = f v4 in
      let v5 = f v5 in
      let v6 = f v6 in
      let v7 = f v7 in
      let v8 = f v8 in
      let v9 = f v9 in
      let v10 = f v10 in
      let v11 = f v11 in
      let v12 = f v12 in
      let v13 = f v13 in
      let v14 = f v14 in
      let v15 = f v15 in
      let v16 = f v16 in
      of_i8x16 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16

    let mapi f v =
      let v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16
          =
        to_i8x16 v
      in
      let v1 = f 0 v1 in
      let v2 = f 1 v2 in
      let v3 = f 2 v3 in
      let v4 = f 3 v4 in
      let v5 = f 4 v5 in
      let v6 = f 5 v6 in
      let v7 = f 6 v7 in
      let v8 = f 7 v8 in
      let v9 = f 8 v9 in
      let v10 = f 9 v10 in
      let v11 = f 10 v11 in
      let v12 = f 11 v12 in
      let v13 = f 12 v13 in
      let v14 = f 13 v14 in
      let v15 = f 14 v15 in
      let v16 = f 15 v16 in
      of_i8x16 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16

    let map2 f u v =
      let u1, u2, u3, u4, u5, u6, u7, u8, u9, u10, u11, u12, u13, u14, u15, u16
          =
        to_i8x16 u
      in
      let v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16
          =
        to_i8x16 v
      in
      let w1 = f u1 v1 in
      let w2 = f u2 v2 in
      let w3 = f u3 v3 in
      let w4 = f u4 v4 in
      let w5 = f u5 v5 in
      let w6 = f u6 v6 in
      let w7 = f u7 v7 in
      let w8 = f u8 v8 in
      let w9 = f u9 v9 in
      let w10 = f u10 v10 in
      let w11 = f u11 v11 in
      let w12 = f u12 v12 in
      let w13 = f u13 v13 in
      let w14 = f u14 v14 in
      let w15 = f u15 v15 in
      let w16 = f u16 v16 in
      of_i8x16 w1 w2 w3 w4 w5 w6 w7 w8 w9 w10 w11 w12 w13 w14 w15 w16

    let fold_left f acc v =
      let v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16
          =
        to_i8x16 v
      in
      let acc = f acc v1 in
      let acc = f acc v2 in
      let acc = f acc v3 in
      let acc = f acc v4 in
      let acc = f acc v5 in
      let acc = f acc v6 in
      let acc = f acc v7 in
      let acc = f acc v8 in
      let acc = f acc v9 in
      let acc = f acc v10 in
      let acc = f acc v11 in
      let acc = f acc v12 in
      let acc = f acc v13 in
      let acc = f acc v14 in
      let acc = f acc v15 in
      let acc = f acc v16 in
      acc

    let to_array v =
      let a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15 =
        to_i8x16 v
      in
      [| a0; a1; a2; a3; a4; a5; a6; a7; a8; a9; a10; a11; a12; a13; a14; a15 |]

    let of_array a =
      of_i8x16 a.(0) a.(1) a.(2) a.(3) a.(4) a.(5) a.(6) a.(7) a.(8) a.(9)
        a.(10) a.(11) a.(12) a.(13) a.(14) a.(15)

    let eq u v =
      map2
        (fun x y ->
          Bool.ite (Bitv8.eq x y) (Bitv8.of_int 0xFF) (Bitv8.of_int 0x00) )
        u v

    let splat v = of_i8x16 v v v v v v v v v v v v v v v v

    let bitmask v =
      let _i, acc =
        fold_left
          (fun (i, acc) lane ->
            let sign =
              Bool.not
                (Bitv8.eq (Bitv8.logand lane (Bitv8.of_int 0x80)) Bitv8.zero)
            in
            let bit =
              Bool.ite sign
                (Bitv32.shl (Bitv32.of_int 1) (Bitv32.of_int i))
                Bitv32.zero
            in
            (succ i, Bitv32.logor acc bit) )
          (0, Bitv32.zero) v
      in
      acc

    let add x y = map2 Bitv8.add x y

    let sub x y = map2 Bitv8.sub x y

    let ne u v =
      map2
        (fun x y ->
          Bool.ite (Bitv8.ne x y) (Bitv8.of_int 0xFF) (Bitv8.of_int 0x00) )
        u v

    let abs v =
      map
        (fun x ->
          let neg = Bitv8.sub Bitv8.zero x in
          Bool.ite (Bitv8.lt x Bitv8.zero) neg x )
        v

    let neg v = map (fun x -> Bitv8.sub Bitv8.zero x) v

    let popcnt v =
      map
        (fun lane ->
          let rec aux i acc =
            if i = 8 then acc
            else
              let bit =
                Bitv8.logand (Bitv8.lshr lane (Bitv8.of_int i)) (Bitv8.of_int 1)
              in
              aux (i + 1) (Bitv8.add acc bit)
          in
          aux 0 Bitv8.zero )
        v

    let all_true v =
      fold_left
        (fun acc lane -> Bool.and_ acc (Bitv8.ne lane Bitv8.zero))
        Bool.true_ v

    let lt_s u v =
      map2
        (fun x y ->
          Bool.ite (Bitv8.lt x y) (Bitv8.of_int 0xFF) (Bitv8.of_int 0x00) )
        u v

    let lt_u u v =
      map2
        (fun x y ->
          Bool.ite (Bitv8.lt_u x y) (Bitv8.of_int 0xFF) (Bitv8.of_int 0x00) )
        u v

    let gt_s u v = lt_s v u

    let gt_u u v = lt_u v u

    let le_s u v =
      map2
        (fun x y ->
          Bool.ite (Bitv8.le x y) (Bitv8.of_int 0xFF) (Bitv8.of_int 0x00) )
        u v

    let le_u u v =
      map2
        (fun x y ->
          Bool.ite (Bitv8.le_u x y) (Bitv8.of_int 0xFF) (Bitv8.of_int 0x00) )
        u v

    let ge_s u v = le_s v u

    let ge_u u v = le_u v u

    let shl v amt =
      let amt = Bitv8.of_int (Bitv32.to_int ~signed:true amt land 7) in
      map (fun x -> Bitv8.shl x amt) v

    let shr_s v amt =
      let amt = Bitv8.of_int (Bitv32.to_int ~signed:true amt land 7) in
      map (fun x -> Bitv8.ashr x amt) v

    let shr_u v amt =
      let amt = Bitv8.of_int (Bitv32.to_int ~signed:true amt land 7) in
      map (fun x -> Bitv8.lshr x amt) v

    let min_s u v = map2 (fun x y -> Bool.ite (Bitv8.lt x y) x y) u v

    let min_u u v = map2 (fun x y -> Bool.ite (Bitv8.lt_u x y) x y) u v

    let max_s u v = map2 (fun x y -> Bool.ite (Bitv8.lt y x) x y) u v

    let max_u u v = map2 (fun x y -> Bool.ite (Bitv8.lt_u y x) x y) u v

    let add_sat_s u v =
      map2
        (fun x y ->
          let sum = Bitv8.add x y in
          let sx = Bitv8.lt x Bitv8.zero in
          let sy = Bitv8.lt y Bitv8.zero in
          let ss = Bitv8.lt sum Bitv8.zero in
          let overflow = Bool.and_ (Bool.eq sx sy) (Bool.not (Bool.eq sx ss)) in
          Bool.ite overflow
            (Bool.ite sx (Bitv8.of_int 0x80) (Bitv8.of_int 0x7f))
            sum )
        u v

    let add_sat_u u v =
      map2
        (fun x y ->
          let sum = Bitv8.add x y in
          let overflow = Bitv8.lt_u sum x in
          Bool.ite overflow (Bitv8.of_int 0xff) sum )
        u v

    let sub_sat_s u v =
      map2
        (fun x y ->
          let diff = Bitv8.sub x y in
          let sx = Bitv8.lt x Bitv8.zero in
          let sy = Bitv8.lt y Bitv8.zero in
          let sd = Bitv8.lt diff Bitv8.zero in
          let overflow =
            Bool.and_ (Bool.not @@ Bool.eq sx sy) (Bool.not @@ Bool.eq sx sd)
          in
          Bool.ite overflow
            (Bool.ite sx (Bitv8.of_int 0x80) (Bitv8.of_int 0x7f))
            diff )
        u v

    let sub_sat_u u v =
      map2
        (fun x y ->
          let underflow = Bitv8.lt_u x y in
          Bool.ite underflow Bitv8.zero (Bitv8.sub x y) )
        u v

    let avgr_u u v =
      map2
        (fun x y ->
          let s = Bitv16.add x y in
          let s = Bitv16.add s (Bitv16.of_int 1) in
          Bitv16.extract (Bitv16.lshr s (Bitv16.of_int 1)) ~high:7 ~low:0 )
        u v

    let narrow_i16x8_s u v =
      let sat x =
        Bool.ite
          (Bitv16.lt x (Bitv16.of_int (-128)))
          (Bitv8.of_int 0x80)
          (Bool.ite
             (Bitv16.lt (Bitv16.of_int 127) x)
             (Bitv8.of_int 0x7f)
             (Bitv16.extract x ~high:7 ~low:0) )
      in
      let a0, a1, a2, a3, a4, a5, a6, a7 = to_i16x8 u in
      let b0, b1, b2, b3, b4, b5, b6, b7 = to_i16x8 v in
      of_i8x16 (sat a0) (sat a1) (sat a2) (sat a3) (sat a4) (sat a5) (sat a6)
        (sat a7) (sat b0) (sat b1) (sat b2) (sat b3) (sat b4) (sat b5) (sat b6)
        (sat b7)

    let narrow_i16x8_u u v =
      let sat x =
        Bool.ite
          (Bitv16.lt_u x Bitv16.zero)
          Bitv8.zero
          (Bool.ite
             (Bitv16.lt_u (Bitv16.of_int 0xff) x)
             (Bitv8.of_int 0xff)
             (Bitv16.extract x ~high:7 ~low:0) )
      in
      let a0, a1, a2, a3, a4, a5, a6, a7 = to_i16x8 u in
      let b0, b1, b2, b3, b4, b5, b6, b7 = to_i16x8 v in
      of_i8x16 (sat a0) (sat a1) (sat a2) (sat a3) (sat a4) (sat a5) (sat a6)
        (sat a7) (sat b0) (sat b1) (sat b2) (sat b3) (sat b4) (sat b5) (sat b6)
        (sat b7)

    let bitselect v1 v2 mask = logor (logand v1 mask) (logand v2 (lognot mask))

    let swizzle v idx =
      let src = to_array v in
      let index = to_array idx in
      let select i =
        let rec aux j =
          if j >= 16 then Bitv8.zero
          else Bool.ite (Bitv8.eq i (Bitv8.of_int j)) src.(j) (aux (j + 1))
        in
        aux 0
      in
      Array.map
        (fun i ->
          Bool.ite (Bitv8.lt_u i (Bitv8.of_int 16)) (select i) Bitv8.zero )
        index
      |> of_array

    let extract_lane_s i v =
      let lanes = to_array v in
      Bitv8.to_int ~signed:true lanes.(i) |> Bitv32.of_int

    let extract_lane_u i v =
      let lanes = to_array v in
      Bitv8.to_int ~signed:false lanes.(i) |> Bitv32.of_int

    let replace_lane i x v =
      let lanes = to_array v in
      lanes.(i) <- x;
      of_array lanes

    let shuffle idx u v =
      let src = Array.init 32 (fun _i -> Bitv8.zero) in
      Array.blit (to_array u) 0 src 0 16;
      Array.blit (to_array v) 0 src 16 16;
      Array.init 16 (fun i -> src.(idx.(i))) |> of_array
  end

  module I16x8 = struct
    let map f v =
      let v1, v2, v3, v4, v5, v6, v7, v8 = to_i16x8 v in
      let v1 = f v1 in
      let v2 = f v2 in
      let v3 = f v3 in
      let v4 = f v4 in
      let v5 = f v5 in
      let v6 = f v6 in
      let v7 = f v7 in
      let v8 = f v8 in
      of_i16x8 v1 v2 v3 v4 v5 v6 v7 v8

    let mapi f v =
      let v1, v2, v3, v4, v5, v6, v7, v8 = to_i16x8 v in
      let v1 = f 0 v1 in
      let v2 = f 1 v2 in
      let v3 = f 2 v3 in
      let v4 = f 3 v4 in
      let v5 = f 4 v5 in
      let v6 = f 5 v6 in
      let v7 = f 6 v7 in
      let v8 = f 7 v8 in
      of_i16x8 v1 v2 v3 v4 v5 v6 v7 v8

    let map2 f u v =
      let u1, u2, u3, u4, u5, u6, u7, u8 = to_i16x8 u in
      let v1, v2, v3, v4, v5, v6, v7, v8 = to_i16x8 v in
      let w1 = f u1 v1 in
      let w2 = f u2 v2 in
      let w3 = f u3 v3 in
      let w4 = f u4 v4 in
      let w5 = f u5 v5 in
      let w6 = f u6 v6 in
      let w7 = f u7 v7 in
      let w8 = f u8 v8 in
      of_i16x8 w1 w2 w3 w4 w5 w6 w7 w8

    let fold_left f acc v =
      let v1, v2, v3, v4, v5, v6, v7, v8 = to_i16x8 v in
      let acc = f acc v1 in
      let acc = f acc v2 in
      let acc = f acc v3 in
      let acc = f acc v4 in
      let acc = f acc v5 in
      let acc = f acc v6 in
      let acc = f acc v7 in
      let acc = f acc v8 in
      acc

    let eq u v =
      map2
        (fun x y ->
          Bool.ite (Bitv16.eq x y) (Bitv16.of_int 0xFFFF) (Bitv16.of_int 0x0000) )
        u v

    let splat v = of_i16x8 v v v v v v v v

    let bitmask v =
      let _i, acc =
        fold_left
          (fun (i, acc) lane ->
            let sign =
              Bool.not
                (Bitv16.eq
                   (Bitv16.logand lane (Bitv16.of_int 0x8000))
                   Bitv16.zero )
            in
            let acc =
              Bitv32.logor acc
                (Bool.ite sign
                   (Bitv32.shl (Bitv32.of_int 1) (Bitv32.of_int i))
                   Bitv32.zero )
            in
            (succ i, acc) )
          (0, Bitv32.zero) v
      in
      acc

    let add x y = map2 Bitv16.add x y

    let sub x y = map2 Bitv16.sub x y

    let ne u v =
      map2
        (fun x y ->
          Bool.ite (Bitv16.ne x y) (Bitv16.of_int 0xFFFF) (Bitv16.of_int 0x0000) )
        u v

    let mul u v = map2 Bitv16.mul u v

    let abs v =
      map
        (fun x ->
          let neg = Bitv16.sub Bitv16.zero x in
          Bool.ite (Bitv16.lt x Bitv16.zero) neg x )
        v

    let neg v = map (fun x -> Bitv16.sub Bitv16.zero x) v

    let all_true v =
      fold_left
        (fun acc lane -> Bool.and_ acc (Bitv16.ne lane Bitv16.zero))
        Bool.true_ v

    let lt_s u v =
      map2
        (fun x y ->
          Bool.ite (Bitv16.lt x y) (Bitv16.of_int 0xFFFF) (Bitv16.of_int 0x0000) )
        u v

    let lt_u u v =
      map2
        (fun x y ->
          Bool.ite (Bitv16.lt_u x y) (Bitv16.of_int 0xFFFF)
            (Bitv16.of_int 0x0000) )
        u v

    let gt_s u v = lt_s v u

    let gt_u u v = lt_u v u

    let le_s u v =
      map2
        (fun x y ->
          Bool.ite (Bitv16.le x y) (Bitv16.of_int 0xFFFF) (Bitv16.of_int 0x0000) )
        u v

    let le_u u v =
      map2
        (fun x y ->
          Bool.ite (Bitv16.le_u x y) (Bitv16.of_int 0xFFFF)
            (Bitv16.of_int 0x0000) )
        u v

    let ge_s u v = le_s v u

    let ge_u u v = le_u v u

    let min_s u v = map2 (fun x y -> Bool.ite (Bitv16.lt x y) x y) u v

    let min_u u v = map2 (fun x y -> Bool.ite (Bitv16.lt_u x y) x y) u v

    let max_s u v = map2 (fun x y -> Bool.ite (Bitv16.lt y x) x y) u v

    let max_u u v = map2 (fun x y -> Bool.ite (Bitv16.lt_u y x) x y) u v

    let add_sat_s u v =
      map2
        (fun x y ->
          let sum = Bitv16.add x y in
          let x = Bitv16.lt x Bitv16.zero in
          let y = Bitv16.lt y Bitv16.zero in
          let s = Bitv16.lt sum Bitv16.zero in
          let overflow = Bool.and_ (Bool.eq x y) (Bool.not (Bool.eq x s)) in
          Bool.ite overflow
            (Bool.ite x (Bitv16.of_int 0x8000) (Bitv16.of_int 0x7fff))
            sum )
        u v

    let add_sat_u u v =
      map2
        (fun x y ->
          let sum = Bitv16.add x y in
          let overflow = Bitv16.lt_u sum x in
          Bool.ite overflow (Bitv16.of_int 0xffff) sum )
        u v

    let sub_sat_s u v =
      map2
        (fun x y ->
          let diff = Bitv16.sub x y in
          let x = Bitv16.lt x Bitv16.zero in
          let y = Bitv16.lt y Bitv16.zero in
          let d = Bitv16.lt diff Bitv16.zero in
          let overflow =
            Bool.and_ (Bool.not (Bool.eq x y)) (Bool.not (Bool.eq x d))
          in
          Bool.ite overflow
            (Bool.ite x (Bitv16.of_int 0x8000) (Bitv16.of_int 0x7fff))
            diff )
        u v

    let sub_sat_u u v =
      map2
        (fun x y ->
          let underflow = Bitv16.lt_u x y in
          Bool.ite underflow Bitv16.zero (Bitv16.sub x y) )
        u v

    let q15mulr_sat_s u v =
      map2
        (fun x y ->
          let x = Bitv16.to_int ~signed:true x |> Bitv32.of_int in
          let y = Bitv16.to_int ~signed:true y |> Bitv32.of_int in
          let res =
            Bitv32.ashr
              (Bitv32.add (Bitv32.mul x y) (Bitv32.of_int 0x4000))
              (Bitv32.of_int 15)
          in
          Bool.ite
            (Bitv32.lt (Bitv32.of_int 32767) res)
            (Bitv16.of_int 0x7fff)
            (Bool.ite
               (Bitv32.lt res (Bitv32.of_int (-32768)))
               (Bitv16.of_int 0x8000)
               (Bitv32.extract res ~high:15 ~low:0) ) )
        u v

    let shl v amt =
      let amt = Bitv16.of_int (Bitv32.to_int ~signed:true amt land 15) in
      map (fun x -> Bitv16.shl x amt) v

    let shr_s v amt =
      let amt = Bitv16.of_int (Bitv32.to_int ~signed:true amt land 15) in
      map (fun x -> Bitv16.ashr x amt) v

    let shr_u v amt =
      let amt = Bitv16.of_int (Bitv32.to_int ~signed:true amt land 15) in
      map (fun x -> Bitv16.lshr x amt) v

    let avgr_u u v =
      map2
        (fun x y ->
          let s = Bitv32.add x y in
          let s = Bitv32.add s (Bitv32.of_int 1) in
          Bitv32.extract (Bitv32.lshr s (Bitv32.of_int 1)) ~high:15 ~low:0 )
        u v

    let narrow_i32x4_s u v =
      let sat x =
        Bool.ite
          (Bitv32.lt x (Bitv32.of_int (-32768)))
          (Bitv16.of_int 0x8000)
          (Bool.ite
             (Bitv32.lt (Bitv32.of_int 32767) x)
             (Bitv16.of_int 0x7fff)
             (Bitv32.extract x ~high:15 ~low:0) )
      in
      let a0, a1, a2, a3 = to_i32x4 u in
      let b0, b1, b2, b3 = to_i32x4 v in
      of_i16x8 (sat a0) (sat a1) (sat a2) (sat a3) (sat b0) (sat b1) (sat b2)
        (sat b3)

    let narrow_i32x4_u u v =
      let sat x =
        Bool.ite
          (Bitv32.lt_u x Bitv32.zero)
          Bitv16.zero
          (Bool.ite
             (Bitv32.lt_u (Bitv32.of_int 0xffff) x)
             (Bitv16.of_int 0xffff)
             (Bitv32.extract x ~high:15 ~low:0) )
      in
      let a0, a1, a2, a3 = to_i32x4 u in
      let b0, b1, b2, b3 = to_i32x4 v in
      of_i16x8 (sat a0) (sat a1) (sat a2) (sat a3) (sat b0) (sat b1) (sat b2)
        (sat b3)

    let extend_low_i8x16_s v =
      let ext x =
        let sign =
          Bool.not (Bitv8.eq (Bitv8.logand x (Bitv8.of_int 0x80)) Bitv8.zero)
        in
        let low = Bitv16.of_int (Bitv8.to_int ~signed:false x) in
        Bool.ite sign (Bitv16.logor low (Bitv16.of_int 0xff00)) low
      in
      let a0, a1, a2, a3, a4, a5, a6, a7, _, _, _, _, _, _, _, _ = to_i8x16 v in
      of_i16x8 (ext a0) (ext a1) (ext a2) (ext a3) (ext a4) (ext a5) (ext a6)
        (ext a7)

    let extend_low_i8x16_u v =
      let ext x = Bitv16.of_int (Bitv8.to_int ~signed:false x) in
      let a0, a1, a2, a3, a4, a5, a6, a7, _, _, _, _, _, _, _, _ = to_i8x16 v in
      of_i16x8 (ext a0) (ext a1) (ext a2) (ext a3) (ext a4) (ext a5) (ext a6)
        (ext a7)

    let extend_high_i8x16_s v =
      let ext x =
        let sign =
          Bool.not (Bitv8.eq (Bitv8.logand x (Bitv8.of_int 0x80)) Bitv8.zero)
        in
        let low = Bitv16.of_int (Bitv8.to_int ~signed:false x) in
        Bool.ite sign (Bitv16.logor low (Bitv16.of_int 0xff00)) low
      in
      let _, _, _, _, _, _, _, _, a8, a9, a10, a11, a12, a13, a14, a15 =
        to_i8x16 v
      in
      of_i16x8 (ext a8) (ext a9) (ext a10) (ext a11) (ext a12) (ext a13)
        (ext a14) (ext a15)

    let extend_high_i8x16_u v =
      let ext x = Bitv16.of_int (Bitv8.to_int ~signed:false x) in
      let _, _, _, _, _, _, _, _, a8, a9, a10, a11, a12, a13, a14, a15 =
        to_i8x16 v
      in
      of_i16x8 (ext a8) (ext a9) (ext a10) (ext a11) (ext a12) (ext a13)
        (ext a14) (ext a15)

    let extmul_low_i8x16_s u v =
      let ext_s x =
        let sign =
          Bool.not (Bitv8.eq (Bitv8.logand x (Bitv8.of_int 0x80)) Bitv8.zero)
        in
        let low = Bitv16.of_int (Bitv8.to_int ~signed:false x) in
        Bool.ite sign (Bitv16.logor low (Bitv16.of_int 0xff00)) low
      in
      let a0, a1, a2, a3, a4, a5, a6, a7, _, _, _, _, _, _, _, _ = to_i8x16 u in
      let b0, b1, b2, b3, b4, b5, b6, b7, _, _, _, _, _, _, _, _ = to_i8x16 v in
      of_i16x8
        (Bitv16.mul (ext_s a0) (ext_s b0))
        (Bitv16.mul (ext_s a1) (ext_s b1))
        (Bitv16.mul (ext_s a2) (ext_s b2))
        (Bitv16.mul (ext_s a3) (ext_s b3))
        (Bitv16.mul (ext_s a4) (ext_s b4))
        (Bitv16.mul (ext_s a5) (ext_s b5))
        (Bitv16.mul (ext_s a6) (ext_s b6))
        (Bitv16.mul (ext_s a7) (ext_s b7))

    let extmul_low_i8x16_u u v =
      let ext_u x = Bitv16.of_int (Bitv8.to_int ~signed:false x) in
      let a0, a1, a2, a3, a4, a5, a6, a7, _, _, _, _, _, _, _, _ = to_i8x16 u in
      let b0, b1, b2, b3, b4, b5, b6, b7, _, _, _, _, _, _, _, _ = to_i8x16 v in
      of_i16x8
        (Bitv16.mul (ext_u a0) (ext_u b0))
        (Bitv16.mul (ext_u a1) (ext_u b1))
        (Bitv16.mul (ext_u a2) (ext_u b2))
        (Bitv16.mul (ext_u a3) (ext_u b3))
        (Bitv16.mul (ext_u a4) (ext_u b4))
        (Bitv16.mul (ext_u a5) (ext_u b5))
        (Bitv16.mul (ext_u a6) (ext_u b6))
        (Bitv16.mul (ext_u a7) (ext_u b7))

    let extmul_high_i8x16_s u v =
      let ext_s x =
        let sign =
          Bool.not (Bitv8.eq (Bitv8.logand x (Bitv8.of_int 0x80)) Bitv8.zero)
        in
        let low = Bitv16.of_int (Bitv8.to_int ~signed:false x) in
        Bool.ite sign (Bitv16.logor low (Bitv16.of_int 0xff00)) low
      in
      let _, _, _, _, _, _, _, _, a8, a9, a10, a11, a12, a13, a14, a15 =
        to_i8x16 u
      in
      let _, _, _, _, _, _, _, _, b8, b9, b10, b11, b12, b13, b14, b15 =
        to_i8x16 v
      in
      of_i16x8
        (Bitv16.mul (ext_s a8) (ext_s b8))
        (Bitv16.mul (ext_s a9) (ext_s b9))
        (Bitv16.mul (ext_s a10) (ext_s b10))
        (Bitv16.mul (ext_s a11) (ext_s b11))
        (Bitv16.mul (ext_s a12) (ext_s b12))
        (Bitv16.mul (ext_s a13) (ext_s b13))
        (Bitv16.mul (ext_s a14) (ext_s b14))
        (Bitv16.mul (ext_s a15) (ext_s b15))

    let extmul_high_i8x16_u u v =
      let ext_u x = Bitv16.of_int (Bitv8.to_int ~signed:false x) in
      let _, _, _, _, _, _, _, _, a8, a9, a10, a11, a12, a13, a14, a15 =
        to_i8x16 u
      in
      let _, _, _, _, _, _, _, _, b8, b9, b10, b11, b12, b13, b14, b15 =
        to_i8x16 v
      in
      of_i16x8
        (Bitv16.mul (ext_u a8) (ext_u b8))
        (Bitv16.mul (ext_u a9) (ext_u b9))
        (Bitv16.mul (ext_u a10) (ext_u b10))
        (Bitv16.mul (ext_u a11) (ext_u b11))
        (Bitv16.mul (ext_u a12) (ext_u b12))
        (Bitv16.mul (ext_u a13) (ext_u b13))
        (Bitv16.mul (ext_u a14) (ext_u b14))
        (Bitv16.mul (ext_u a15) (ext_u b15))

    let extadd_pairwise_i8x16_s v =
      let ext_s x =
        let sign =
          Bool.not (Bitv8.eq (Bitv8.logand x (Bitv8.of_int 0x80)) Bitv8.zero)
        in
        let low = Bitv16.of_int (Bitv8.to_int ~signed:false x) in
        Bool.ite sign (Bitv16.logor low (Bitv16.of_int 0xff00)) low
      in
      let a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15 =
        to_i8x16 v
      in
      of_i16x8
        (Bitv16.add (ext_s a0) (ext_s a1))
        (Bitv16.add (ext_s a2) (ext_s a3))
        (Bitv16.add (ext_s a4) (ext_s a5))
        (Bitv16.add (ext_s a6) (ext_s a7))
        (Bitv16.add (ext_s a8) (ext_s a9))
        (Bitv16.add (ext_s a10) (ext_s a11))
        (Bitv16.add (ext_s a12) (ext_s a13))
        (Bitv16.add (ext_s a14) (ext_s a15))

    let extadd_pairwise_i8x16_u v =
      let ext_u x = Bitv16.of_int (Bitv8.to_int ~signed:false x) in
      let a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15 =
        to_i8x16 v
      in
      of_i16x8
        (Bitv16.add (ext_u a0) (ext_u a1))
        (Bitv16.add (ext_u a2) (ext_u a3))
        (Bitv16.add (ext_u a4) (ext_u a5))
        (Bitv16.add (ext_u a6) (ext_u a7))
        (Bitv16.add (ext_u a8) (ext_u a9))
        (Bitv16.add (ext_u a10) (ext_u a11))
        (Bitv16.add (ext_u a12) (ext_u a13))
        (Bitv16.add (ext_u a14) (ext_u a15))

    let extract_lane_s i v =
      let lanes =
        let a0, a1, a2, a3, a4, a5, a6, a7 = to_i16x8 v in
        [| a0; a1; a2; a3; a4; a5; a6; a7 |]
      in
      let x = lanes.(i) in
      let sign =
        Bool.not
          (Bitv16.eq (Bitv16.logand x (Bitv16.of_int 0x8000)) Bitv16.zero)
      in
      let low = Bitv32.of_int (Bitv16.to_int ~signed:false x) in
      Bool.ite sign (Bitv32.logor low (Bitv32.of_int 0xffff0000)) low

    let extract_lane_u i v =
      let lanes =
        let a0, a1, a2, a3, a4, a5, a6, a7 = to_i16x8 v in
        [| a0; a1; a2; a3; a4; a5; a6; a7 |]
      in
      Bitv32.of_int (Bitv16.to_int ~signed:false lanes.(i))

    let replace_lane i x v =
      let lanes =
        let a0, a1, a2, a3, a4, a5, a6, a7 = to_i16x8 v in
        [| a0; a1; a2; a3; a4; a5; a6; a7 |]
      in
      lanes.(i) <- x;
      of_i16x8 lanes.(0) lanes.(1) lanes.(2) lanes.(3) lanes.(4) lanes.(5)
        lanes.(6) lanes.(7)
  end

  module I32x4 = struct
    let map f v =
      let v1, v2, v3, v4 = to_i32x4 v in
      let v1 = f v1 in
      let v2 = f v2 in
      let v3 = f v3 in
      let v4 = f v4 in
      of_i32x4 v1 v2 v3 v4

    let mapi f v =
      let v1, v2, v3, v4 = to_i32x4 v in
      let v1 = f 0 v1 in
      let v2 = f 1 v2 in
      let v3 = f 2 v3 in
      let v4 = f 3 v4 in
      of_i32x4 v1 v2 v3 v4

    let map2 f u v =
      let u1, u2, u3, u4 = to_i32x4 u in
      let v1, v2, v3, v4 = to_i32x4 v in
      let w1 = f u1 v1 in
      let w2 = f u2 v2 in
      let w3 = f u3 v3 in
      let w4 = f u4 v4 in
      of_i32x4 w1 w2 w3 w4

    let fold_left f acc v =
      let v1, v2, v3, v4 = to_i32x4 v in
      let acc = f acc v1 in
      let acc = f acc v2 in
      let acc = f acc v3 in
      let acc = f acc v4 in
      acc

    let eq u v =
      map2
        (fun x y ->
          Bool.ite (Bitv32.eq x y)
            (Bitv32.of_int32 0xFFFFFFFFl)
            (Bitv32.of_int32 0x00000000l) )
        u v

    let splat v = of_i32x4 v v v v

    let bitmask v =
      mapi
        (fun i lane ->
          let sign =
            Bool.not
              (Bitv32.eq
                 (Bitv32.logand lane (Bitv32.of_int32 Int32.min_int))
                 Bitv32.zero )
          in
          Bool.ite sign
            (Bitv32.shl (Bitv32.of_int 1) (Bitv32.of_int i))
            Bitv32.zero )
        v
      |> fold_left Bitv32.logor Bitv32.zero

    let add x y = map2 Bitv32.add x y

    let sub x y = map2 Bitv32.sub x y

    let ne u v =
      map2
        (fun x y ->
          Bool.ite (Bitv32.ne x y)
            (Bitv32.of_int32 0xFFFFFFFFl)
            (Bitv32.of_int32 0x00000000l) )
        u v

    let mul u v = map2 Bitv32.mul u v

    let neg v = map (fun x -> Bitv32.sub Bitv32.zero x) v

    let abs v =
      map
        (fun x ->
          Bool.ite (Bitv32.lt x Bitv32.zero) (Bitv32.sub Bitv32.zero x) x )
        v

    let all_true v =
      fold_left
        (fun acc lane -> Bool.and_ acc (Bitv32.ne lane Bitv32.zero))
        Bool.true_ v

    let lt_s u v =
      map2
        (fun x y ->
          Bool.ite (Bitv32.lt x y)
            (Bitv32.of_int32 0xFFFFFFFFl)
            (Bitv32.of_int32 0x00000000l) )
        u v

    let lt_u u v =
      map2
        (fun x y ->
          Bool.ite (Bitv32.lt_u x y)
            (Bitv32.of_int32 0xFFFFFFFFl)
            (Bitv32.of_int32 0x00000000l) )
        u v

    let gt_s u v = lt_s v u

    let gt_u u v = lt_u v u

    let le_s u v =
      map2
        (fun x y ->
          Bool.ite (Bitv32.le x y)
            (Bitv32.of_int32 0xFFFFFFFFl)
            (Bitv32.of_int32 0x00000000l) )
        u v

    let le_u u v =
      map2
        (fun x y ->
          Bool.ite (Bitv32.le_u x y)
            (Bitv32.of_int32 0xFFFFFFFFl)
            (Bitv32.of_int32 0x00000000l) )
        u v

    let ge_s u v = le_s v u

    let ge_u u v = le_u v u

    let min_s u v = map2 (fun x y -> Bool.ite (Bitv32.lt x y) x y) u v

    let min_u u v = map2 (fun x y -> Bool.ite (Bitv32.lt_u x y) x y) u v

    let max_s u v = map2 (fun x y -> Bool.ite (Bitv32.lt y x) x y) u v

    let max_u u v = map2 (fun x y -> Bool.ite (Bitv32.lt_u y x) x y) u v

    let shl v amt =
      let amt = Bitv32.of_int (Bitv32.to_int ~signed:true amt land 31) in
      map (fun x -> Bitv32.shl x amt) v

    let shr_s v amt =
      let amt = Bitv32.of_int (Bitv32.to_int ~signed:true amt land 31) in
      map (fun x -> Bitv32.ashr x amt) v

    let shr_u v amt =
      let amt = Bitv32.of_int (Bitv32.to_int ~signed:true amt land 31) in
      map (fun x -> Bitv32.lshr x amt) v

    let extend_i16_s x =
      let sign =
        Bool.not
          (Bitv16.eq (Bitv16.logand x (Bitv16.of_int 0x8000)) Bitv16.zero)
      in
      let low = Bitv32.of_int (Bitv16.to_int ~signed:false x) in
      Bool.ite sign (Bitv32.logor low (Bitv32.of_int32 0xffff0000l)) low

    let extend_i16_u x = Bitv32.of_int (Bitv16.to_int ~signed:false x)

    let extend_low_i16x8_s v =
      let a0, a1, a2, a3, _, _, _, _ = to_i16x8 v in
      of_i32x4 (extend_i16_s a0) (extend_i16_s a1) (extend_i16_s a2)
        (extend_i16_s a3)

    let extend_low_i16x8_u v =
      let a0, a1, a2, a3, _, _, _, _ = to_i16x8 v in
      of_i32x4 (extend_i16_u a0) (extend_i16_u a1) (extend_i16_u a2)
        (extend_i16_u a3)

    let extend_high_i16x8_s v =
      let _, _, _, _, a4, a5, a6, a7 = to_i16x8 v in
      of_i32x4 (extend_i16_s a4) (extend_i16_s a5) (extend_i16_s a6)
        (extend_i16_s a7)

    let extend_high_i16x8_u v =
      let _, _, _, _, a4, a5, a6, a7 = to_i16x8 v in
      of_i32x4 (extend_i16_u a4) (extend_i16_u a5) (extend_i16_u a6)
        (extend_i16_u a7)

    let extmul_low_i16x8_s u v =
      let a0, a1, a2, a3, _, _, _, _ = to_i16x8 u in
      let b0, b1, b2, b3, _, _, _, _ = to_i16x8 v in
      of_i32x4
        (Bitv32.mul (extend_i16_s a0) (extend_i16_s b0))
        (Bitv32.mul (extend_i16_s a1) (extend_i16_s b1))
        (Bitv32.mul (extend_i16_s a2) (extend_i16_s b2))
        (Bitv32.mul (extend_i16_s a3) (extend_i16_s b3))

    let extmul_low_i16x8_u u v =
      let a0, a1, a2, a3, _, _, _, _ = to_i16x8 u in
      let b0, b1, b2, b3, _, _, _, _ = to_i16x8 v in
      of_i32x4
        (Bitv32.mul (extend_i16_u a0) (extend_i16_u b0))
        (Bitv32.mul (extend_i16_u a1) (extend_i16_u b1))
        (Bitv32.mul (extend_i16_u a2) (extend_i16_u b2))
        (Bitv32.mul (extend_i16_u a3) (extend_i16_u b3))

    let extmul_high_i16x8_s u v =
      let _, _, _, _, a4, a5, a6, a7 = to_i16x8 u in
      let _, _, _, _, b4, b5, b6, b7 = to_i16x8 v in
      of_i32x4
        (Bitv32.mul (extend_i16_s a4) (extend_i16_s b4))
        (Bitv32.mul (extend_i16_s a5) (extend_i16_s b5))
        (Bitv32.mul (extend_i16_s a6) (extend_i16_s b6))
        (Bitv32.mul (extend_i16_s a7) (extend_i16_s b7))

    let extmul_high_i16x8_u u v =
      let _, _, _, _, a4, a5, a6, a7 = to_i16x8 u in
      let _, _, _, _, b4, b5, b6, b7 = to_i16x8 v in
      of_i32x4
        (Bitv32.mul (extend_i16_u a4) (extend_i16_u b4))
        (Bitv32.mul (extend_i16_u a5) (extend_i16_u b5))
        (Bitv32.mul (extend_i16_u a6) (extend_i16_u b6))
        (Bitv32.mul (extend_i16_u a7) (extend_i16_u b7))

    let extadd_pairwise_i16x8_s v =
      let a0, a1, a2, a3, a4, a5, a6, a7 = to_i16x8 v in
      let add x y =
        Bitv32.of_int
          (Bitv16.to_int ~signed:true x + Bitv16.to_int ~signed:true y)
      in
      of_i32x4 (add a0 a1) (add a2 a3) (add a4 a5) (add a6 a7)

    let extadd_pairwise_i16x8_u v =
      let a0, a1, a2, a3, a4, a5, a6, a7 = to_i16x8 v in
      let add x y =
        Bitv32.of_int
          (Bitv16.to_int ~signed:false x + Bitv16.to_int ~signed:false y)
      in
      of_i32x4 (add a0 a1) (add a2 a3) (add a4 a5) (add a6 a7)

    let dot_i16x8_s u v =
      let a0, a1, a2, a3, a4, a5, a6, a7 = to_i16x8 u in
      let b0, b1, b2, b3, b4, b5, b6, b7 = to_i16x8 v in
      let mul x y =
        Bitv32.of_int
          (Bitv16.to_int ~signed:true x * Bitv16.to_int ~signed:true y)
      in
      of_i32x4
        (Bitv32.add (mul a0 b0) (mul a1 b1))
        (Bitv32.add (mul a2 b2) (mul a3 b3))
        (Bitv32.add (mul a4 b4) (mul a5 b5))
        (Bitv32.add (mul a6 b6) (mul a7 b7))

    let trunc_sat_f32x4_s v =
      let a, b, c, d = to_f32x4 v in
      of_i32x4 (Bitv32.trunc_sat_f32_s a) (Bitv32.trunc_sat_f32_s b)
        (Bitv32.trunc_sat_f32_s c) (Bitv32.trunc_sat_f32_s d)

    let trunc_sat_f32x4_u v =
      let a, b, c, d = to_f32x4 v in
      of_i32x4 (Bitv32.trunc_sat_f32_u a) (Bitv32.trunc_sat_f32_u b)
        (Bitv32.trunc_sat_f32_u c) (Bitv32.trunc_sat_f32_u d)

    let trunc_sat_f64x2_s_zero v =
      let a, b = to_f64x2 v in
      of_i32x4 (Bitv32.trunc_sat_f64_s a) (Bitv32.trunc_sat_f64_s b) Bitv32.zero
        Bitv32.zero

    let trunc_sat_f64x2_u_zero v =
      let a, b = to_f64x2 v in
      of_i32x4 (Bitv32.trunc_sat_f64_u a) (Bitv32.trunc_sat_f64_u b) Bitv32.zero
        Bitv32.zero

    let extract_lane i v =
      let a, b, c, d = to_i32x4 v in
      match i with 0 -> a | 1 -> b | 2 -> c | 3 -> d | _ -> assert false

    let replace_lane i x v =
      let a, b, c, d = to_i32x4 v in
      match i with
      | 0 -> of_i32x4 x b c d
      | 1 -> of_i32x4 a x c d
      | 2 -> of_i32x4 a b x d
      | 3 -> of_i32x4 a b c x
      | _ -> assert false
  end

  module I64x2 = struct
    let map f v =
      let v1, v2 = to_i64x2 v in
      let v1 = f v1 in
      let v2 = f v2 in
      of_i64x2 v1 v2

    let mapi f v =
      let v1, v2 = to_i64x2 v in
      let v1 = f 0 v1 in
      let v2 = f 1 v2 in
      of_i64x2 v1 v2

    let map2 f u v =
      let u1, u2 = to_i64x2 u in
      let v1, v2 = to_i64x2 v in
      let w1 = f u1 v1 in
      let w2 = f u2 v2 in
      of_i64x2 w1 w2

    let fold_left f acc v =
      let v1, v2 = to_i64x2 v in
      let acc = f acc v1 in
      let acc = f acc v2 in
      acc

    let eq u v =
      map2
        (fun x y ->
          Bool.ite (Bitv64.eq x y)
            (Bitv64.of_int64 0xFFFFFFFFFFFFFFFFL)
            (Bitv64.of_int64 0x0000000000000000L) )
        u v

    let splat v = of_i64x2 v v

    let bitmask v =
      let _i, acc =
        fold_left
          (fun (i, acc) lane ->
            let sign =
              Bool.not
                (Bitv64.eq
                   (Bitv64.logand lane (Bitv64.of_int64 0x8000000000000000L))
                   Bitv64.zero )
            in
            let acc =
              Bitv32.logor acc
                (Bool.ite sign
                   (Bitv32.shl (Bitv32.of_int 1) (Bitv32.of_int i))
                   Bitv32.zero )
            in
            (succ i, acc) )
          (0, Bitv32.zero) v
      in
      acc

    let add x y = map2 Bitv64.add x y

    let sub x y = map2 Bitv64.sub x y

    let ne u v =
      map2
        (fun x y ->
          Bool.ite (Bitv64.ne x y)
            (Bitv64.of_int64 0xFFFFFFFFFFFFFFFFL)
            (Bitv64.of_int64 0L) )
        u v

    let mul u v = map2 Bitv64.mul u v

    let neg v = map (fun x -> Bitv64.sub Bitv64.zero x) v

    let abs v =
      map
        (fun x ->
          Bool.ite (Bitv64.lt x Bitv64.zero) (Bitv64.sub Bitv64.zero x) x )
        v

    let all_true v =
      fold_left
        (fun acc lane -> Bool.and_ acc (Bitv64.ne lane Bitv64.zero))
        Bool.true_ v

    let lt_s u v =
      map2
        (fun x y -> Bool.ite (Bitv64.lt x y) (Bitv64.of_int64 (-1L)) Bitv64.zero)
        u v

    let gt_s u v = lt_s v u

    let le_s u v =
      map2
        (fun x y -> Bool.ite (Bitv64.le x y) (Bitv64.of_int64 (-1L)) Bitv64.zero)
        u v

    let ge_s u v = le_s v u

    let shl v amt =
      let amt = Bitv64.of_int (Bitv32.to_int ~signed:true amt land 63) in
      map (fun x -> Bitv64.shl x amt) v

    let shr_s v amt =
      let amt = Bitv64.of_int (Bitv32.to_int ~signed:true amt land 63) in
      map (fun x -> Bitv64.ashr x amt) v

    let shr_u v amt =
      let amt = Bitv64.of_int (Bitv32.to_int ~signed:true amt land 63) in
      map (fun x -> Bitv64.lshr x amt) v

    let extend_low_i32x4_s v =
      let a, _, _, _ = to_i32x4 v in
      of_i64x2 (Bitv64.of_int (Bitv32.to_int ~signed:true a)) Bitv64.zero

    let extend_low_i32x4_u v =
      let a, _, _, _ = to_i32x4 v in
      of_i64x2 (Bitv64.of_int (Bitv32.to_int ~signed:false a)) Bitv64.zero

    let extend_high_i32x4_s v =
      let _, _, a, _ = to_i32x4 v in
      of_i64x2 (Bitv64.of_int (Bitv32.to_int ~signed:true a)) Bitv64.zero

    let extend_high_i32x4_u v =
      let _, _, a, _ = to_i32x4 v in
      of_i64x2 (Bitv64.of_int (Bitv32.to_int ~signed:false a)) Bitv64.zero

    let extmul_low_i32x4_s u v =
      let a0, a1, _, _ = to_i32x4 u in
      let b0, b1, _, _ = to_i32x4 v in
      of_i64x2
        (Bitv64.of_int
           (Bitv32.to_int ~signed:true a0 * Bitv32.to_int ~signed:true b0) )
        (Bitv64.of_int
           (Bitv32.to_int ~signed:true a1 * Bitv32.to_int ~signed:true b1) )

    let extmul_low_i32x4_u u v =
      let a0, a1, _, _ = to_i32x4 u in
      let b0, b1, _, _ = to_i32x4 v in
      of_i64x2
        (Bitv64.of_int
           (Bitv32.to_int ~signed:false a0 * Bitv32.to_int ~signed:false b0) )
        (Bitv64.of_int
           (Bitv32.to_int ~signed:false a1 * Bitv32.to_int ~signed:false b1) )

    let extmul_high_i32x4_s u v =
      let _, _, a2, a3 = to_i32x4 u in
      let _, _, b2, b3 = to_i32x4 v in
      of_i64x2
        (Bitv64.of_int
           (Bitv32.to_int ~signed:true a2 * Bitv32.to_int ~signed:true b2) )
        (Bitv64.of_int
           (Bitv32.to_int ~signed:true a3 * Bitv32.to_int ~signed:true b3) )

    let extmul_high_i32x4_u u v =
      let _, _, a2, a3 = to_i32x4 u in
      let _, _, b2, b3 = to_i32x4 v in
      of_i64x2
        (Bitv64.of_int
           (Bitv32.to_int ~signed:false a2 * Bitv32.to_int ~signed:false b2) )
        (Bitv64.of_int
           (Bitv32.to_int ~signed:false a3 * Bitv32.to_int ~signed:false b3) )

    let extract_lane i v =
      let a, b = to_i64x2 v in
      match i with 0 -> a | 1 -> b | _ -> assert false

    let replace_lane i x v =
      let a, b = to_i64x2 v in
      match i with 0 -> of_i64x2 x b | 1 -> of_i64x2 a x | _ -> assert false
  end

  module F32x4 = struct
    let map f v =
      let v1, v2, v3, v4 = to_f32x4 v in
      let v1 = f v1 in
      let v2 = f v2 in
      let v3 = f v3 in
      let v4 = f v4 in
      of_f32x4 v1 v2 v3 v4

    let map2 f u v =
      let u1, u2, u3, u4 = to_f32x4 u in
      let v1, v2, v3, v4 = to_f32x4 v in
      of_f32x4 (f u1 v1) (f u2 v2) (f u3 v3) (f u4 v4)

    let abs v = map Float32.abs v

    let neg v = map Float32.neg v

    let sqrt v = map Float32.sqrt v

    let add u v = map2 Float32.add u v

    let sub u v = map2 Float32.sub u v

    let mul u v = map2 Float32.mul u v

    let div u v = map2 Float32.div u v

    let min u v = map2 Float32.min u v

    let max u v = map2 Float32.max u v

    let pmin u v = map2 Float32.pmin u v

    let pmax u v = map2 Float32.pmax u v

    let cmp f u v =
      let f32_mask b =
        Bool.ite b (Bitv32.of_int32 0xffffffffl) (Bitv32.of_int32 0x00000000l)
      in
      let u1, u2, u3, u4 = to_f32x4 u in
      let v1, v2, v3, v4 = to_f32x4 v in
      of_i32x4
        (f32_mask (f u1 v1))
        (f32_mask (f u2 v2))
        (f32_mask (f u3 v3))
        (f32_mask (f u4 v4))

    let eq u v = cmp Float32.eq u v

    let ne u v = cmp Float32.ne u v

    let lt u v = cmp Float32.lt u v

    let gt u v = cmp (fun x y -> Float32.lt y x) u v

    let le u v = cmp Float32.le u v

    let ge u v = cmp (fun x y -> Float32.le y x) u v

    let ceil v = map Float32.ceil v

    let floor v = map Float32.floor v

    let trunc v = map Float32.trunc v

    let nearest v = map Float32.nearest v

    let splat v = of_f32x4 v v v v

    let convert_i32x4_s v =
      let a, b, c, d = to_i32x4 v in
      of_f32x4 (Float32.convert_i32_s a) (Float32.convert_i32_s b)
        (Float32.convert_i32_s c) (Float32.convert_i32_s d)

    let convert_i32x4_u v =
      let a, b, c, d = to_i32x4 v in
      of_f32x4 (Float32.convert_i32_u a) (Float32.convert_i32_u b)
        (Float32.convert_i32_u c) (Float32.convert_i32_u d)

    let convert_low_i32x4_s v =
      let a, b, _, _ = to_i32x4 v in
      of_f32x4 (Float32.convert_i32_s a) (Float32.convert_i32_s b) Float32.zero
        Float32.zero

    let convert_low_i32x4_u v =
      let a, b, _, _ = to_i32x4 v in
      of_f32x4 (Float32.convert_i32_u a) (Float32.convert_i32_u b) Float32.zero
        Float32.zero

    let convert_high_i32x4_s v =
      let _, _, c, d = to_i32x4 v in
      of_f32x4 (Float32.convert_i32_s c) (Float32.convert_i32_s d) Float32.zero
        Float32.zero

    let convert_high_i32x4_u v =
      let _, _, c, d = to_i32x4 v in
      of_f32x4 (Float32.convert_i32_u c) (Float32.convert_i32_u d) Float32.zero
        Float32.zero

    let demote_f64x2_zero v =
      let a, b = to_f64x2 v in
      of_f32x4 (Float32.demote_f64 a) (Float32.demote_f64 b) Float32.zero
        Float32.zero

    let extract_lane i v =
      let a, b, c, d = to_f32x4 v in
      match i with 0 -> a | 1 -> b | 2 -> c | 3 -> d | _ -> assert false

    let replace_lane i x v =
      let a, b, c, d = to_f32x4 v in
      match i with
      | 0 -> of_f32x4 x b c d
      | 1 -> of_f32x4 a x c d
      | 2 -> of_f32x4 a b x d
      | 3 -> of_f32x4 a b c x
      | _ -> assert false
  end

  module F64x2 = struct
    let map f v =
      let v1, v2 = to_f64x2 v in
      let v1 = f v1 in
      let v2 = f v2 in
      of_f64x2 v1 v2

    let map2 f u v =
      let u1, u2 = to_f64x2 u in
      let v1, v2 = to_f64x2 v in
      of_f64x2 (f u1 v1) (f u2 v2)

    let abs v = map Float64.abs v

    let neg v = map Float64.neg v

    let sqrt v = map Float64.sqrt v

    let add u v = map2 Float64.add u v

    let sub u v = map2 Float64.sub u v

    let mul u v = map2 Float64.mul u v

    let div u v = map2 Float64.div u v

    let min u v = map2 Float64.min u v

    let max u v = map2 Float64.max u v

    let pmin u v = map2 Float64.pmin u v

    let pmax u v = map2 Float64.pmax u v

    let cmp f u v =
      let mask b =
        Bool.ite b
          (Bitv64.of_int64 0xFFFFFFFFFFFFFFFFL)
          (Bitv64.of_int64 0x0000000000000000L)
      in
      let u1, u2 = to_f64x2 u in
      let v1, v2 = to_f64x2 v in
      of_i64x2 (mask (f u1 v1)) (mask (f u2 v2))

    let eq u v = cmp Float64.eq u v

    let ne u v = cmp Float64.ne u v

    let lt u v = cmp Float64.lt u v

    let gt u v = cmp (fun x y -> Float64.lt y x) u v

    let le u v = cmp Float64.le u v

    let ge u v = cmp (fun x y -> Float64.le y x) u v

    let ceil v = map Float64.ceil v

    let floor v = map Float64.floor v

    let trunc v = map Float64.trunc v

    let nearest v = map Float64.nearest v

    let splat v = of_f64x2 v v

    let convert_low_i32x4_s v =
      let a, b, _, _ = to_i32x4 v in
      of_f64x2 (Float64.convert_i32_s a) (Float64.convert_i32_s b)

    let convert_low_i32x4_u v =
      let a, b, _, _ = to_i32x4 v in
      of_f64x2 (Float64.convert_i32_u a) (Float64.convert_i32_u b)

    let convert_high_i32x4_s v =
      let _, _, c, d = to_i32x4 v in
      of_f64x2 (Float64.convert_i32_s c) (Float64.convert_i32_s d)

    let convert_high_i32x4_u v =
      let _, _, c, d = to_i32x4 v in
      of_f64x2 (Float64.convert_i32_u c) (Float64.convert_i32_u d)

    let promote_low_f32x4 v =
      let a, b, _, _ = to_f32x4 v in
      of_f64x2 (Float64.promote_f32 a) (Float64.promote_f32 b)

    let extract_lane i v =
      let a, b = to_f64x2 v in
      match i with 0 -> a | 1 -> b | _ -> assert false

    let replace_lane i x v =
      let a, b = to_f64x2 v in
      match i with 0 -> of_f64x2 x b | 1 -> of_f64x2 a x | _ -> assert false
  end

  let any_true v = Bool.not (eq v zero)

  let andnot u v = logand u (lognot v)

  let bitselect v1 v2 mask = logor (logand v1 mask) (logand v2 (lognot mask))

  let replace_lane8 i x v =
    let lanes =
      Array.init 16 (fun j -> extract v ~high:((j * 8) + 7) ~low:(j * 8))
    in
    lanes.(i) <- x;
    of_i8x16 lanes.(15) lanes.(14) lanes.(13) lanes.(12) lanes.(11) lanes.(10)
      lanes.(9) lanes.(8) lanes.(7) lanes.(6) lanes.(5) lanes.(4) lanes.(3)
      lanes.(2) lanes.(1) lanes.(0)

  let replace_lane16 i x v =
    let lanes =
      Array.init 8 (fun j -> extract v ~high:((j * 16) + 15) ~low:(j * 16))
    in
    lanes.(i) <- x;
    of_i16x8 lanes.(7) lanes.(6) lanes.(5) lanes.(4) lanes.(3) lanes.(2)
      lanes.(1) lanes.(0)

  let replace_lane32 i x v =
    let lanes =
      Array.init 4 (fun j -> extract v ~high:((j * 32) + 31) ~low:(j * 32))
    in
    lanes.(i) <- x;
    of_i32x4 lanes.(3) lanes.(2) lanes.(1) lanes.(0)

  let replace_lane64 i x v =
    let lanes =
      Array.init 2 (fun j -> extract v ~high:((j * 64) + 63) ~low:(j * 64))
    in
    lanes.(i) <- x;
    of_i64x2 lanes.(1) lanes.(0)

  let extract_lane8 i v = extract v ~high:((i * 8) + 7) ~low:(i * 8)

  let extract_lane16 i v = extract v ~high:((i * 16) + 15) ~low:(i * 16)

  let extract_lane32 i v = extract v ~high:((i * 32) + 31) ~low:(i * 32)

  let extract_lane64 i v = extract v ~high:((i * 64) + 63) ~low:(i * 64)
end

module Int = struct
  type t = int expr

  let[@inline] v x = Expr.value (Int x)

  let[@inline] symbol x = Expr.symbol x

  let[@inline] pp fmt x = Expr.pp fmt x

  let[@inline] neg x = Expr.unop Types_inner.int Neg x

  let[@inline] to_real x = Expr.cvtop Types_inner.real Reinterpret_int x

  let[@inline] add x y = Expr.binop Types_inner.int Add x y

  let[@inline] sub x y = Expr.binop Types_inner.int Sub x y

  let[@inline] mul x y = Expr.binop Types_inner.int Mul x y

  let[@inline] div x y = Expr.binop Types_inner.int Div x y

  let[@inline] rem x y = Expr.binop Types_inner.int Rem x y

  let[@inline] mod_ _x _y = assert false

  let[@inline] pow x y = Expr.binop Types_inner.int Pow x y

  let[@inline] eq a b = Expr.relop Types_inner.bool Eq a b

  let[@inline] lt x y = Expr.relop Types_inner.int Lt x y

  let[@inline] le x y = Expr.relop Types_inner.int Le x y
end

module Real = struct
  type t = real expr

  let[@inline] v x = Expr.value (Real x)

  let[@inline] symbol x = Expr.symbol x

  let[@inline] pp fmt x = Expr.pp fmt x

  let[@inline] neg x = Expr.unop Types_inner.real Neg x

  let[@inline] to_int x = Expr.cvtop Ty_int Reinterpret_float x

  let[@inline] add x y = Expr.binop Types_inner.real Add x y

  let[@inline] sub x y = Expr.binop Types_inner.real Sub x y

  let[@inline] mul x y = Expr.binop Types_inner.real Mul x y

  let[@inline] div x y = Expr.binop Types_inner.real Div x y

  let[@inline] pow x y = Expr.binop Types_inner.real Pow x y

  let[@inline] eq a b = Expr.relop Types_inner.bool Eq a b

  let[@inline] lt x y = Expr.relop Types_inner.real Lt x y

  let[@inline] le x y = Expr.relop Types_inner.real Le x y
end

module String = struct
  type t = string expr

  let[@inline] v s = Expr.value (Str s)

  let[@inline] symbol sym = Expr.symbol sym

  let[@inline] pp fmt s = Expr.pp fmt s

  let[@inline] length s = Expr.unop Types_inner.string Length s

  let[@inline] to_code s = Expr.cvtop Types_inner.string String_to_code s

  let[@inline] of_code code =
    Expr.cvtop Types_inner.string String_from_code code

  let[@inline] to_int s = Expr.cvtop Types_inner.string String_to_int s

  let[@inline] of_int i = Expr.cvtop Types_inner.string String_from_int i

  let[@inline] at s ~pos = Expr.binop Types_inner.string At s pos

  let[@inline] concat xs = Expr.naryop Types_inner.string Concat xs

  let[@inline] contains s ~sub =
    Expr.binop Types_inner.string String_contains s sub

  let[@inline] is_prefix s ~prefix =
    Expr.binop Types_inner.string String_prefix s prefix

  let[@inline] is_suffix s ~suffix =
    Expr.binop Types_inner.string String_suffix s suffix

  let[@inline] eq a b = Expr.relop Types_inner.bool Eq a b

  let[@inline] lt a b = Expr.relop Types_inner.string Lt a b

  let[@inline] le a b = Expr.relop Types_inner.string Le a b

  let[@inline] sub s ~pos ~len =
    Expr.triop Types_inner.string String_extract s pos len

  let[@inline] index_of s ~sub ~pos =
    Expr.triop Types_inner.string String_index s sub pos

  let[@inline] replace s ~pattern ~with_ =
    Expr.triop Types_inner.string String_replace s pattern with_

  let[@inline] replace_all s ~pattern ~with_ =
    Expr.triop Types_inner.string String_replace_all s pattern with_

  let[@inline] replace_re s ~pattern ~with_ =
    Expr.triop Types_inner.string String_replace_re s pattern with_

  let[@inline] replace_re_all s ~pattern ~with_ =
    Expr.triop Types_inner.string String_replace_re_all s pattern with_

  let[@inline] to_re s = Expr.raw_cvtop Types_inner.string String_to_re s

  let[@inline] in_re s re = Expr.raw_binop Types_inner.string String_in_re s re

  module Re = struct
    type t = regexp expr

    let none = Expr.value Re_none

    let all = Expr.value Re_all

    let allchar = Expr.value Re_allchar

    let[@inline] star re = Expr.raw_unop Types_inner.regexp Regexp_star re

    let[@inline] plus re = Expr.raw_unop Types_inner.regexp Regexp_plus re

    let[@inline] opt re = Expr.raw_unop Types_inner.regexp Regexp_opt re

    let[@inline] complement re = Expr.raw_unop Types_inner.regexp Regexp_comp re

    let[@inline] range re1 re2 =
      Expr.raw_binop Types_inner.regexp Regexp_range re1 re2

    let[@inline] diff re1 re2 =
      Expr.raw_binop Types_inner.regexp Regexp_diff re1 re2

    let[@inline] inter re1 re2 =
      Expr.raw_binop Types_inner.regexp Regexp_inter re1 re2

    let[@inline] loop ~min ~max re =
      Expr.raw_unop Types_inner.regexp (Regexp_loop (min, max)) re

    let[@inline] union res = Expr.raw_naryop Types_inner.regexp Regexp_union res

    let[@inline] concat res = Expr.raw_naryop Types_inner.regexp Concat res

    let[@inline] pp fmt re = Expr.pp fmt re
  end
end

module Func = struct
  type ('fn, 'r) t =
    | Ret : 'r ty -> ('r expr, 'r) t
    | Arg : 'a ty * ('fn, 'r) t -> ('a expr -> 'fn, 'r) t

  let ret ret = Ret ret

  let ( @-> ) ty next = Arg (ty, next)

  let rec compile : type fn r. (fn, r) t -> string -> Expr.t list -> fn =
   fun (spec : (fn, r) t) name args ->
    match spec with
    | Ret ret_ty -> Expr.app (Symbol.make ret_ty name) (List.rev args)
    | Arg (_arg_ty, next) -> fun arg -> compile next name (arg :: args)

  let make name spec = compile spec name []
end

module Types = struct
  include Types_inner

  let bitv128 : Bitv128.w ty = Bitv128.ty
end

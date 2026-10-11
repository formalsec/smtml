open Crowbar

let bool_binop =
  choose
    [ const Smtml.Typed.Bool.or_
    ; const Smtml.Typed.Bool.and_
    ; const Smtml.Typed.Bool.implies
    ; const Smtml.Typed.Bool.xor
    ]

let bool_unop = choose [ const Smtml.Typed.Bool.not ]

let bv32_bool_bin =
  choose
    [ const Smtml.Typed.Bitv32.eq
    ; const Smtml.Typed.Bitv32.ne
    ; const Smtml.Typed.Bitv32.lt
    ; const Smtml.Typed.Bitv32.lt_u
    ; const Smtml.Typed.Bitv32.le
    ; const Smtml.Typed.Bitv32.le_u
    ]

let bv32_binop =
  choose
    [ const Smtml.Typed.Bitv32.add
    ; const Smtml.Typed.Bitv32.sub
    ; const Smtml.Typed.Bitv32.mul
    ; const Smtml.Typed.Bitv32.logor
    ; const Smtml.Typed.Bitv32.logand
    ; const Smtml.Typed.Bitv32.logxor
    ; const Smtml.Typed.Bitv32.shl
    ; const Smtml.Typed.Bitv32.ashr
    ; const Smtml.Typed.Bitv32.lshr
    ; const Smtml.Typed.Bitv32.rem
    ; const Smtml.Typed.Bitv32.unsigned_rem
    ; const Smtml.Typed.Bitv32.div
    ; const Smtml.Typed.Bitv32.unsigned_div
    ]

let bv32_unop =
  choose
    [ const Smtml.Typed.Bitv32.neg
    ; const Smtml.Typed.Bitv32.clz
    ; const Smtml.Typed.Bitv32.ctz
    ; const Smtml.Typed.Bitv32.popcnt
    ; const Smtml.Typed.Bitv32.lognot
    ]

let rotate =
  choose
    [ const Smtml.Typed.Bitv32.rotate_left
    ; const Smtml.Typed.Bitv32.rotate_right
    ]

(* ======= INT ======= *)

let binop_int =
  choose
    [ const Smtml.Typed.Int.add
    ; const Smtml.Typed.Int.sub
    ; const Smtml.Typed.Int.mul (* ; const Smtml.Typed.Int.mod_ *)
    ; const Smtml.Typed.Int.pow
    ; const Smtml.Typed.Int.div
    ; const Smtml.Typed.Int.rem
    ]

let unop_int = choose [ const Smtml.Typed.Int.neg ]

let int_bool_bin =
  choose
    [ const Smtml.Typed.Int.eq
    ; const Smtml.Typed.Int.lt
    ; const Smtml.Typed.Int.le
    ]

(* ======= float ======= *)

let binop_float32 =
  choose
    [ const Smtml.Typed.Float32.add
    ; const Smtml.Typed.Float32.sub
    ; const Smtml.Typed.Float32.mul
    ; const Smtml.Typed.Float32.div
    ; const Smtml.Typed.Float32.min
    ; const Smtml.Typed.Float32.max
    ; const Smtml.Typed.Float32.copy_sign
    ; const Smtml.Typed.Float32.rem
    ]

let unop_float32 =
  choose
    [ const Smtml.Typed.Float32.neg
    ; const Smtml.Typed.Float32.abs
    ; const Smtml.Typed.Float32.sqrt
    ; const Smtml.Typed.Float32.ceil
    ; const Smtml.Typed.Float32.floor
    ; const Smtml.Typed.Float32.trunc
    ; const Smtml.Typed.Float32.nearest
    ]

let float32_bool_bin =
  choose
    [ const Smtml.Typed.Float32.eq
    ; const Smtml.Typed.Float32.ne
    ; const Smtml.Typed.Float32.lt
    ; const Smtml.Typed.Float32.le
    ]

let float32_bool_unop =
  choose
    [ const Smtml.Typed.Float32.is_normal
    ; const Smtml.Typed.Float32.is_subnormal
    ; const Smtml.Typed.Float32.is_negative
    ; const Smtml.Typed.Float32.is_positive
    ; const Smtml.Typed.Float32.is_infinite
    ; const Smtml.Typed.Float32.is_zero
    ; const Smtml.Typed.Float32.is_nan
    ]

let float32_convert =
  choose
    [ const Smtml.Typed.Float32.convert_i32_s
    ; const Smtml.Typed.Float32.convert_i32_u
    ; const Smtml.Typed.Float32.reinterpret_i32
    ]

let binop_float64 =
  choose
    [ const Smtml.Typed.Float64.add
    ; const Smtml.Typed.Float64.sub
    ; const Smtml.Typed.Float64.mul
    ; const Smtml.Typed.Float64.div
    ; const Smtml.Typed.Float64.min
    ; const Smtml.Typed.Float64.max
    ; const Smtml.Typed.Float64.copy_sign
    ; const Smtml.Typed.Float64.rem
    ]

let unop_float64 =
  choose
    [ const Smtml.Typed.Float64.neg
    ; const Smtml.Typed.Float64.abs
    ; const Smtml.Typed.Float64.sqrt
    ; const Smtml.Typed.Float64.ceil
    ; const Smtml.Typed.Float64.floor
    ; const Smtml.Typed.Float64.trunc
    ; const Smtml.Typed.Float64.nearest
    ]

let float64_bool_bin =
  choose
    [ const Smtml.Typed.Float64.eq
    ; const Smtml.Typed.Float64.ne
    ; const Smtml.Typed.Float64.lt
    ; const Smtml.Typed.Float64.le
    ]

let float64_bool_unop =
  choose
    [ const Smtml.Typed.Float64.is_normal
    ; const Smtml.Typed.Float64.is_subnormal
    ; const Smtml.Typed.Float64.is_negative
    ; const Smtml.Typed.Float64.is_positive
    ; const Smtml.Typed.Float64.is_infinite
    ; const Smtml.Typed.Float64.is_zero
    ; const Smtml.Typed.Float64.is_nan
    ]

let bv32_to_float64 =
  choose
    [ const Smtml.Typed.Float64.convert_i32_s
    ; const Smtml.Typed.Float64.convert_i32_u
    ]

(* let bv64_to_float64 =
  choose
    [ const Smtml.Typed.Float64.convert_i64_s
    ; const Smtml.Typed.Float64.convert_i64_u
    ; const Smtml.Typed.Float64.reinterpret_i64
    ] 
    Todo float64 <-> bv64*)

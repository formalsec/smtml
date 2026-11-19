open Crowbar

val bool_binop :
  (Smtml.Typed.Bool.t -> Smtml.Typed.Bool.t -> Smtml.Typed.Bool.t) gen

val bool_unop : (Smtml.Typed.Bool.t -> Smtml.Typed.Bool.t) gen

val bv32_bool_bin :
  (Smtml.Typed.Bitv32.t -> Smtml.Typed.Bitv32.t -> bool Smtml.Typed.expr) gen

val bv32_binop :
  (Smtml.Typed.Bitv32.t -> Smtml.Typed.Bitv32.t -> Smtml.Typed.Bitv32.t) gen

val bv32_unop : (Smtml.Typed.Bitv32.t -> Smtml.Typed.Bitv32.t) gen

val rotate : (int -> Smtml.Typed.Bitv32.t -> Smtml.Typed.Bitv32.t) gen

val binop_int :
  (Smtml.Typed.Int.t -> Smtml.Typed.Int.t -> Smtml.Typed.Int.t) gen

val unop_int : (Smtml.Typed.Int.t -> Smtml.Typed.Int.t) gen

val int_bool_bin :
  (Smtml.Typed.Int.t -> Smtml.Typed.Int.t -> bool Smtml.Typed.expr) gen

val binop_float32 :
  (Smtml.Typed.Float32.t -> Smtml.Typed.Float32.t -> Smtml.Typed.Float32.t) gen

val unop_float32 : (Smtml.Typed.Float32.t -> Smtml.Typed.Float32.t) gen

val float32_bool_bin :
  (Smtml.Typed.Float32.t -> Smtml.Typed.Float32.t -> bool Smtml.Typed.expr) gen

val float32_bool_unop : (Smtml.Typed.Float32.t -> bool Smtml.Typed.expr) gen

val float32_convert : (Smtml.Typed.Bitv32.t -> Smtml.Typed.Float32.t) gen

val binop_float64 :
  (Smtml.Typed.Float64.t -> Smtml.Typed.Float64.t -> Smtml.Typed.Float64.t) gen

val unop_float64 : (Smtml.Typed.Float64.t -> Smtml.Typed.Float64.t) gen

val float64_bool_bin :
  (Smtml.Typed.Float64.t -> Smtml.Typed.Float64.t -> bool Smtml.Typed.expr) gen

val float64_bool_unop : (Smtml.Typed.Float64.t -> bool Smtml.Typed.expr) gen

val bv32_to_float64 : (Smtml.Typed.Bitv32.t -> Smtml.Typed.Float64.t) gen

(* val bv64_to_float64 : (Smtml.Typed.bitv64 Smtml.Typed.expr -> Smtml.Typed.Float64.t)
gen *)

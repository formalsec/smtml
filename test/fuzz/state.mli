open Crowbar

type t =
  { bv32_names : Smtml.Symbol.t Crowbar.gen
  ; bool_names : Smtml.Symbol.t Crowbar.gen
  ; float32_names : Smtml.Symbol.t Crowbar.gen
  ; int_names : Smtml.Symbol.t Crowbar.gen
  ; float64_names : Smtml.Symbol.t Crowbar.gen
  ; theory : Theory.t
  }

val state : Theory.t gen -> t gen

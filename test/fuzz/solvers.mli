type t

val init :
     disable_z3:bool
  -> disable_altergo:bool
  -> disable_bitwuzla:bool
  -> disable_cvc5:bool
  -> disable_colibri2:bool
  -> disable_smtzilla:bool
  -> t list

val check_same_output : bool Smtml.Typed.expr -> t list -> bool -> bool

val check_same_output_and_sat :
  Smtml.Typed.Bool.t -> t list -> [ `Sat | `Unknown | `Unsat ] option

val check_model : Smtml.Typed.Bool.t -> t list -> bool

val run :
     seed:int64 option
  -> repeat:int
  -> randomness_file:string option
  -> infinity:bool
  -> disable_z3:bool
  -> disable_altergo:bool
  -> disable_bitwuzla:bool
  -> disable_cvc5:bool
  -> disable_colibri2:bool
  -> disable_smtzilla:bool
  -> set_theory:Theory.t option
  -> get_model:bool
  -> int

val randomness_file : string option Cmdliner.Term.t

val seed : int64 option Cmdliner.Term.t

val repeat : int Cmdliner.Term.t

val infinity : bool Cmdliner.Term.t

val crowbar_info : Cmdliner.Cmd.info

val disable_z3 : bool Cmdliner.Term.t

val disable_altergo : bool Cmdliner.Term.t

val disable_bitwuzla : bool Cmdliner.Term.t

val disable_cvc5 : bool Cmdliner.Term.t

val disable_colibri2 : bool Cmdliner.Term.t

val disable_smtzilla : bool Cmdliner.Term.t

val get_model : bool Cmdliner.Term.t

val cmp_model : bool Cmdliner.Term.t

val setup_log : unit Cmdliner.Term.t

val set_theory : Theory.t option Cmdliner.Term.t

open Crowbar

type t =
  | ALL  (** The logic that encompasses all theories. *)
  | QF_BV  (** Quantifier-free bitvector theory. *)
  | QF_BVFP  (** Quantifier-free bitvectors and floating-point arithmetic. *)
  | QF_FP  (** Quantifier-free floating-point arithmetic. *)
  | QF_LIA  (** Quantifier-free linear integer arithmetic. *)
  | ONLY_BOOL
  | LIA

val get_theory : unit -> t gen

val has_bv : t -> bool

val has_fp : t -> bool

val has_qf_lia : t -> bool

val has_all : t -> bool

val has_lia : t -> bool

val print_theory : t -> unit

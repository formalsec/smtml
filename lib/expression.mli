open Types

exception InvalidRelop

type qt =
  | Forall
  | Exists

type expr =
  | Val of Value.t
  | SymPtr of int32 * expr
  | Unop of unop * expr
  | Binop of binop * expr * expr
  | Relop of relop * expr * expr
  | Cvtop of cvtop * expr
  | Triop of triop * expr * expr * expr
  | Symbol of Symbol.t
  | Extract of expr * int * int
  | Quantifier of qt * Symbol.t list * expr * expr list list

type t = expr

val ( ++ ) : expr -> expr -> expr
val mk_symbol : Symbol.t -> expr
val mk_symbol_s : expr_type -> string -> expr
val is_num : expr -> bool
val is_val : expr -> bool
val is_unop : expr -> bool
val is_relop : expr -> bool
val is_binop : expr -> bool
val is_cvtop : expr -> bool
val is_triop : expr -> bool
val is_concrete : expr -> bool
val equal : expr -> expr -> bool
val length : expr -> int
val get_symbols : expr list -> Symbol.t list
val type_of : expr -> expr_type option
val negate_relop : expr -> expr
val pp_unop : Format.formatter -> unop -> unit
val pp_binop : Format.formatter -> binop -> unit
val pp_triop : Format.formatter -> triop -> unit
val pp_relop : Format.formatter -> relop -> unit
val pp_cvtop : Format.formatter -> cvtop -> unit
val pp : Format.formatter -> expr -> unit
val to_string : expr -> string
val pp_list : Format.formatter -> expr list -> unit
val string_of_list : expr list -> string
val to_smt : expr list -> string
val string_of_values : (Num.t * expr) list -> string
val to_bool : expr -> expr option
val simplify : ?extract:bool -> expr -> expr
val mk_relop : ?reduce:bool -> expr -> num_type -> expr

(* FIXME: specific to wasp, remove?  *)
val get_ptr : expr -> Num.t option
val concretize_ptr : expr -> Num.t option
val concretize_base_ptr : expr -> int32 option

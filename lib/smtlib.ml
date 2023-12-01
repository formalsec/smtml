type script = command list

and command =
  | Assert of term
  | Check_sat
  | Check_sat_assuming
  | Declare_const
  | Declare_datatype
  | Declare_datatypes
  | Declare_fun of symbol * sort list * sort
  | Declare_sort of symbol * int
  | Define_fun
  | Define_fun_rec
  | Define_funs_rec
  | Define_sort (* of (symbol, symbols sort) *)
  | Echo of string
  | Exit
  | Get_assertions
  | Get_assignment
  | Get_info (* of info_flag *)
  | Get_model
  | Get_option of string
  | Get_proof
  | Get_unsat_assumptions
  | Get_unsat_core
  | Get_value of term list
  | Pop of int
  | Push of int
  | Reset
  | Reset_assertions
  | Set_info (* of attribute *)
  | Set_logic of string
  | Set_option

and symbol = string

and spec_constant =
  | Num of int
  | Dec of float
  | Hex of string
  | Bin of string
  | Str of string

and index =
  | I of int
  | S of symbol

and identifier =
  | Sym of symbol
  | Hole of symbol * index list

and sort =
  | Sort of identifier
  | Sort_comp of identifier * sort list

and qual_identifier =
  | Plain of identifier
  | As of identifier * sort

and term =
  | Const of spec_constant
  | Id of qual_identifier
  | App of qual_identifier * term list
  | Let of (symbol * term) list * term
  | Forall of (symbol * sort) list * term
  | Exists of (symbol * sort) list * term

let const_of_val v =
  let open Value in
  match v with
  | True -> Id (Plain (Sym "true"))
  | False -> Id (Plain (Sym "false"))
  | Int x -> Const (Num x)
  | Real x -> Const (Dec x)
  | Str x -> Const (Str x)
  | Num (I8 x) -> Id (Plain (Hole ("bv8", [ I x ])))
  | Num (I32 x) -> Id (Plain (Hole ("bv32", [ I (Int32.to_int x) ])))
  | Num (I64 x) -> Id (Plain (Hole ("bv64", [ I (Int64.to_int x) ])))
  | Num (F32 _) | Num (F64 _) -> assert false

let term_of_expr ({ e; _ } : Expr.t) : term =
  let open Expr in
  match e with Val v -> const_of_val v | _ -> assert false

let script_ es = List.map (fun e -> Assert (term_of_expr e)) es @ [ Check_sat ]

module Format = struct
  open Format

  let pp = fprintf
  let pp_string = pp_print_string

  let pp_const fmt = function
    | Num x -> pp_print_int fmt x
    | Dec x -> pp_print_float fmt x
    | Hex x | Bin x | Str x -> pp_string fmt x

  let pp_index fmt = function
    | I i -> Format.pp_print_int fmt i
    | S i -> pp_string fmt i

  let pp_identifier fmt = function
    | Sym id -> pp_string fmt id
    | Hole (id, indices) ->
      pp fmt "(_ %s %a)" id
        (pp_print_list ~pp_sep:pp_print_space pp_index)
        indices

  let rec pp_sort fmt = function
    | Sort id -> pp_identifier fmt id
    | Sort_comp (id, sorts) ->
      pp fmt "(%a %a)" pp_identifier id
        (pp_print_list ~pp_sep:pp_print_space pp_sort)
        sorts

  let pp_qual_identifier fmt = function
    | Plain id -> pp_identifier fmt id
    | As _ -> assert false

  let rec pp_term fmt = function
    | Const x -> pp_const fmt x
    | Id x -> pp_qual_identifier fmt x
    | App (id, terms) ->
      pp fmt "(%a %a)" pp_qual_identifier id
        (pp_print_list ~pp_sep:pp_print_space pp_term)
        terms
    | Let _ | Forall _ | Exists _ -> assert false

  let pp_cmd fmt = function
    | Assert term -> pp fmt "(assert %a)" pp_term term
    | Check_sat -> pp_string fmt "(check-sat)"
    | Check_sat_assuming -> pp_string fmt "(check-sat-assuming)"
    | Declare_const -> pp_string fmt "(declare-const)"
    | Declare_datatype -> pp_string fmt "(declare-datatype)"
    | Declare_datatypes -> pp_string fmt "(declare-datatypes)"
    | Declare_fun (f, args, sort) ->
      pp fmt "(declare-fun %s (%a) %a)" f
        (pp_print_list ~pp_sep:pp_print_space pp_sort)
        args pp_sort sort
    | Declare_sort (sort, n) -> pp fmt "(declare-sort %s %d)" sort n
    | Define_fun -> pp_string fmt "(define-fun)"
    | Define_fun_rec -> pp_string fmt "(define-fun-rec)"
    | Define_funs_rec -> pp_string fmt "(define-funs-rec)"
    | Define_sort -> pp_string fmt "(define-sort)"
    | Echo str -> pp fmt "(echo %s)" str
    | Exit -> pp_string fmt "(exit)"
    | Get_assertions -> pp_string fmt "(get-assertions)"
    | Get_assignment -> pp_string fmt "(get-assignment)"
    | Get_info -> pp_string fmt "(get-info)"
    | Get_model -> pp_string fmt "(get-model)"
    | Get_option x -> pp fmt "(get-option %s)" x
    | Get_proof -> pp_string fmt "(get-proof)"
    | Get_unsat_assumptions -> pp_string fmt "(get-unsat-assumptions)"
    | Get_unsat_core -> pp_string fmt "(get-unsat-core)"
    | Get_value _ -> pp_string fmt "(get-value)"
    | Pop n -> pp fmt "(pop %d)" n
    | Push n -> pp fmt "(push %d)" n
    | Reset -> pp_string fmt "(reset)"
    | Reset_assertions -> pp_string fmt "(reset-assertions)"
    | Set_info -> pp_string fmt "(set-info)"
    | Set_logic x -> pp fmt "(set-logic %s)" x
    | Set_option -> pp_string fmt "(set-option)"

  let pp_script fmt v = pp_print_list ~pp_sep:pp_print_newline pp_cmd fmt v
end

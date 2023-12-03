type script = command list

and command =
  | Assert of term
  | Check_sat
  | Check_sat_assuming
  | Declare_const of symbol * sort
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

let sort_of_ty : Ty.t -> sort = function
  | Ty.Ty_int -> Sort (Sym "Int")
  | Ty.Ty_real -> Sort (Sym "Real")
  | Ty.Ty_bool -> Sort (Sym "Bool")
  | Ty.Ty_str -> Sort (Sym "String")
  | Ty.Ty_bitv S8 -> Sort (Hole ("BitVec", [ I 8 ]))
  | Ty.Ty_bitv S32 -> Sort (Hole ("BitVec", [ I 32 ]))
  | Ty.Ty_bitv S64 -> Sort (Hole ("BitVec", [ I 64 ]))
  | Ty.Ty_fp S32 -> Sort (Sym "Float32")
  | Ty.Ty_fp S64 -> Sort (Sym "Float64")
  | Ty.Ty_fp S8 -> assert false

let const_of_val v =
  let open Value in
  match v with
  | True -> Id (Plain (Sym "true"))
  | False -> Id (Plain (Sym "false"))
  | Int x -> Const (Num x)
  | Real x -> Const (Dec x)
  | Str x -> Const (Str x)
  | Num (I8 x) -> Id (Plain (Hole ("bv" ^ string_of_int x, [ I 8 ])))
  | Num (I32 x) -> Id (Plain (Hole ("bv" ^ Int32.to_string x, [ I 32 ])))
  | Num (I64 x) -> Id (Plain (Hole ("bv" ^ Int64.to_string x, [ I 64 ])))
  | Num (F32 _) | Num (F64 _) -> assert false

let id_of_unop ty op : qual_identifier =
  let open Ty in
  let arith_unop = function Neg -> Plain (Sym "-") | _ -> assert false in
  let core_unop = function Not -> Plain (Sym "not") | _ -> assert false in
  let str_unop = function Len -> Plain (Sym "str.len") | _ -> assert false in
  let bitv_unop = function
    | Not -> Plain (Sym "bvnot")
    | Neg -> Plain (Sym "bvneg")
    | _ -> assert false
  in
  match ty with
  | Ty_int | Ty_real -> arith_unop op
  | Ty_bool -> core_unop op
  | Ty_str -> str_unop op
  | Ty_bitv _ -> bitv_unop op
  | Ty_fp _ -> failwith "TODO: id_of_unop"

let id_of_binop ty op : qual_identifier =
  let open Ty in
  let int_binop = function
    | Add -> Plain (Sym "+")
    | Sub -> Plain (Sym "-")
    | Mul -> Plain (Sym "*")
    | Div -> Plain (Sym "div")
    | Rem -> Plain (Sym "mod")
    | _ -> assert false
  in
  let real_binop = function
    | Add -> Plain (Sym "+")
    | Sub -> Plain (Sym "-")
    | Mul -> Plain (Sym "*")
    | Div -> Plain (Sym "/")
    | _ -> assert false
  in
  let core_binop = function
    | And -> Plain (Sym "and")
    | Or -> Plain (Sym "or")
    | Xor -> Plain (Sym "xor")
    | _ -> assert false
  in
  let bitv_binop = function
    | Add -> Plain (Sym "bvadd")
    | Sub -> Plain (Sym "bvsub")
    | Mul -> Plain (Sym "bvmul")
    | Div -> Plain (Sym "bvsdiv")
    | DivU -> Plain (Sym "bvudiv")
    | And -> Plain (Sym "bvand")
    | Xor -> Plain (Sym "bvxor")
    | Or -> Plain (Sym "bvor")
    | Shl -> Plain (Sym "bvshl")
    | ShrA -> Plain (Sym "bvashr")
    | ShrL -> Plain (Sym "bvlshr")
    | Rem -> Plain (Sym "bvsrem")
    | RemU -> Plain (Sym "bvurem")
    | _ -> assert false
  in
  match ty with
  | Ty_int -> int_binop op
  | Ty_real -> real_binop op
  | Ty_bool -> core_binop op
  | Ty_str -> assert false
  | Ty_bitv _ -> bitv_binop op
  | Ty_fp _ -> failwith "TODO: id_of_binop"

let id_of_triop _ _op : qual_identifier = assert false

let id_of_relop ty op : qual_identifier =
  let open Ty in
  let arith_relop = function
    | Eq -> Plain (Sym "=")
    | Le -> Plain (Sym "<=")
    | Lt -> Plain (Sym "<")
    | Ge -> Plain (Sym ">=")
    | Gt -> Plain (Sym ">")
    | _ -> assert false
  in
  let core_relop = function Eq -> Plain (Sym "=") | _ -> assert false in
  let bitv_relop = function
    | Eq -> Plain (Sym "=")
    | Lt -> Plain (Sym "bvslt")
    | LtU -> Plain (Sym "bvult")
    | Le -> Plain (Sym "bvsle")
    | LeU -> Plain (Sym "bvule")
    | Gt -> Plain (Sym "bvsgt")
    | GtU -> Plain (Sym "bvugt")
    | Ge -> Plain (Sym "bvsge")
    | GeU -> Plain (Sym "bvuge")
    | Ne -> assert false
  in
  match ty with
  | Ty_int | Ty_real -> arith_relop op
  | Ty_bool -> core_relop op
  | Ty_bitv _ -> bitv_relop op
  | Ty_str -> assert false
  | Ty_fp _ -> failwith "TODO: id_of_binop"

let id_of_cvtop _ op : qual_identifier =
  let open Ty in
  match op with
  | ExtS n -> Plain (Hole ("sign_extend", [ I n ]))
  | ExtU n -> Plain (Hole ("zero_extend", [ I n ]))
  | _ -> assert false

let rec term_of_expr ({ e; ty } : Expr.t) : term =
  let open Expr in
  match e with
  | Val v -> const_of_val v
  | Ptr (base, offset) ->
    let tb = const_of_val (Num (I32 base)) in
    let t = term_of_expr offset in
    App (Plain (Sym "bvadd"), [ tb; t ])
  | Unop (op, e) ->
    let id = id_of_unop ty op in
    let t = term_of_expr e in
    App (id, [ t ])
  | Binop (op, e1, e2) ->
    let id = id_of_binop ty op in
    let t1 = term_of_expr e1 in
    let t2 = term_of_expr e2 in
    App (id, [ t1; t2 ])
  | Triop (op, e1, e2, e3) ->
    let id = id_of_triop ty op in
    let t1 = term_of_expr e1 in
    let t2 = term_of_expr e2 in
    let t3 = term_of_expr e3 in
    App (id, [ t1; t2; t3 ])
  | Relop (op, e1, e2) ->
    let id = id_of_relop ty op in
    let t1 = term_of_expr e1 in
    let t2 = term_of_expr e2 in
    App (id, [ t1; t2 ])
  | Cvtop (op, e) ->
    let id = id_of_cvtop ty op in
    let t = term_of_expr e in
    App (id, [ t ])
  | Symbol x -> Id (Plain (Sym (Symbol.to_string x)))
  | Extract (e, h, l) ->
    let t = term_of_expr e in
    App (Plain (Hole ("extract", [ I (h * 8); I (l * 8) ])), [ t ])
  | Concat (e1, e2) ->
    let t1 = term_of_expr e1 in
    let t2 = term_of_expr e2 in
    App (Plain (Sym "concat"), [ t1; t2 ])

(* TODO: This can be improved *)
let script_ es =
  let consts =
    Expr.get_symbols es
    |> List.map (fun s ->
           Declare_const (Symbol.to_string s, Symbol.type_of s |> sort_of_ty) )
  in
  consts
  @ List.map (fun e -> Assert (term_of_expr @@ Expr.rewrite e)) es
  @ [ Check_sat ]

module Format = struct
  open Format

  let pp = fprintf

  let pp_const fmt = function
    | Num x -> pp_print_int fmt x
    | Dec x -> pp_print_float fmt x
    | Hex x | Bin x | Str x -> pp_print_string fmt x

  let pp_index fmt = function
    | I i -> pp_print_int fmt i
    | S i -> pp_print_string fmt i

  let pp_identifier fmt = function
    | Sym id -> pp_print_string fmt id
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
      pp fmt "(%a@ %a)" pp_qual_identifier id
        (pp_print_list ~pp_sep:pp_print_space pp_term)
        terms
    | Let _ | Forall _ | Exists _ -> assert false

  let pp_cmd fmt = function
    | Assert term -> pp fmt "(assert @[<hov 2>%a@])" pp_term term
    | Check_sat -> pp_print_string fmt "(check-sat)"
    | Check_sat_assuming -> pp_print_string fmt "(check-sat-assuming)"
    | Declare_const (x, sort) -> pp fmt "(declare-const %s %a)" x pp_sort sort
    | Declare_datatype -> pp_print_string fmt "(declare-datatype)"
    | Declare_datatypes -> pp_print_string fmt "(declare-datatypes)"
    | Declare_fun (f, args, sort) ->
      pp fmt "(declare-fun %s (%a) %a)" f
        (pp_print_list ~pp_sep:pp_print_space pp_sort)
        args pp_sort sort
    | Declare_sort (sort, n) -> pp fmt "(declare-sort %s %d)" sort n
    | Define_fun -> pp_print_string fmt "(define-fun)"
    | Define_fun_rec -> pp_print_string fmt "(define-fun-rec)"
    | Define_funs_rec -> pp_print_string fmt "(define-funs-rec)"
    | Define_sort -> pp_print_string fmt "(define-sort)"
    | Echo str -> pp fmt "(echo %s)" str
    | Exit -> pp_print_string fmt "(exit)"
    | Get_assertions -> pp_print_string fmt "(get-assertions)"
    | Get_assignment -> pp_print_string fmt "(get-assignment)"
    | Get_info -> pp_print_string fmt "(get-info)"
    | Get_model -> pp_print_string fmt "(get-model)"
    | Get_option x -> pp fmt "(get-option %s)" x
    | Get_proof -> pp_print_string fmt "(get-proof)"
    | Get_unsat_assumptions -> pp_print_string fmt "(get-unsat-assumptions)"
    | Get_unsat_core -> pp_print_string fmt "(get-unsat-core)"
    | Get_value _ -> pp_print_string fmt "(get-value)"
    | Pop n -> pp fmt "(pop %d)" n
    | Push n -> pp fmt "(push %d)" n
    | Reset -> pp_print_string fmt "(reset)"
    | Reset_assertions -> pp_print_string fmt "(reset-assertions)"
    | Set_info -> pp_print_string fmt "(set-info)"
    | Set_logic x -> pp fmt "(set-logic %s)" x
    | Set_option -> pp_print_string fmt "(set-option)"

  let pp_script fmt v = pp_print_list ~pp_sep:pp_print_newline pp_cmd fmt v
end

let of_unop (unop : Ty.Unop.t) : Feature_map.feat =
  match unop with
  | Neg -> Neg
  | Not -> Not
  | Clz -> Clz
  | Ctz -> Ctz
  | Popcnt -> Popcnt
  (* Float *)
  | Abs -> Abs
  | Sqrt -> Sqrt
  | Is_normal -> Is_normal
  | Is_subnormal -> Is_subnormal
  | Is_negative -> Is_negative
  | Is_positive -> Is_positive
  | Is_infinite -> Is_infinite
  | Is_nan -> Is_nan
  | Is_zero -> Is_zero
  | Ceil -> Ceil
  | Floor -> Floor
  | Trunc -> Trunc
  | Nearest -> Nearest
  | Head -> Head
  | Tail -> Tail
  | Reverse -> Reverse
  | Length -> Length
  (* String *)
  | Trim -> Trim
  (* RegExp *)
  | Regexp_star -> Regexp_star
  | Regexp_loop _ -> Regexp_loop
  | Regexp_plus -> Regexp_plus
  | Regexp_opt -> Regexp_opt
  | Regexp_comp -> Regexp_comp

let of_binop (binop : Ty.Binop.t) : Feature_map.feat =
  match binop with
  | Add -> Add
  | Sub -> Sub
  | Mul -> Mul
  | Div -> Div
  | DivU -> DivU
  | Rem -> Rem
  | RemU -> RemU
  | Shl -> Shl
  | ShrA -> ShrA
  | ShrL -> ShrL
  | And -> And
  | Or -> Or
  | Xor -> Xor
  | Implies -> Implies
  | Pow -> Pow
  | Min -> Min
  | Max -> Max
  | Copysign -> Copysign
  | Rotl -> Rotl
  | Rotr -> Rotr
  | At -> At
  | List_cons -> List_cons
  | List_append -> List_append
  (* String *)
  | String_prefix -> String_prefix
  | String_suffix -> String_suffix
  | String_contains -> String_contains
  | String_last_index -> String_last_index
  | String_in_re -> String_in_re
  (* Regexp *)
  | Regexp_range -> Regexp_range
  | Regexp_inter -> Regexp_inter
  | Regexp_diff -> Regexp_diff

let of_triop (triop : Ty.Triop.t) : Feature_map.feat =
  match triop with
  | Ite -> Ite
  | List_set -> List_set
  (* String *)
  | String_extract -> String_extract
  | String_replace -> String_replace
  | String_index -> String_index
  | String_replace_all -> String_replace_all
  | String_replace_re -> String_replace_re
  | String_replace_re_all -> String_replace_re_all

let of_relop (relop : Ty.Relop.t) : Feature_map.feat =
  match relop with
  | Eq -> Eq
  | Ne -> Ne
  | Lt -> Lt
  | LtU -> LtU
  | Le -> Le
  | LeU -> LeU

let of_cvtop (cvtop : Ty.Cvtop.t) : Feature_map.feat =
  match cvtop with
  | ToString -> ToString
  | OfString -> OfString
  | ToBool -> ToBool
  | OfBool -> OfBool
  | Reinterpret_int -> Reinterpret_int
  | Reinterpret_float -> Reinterpret_float
  | DemoteF64 -> DemoteF64
  | PromoteF32 -> PromoteF32
  | ConvertSI32 -> ConvertSI32
  | ConvertUI32 -> ConvertUI32
  | ConvertSI64 -> ConvertSI64
  | ConvertUI64 -> ConvertUI64
  | TruncSF32 -> TruncSF32
  | TruncUF32 -> TruncUF32
  | TruncSF64 -> TruncSF64
  | TruncUF64 -> TruncUF64
  | Trunc_sat_f32_s -> Trunc_sat_f32_s
  | Trunc_sat_f32_u -> Trunc_sat_f32_u
  | Trunc_sat_f64_s -> Trunc_sat_f64_s
  | Trunc_sat_f64_u -> Trunc_sat_f64_u
  | WrapI64 -> WrapI64
  | Sign_extend _ -> Sign_extend
  | Zero_extend _ -> Zero_extend
  (* String *)
  | String_to_code -> String_to_code
  | String_from_code -> String_from_code
  | String_to_int -> String_to_int
  | String_from_int -> String_from_int
  | String_to_float -> String_to_float
  | String_to_re -> String_to_re

let of_ty (ty : Ty.t) : Feature_map.feat =
  match ty with
  | Ty_app -> Ty_app
  | Ty_bitv _ -> Ty_bitv
  | Ty_bool -> Ty_bool
  | Ty_fp _ -> Ty_fp
  | Ty_int -> Ty_int
  | Ty_list -> Ty_list
  | Ty_none -> Ty_none
  | Ty_real -> Ty_real
  | Ty_str -> Ty_str
  | Ty_unit -> Ty_unit
  | Ty_regexp -> Ty_regexp
  | Ty_roundingMode -> Ty_roundingMode

let of_naryop (naryop : Ty.Naryop.t) : Feature_map.feat =
  match naryop with
  | Logand -> Logand
  | Logor -> Logor
  | Concat -> Concat
  | Regexp_union -> Regexp_union
  | Distinct -> Distinct

let of_expr_kind (e : Expr.expr) _ty : Feature_map.feat =
  match e with
  | Val _ -> Val
  | Ptr _ -> Ptr
  | Symbol _ -> Symbol
  | List _ -> List_expr
  | App _ -> App
  | Unop _ -> Unop_expr
  | Binop _ -> Binop_expr
  | Triop _ -> Triop_expr
  | Relop _ -> Relop_expr
  | Cvtop _ -> Cvtop_expr
  | Naryop _ -> Naryop_expr
  | Extract _ -> Extract
  | Concat _ -> Concat
  | Binder _ -> Binder

let all_feats : Feature_map.feat Iarray.t =
  Feature_map.
    [| (* Metadata *)
       Max_depth
     ; Mean_depth
     ; Nb_queries
     ; Time
     ; (* Expr kinds *)
       Val
     ; Ptr
     ; Symbol
     ; List_expr
     ; App
     ; Unop_expr
     ; Binop_expr
     ; Triop_expr
     ; Relop_expr
     ; Cvtop_expr
     ; Naryop_expr
     ; Extract
     ; Concat
     ; Binder
     ; (* Unops *)
       Neg
     ; Not
     ; Clz
     ; Ctz
     ; Popcnt
     ; Abs
     ; Sqrt
     ; Is_normal
     ; Is_subnormal
     ; Is_negative
     ; Is_positive
     ; Is_infinite
     ; Is_nan
     ; Is_zero
     ; Ceil
     ; Floor
     ; Trunc
     ; Nearest
     ; Head
     ; Tail
     ; Reverse
     ; Length
     ; Trim
     ; Regexp_star
     ; Regexp_loop
     ; Regexp_plus
     ; Regexp_opt
     ; Regexp_comp
     ; (* Binops *)
       Add
     ; Sub
     ; Mul
     ; Div
     ; DivU
     ; Rem
     ; RemU
     ; Shl
     ; ShrA
     ; ShrL
     ; And
     ; Or
     ; Xor
     ; Implies
     ; Pow
     ; Min
     ; Max
     ; Copysign
     ; Rotl
     ; Rotr
     ; At
     ; List_cons
     ; List_append
     ; String_prefix
     ; String_suffix
     ; String_contains
     ; String_last_index
     ; String_in_re
     ; Regexp_range
     ; Regexp_inter
     ; Regexp_diff
     ; (* Triops *)
       Ite
     ; List_set
     ; String_extract
     ; String_replace
     ; String_index
     ; String_replace_all
     ; String_replace_re
     ; String_replace_re_all
     ; (* Relops *)
       Eq
     ; Ne
     ; Lt
     ; LtU
     ; Le
     ; LeU
     ; (* Cvtops *)
       ToString
     ; OfString
     ; ToBool
     ; OfBool
     ; Reinterpret_int
     ; Reinterpret_float
     ; DemoteF64
     ; PromoteF32
     ; ConvertSI32
     ; ConvertUI32
     ; ConvertSI64
     ; ConvertUI64
     ; TruncSF32
     ; TruncUF32
     ; TruncSF64
     ; TruncUF64
     ; Trunc_sat_f32_s
     ; Trunc_sat_f32_u
     ; Trunc_sat_f64_s
     ; Trunc_sat_f64_u
     ; WrapI64
     ; Sign_extend
     ; Zero_extend
     ; String_to_code
     ; String_from_code
     ; String_to_int
     ; String_from_int
     ; String_to_float
     ; String_to_re
     ; (* Naryops *)
       Logand
     ; Logor
     ; Regexp_union
     ; (* Types *)
       Ty_app
     ; Ty_bitv
     ; Ty_bool
     ; Ty_fp
     ; Ty_int
     ; Ty_list
     ; Ty_none
     ; Ty_real
     ; Ty_str
     ; Ty_unit
     ; Ty_regexp
     ; Ty_roundingMode
    |]

let all_feature_names =
  "solver" :: "model"
  :: Iarray.fold_right
       (fun feat acc -> Feature_map.feat_to_string feat :: acc)
       all_feats []

let feats_to_str solver_name model feats =
  let values =
    solver_name :: Bool.to_string model :: "solver" :: "model"
    :: Iarray.fold_right
         (fun feat acc -> string_of_int (Feature_map.get_feat feat feats) :: acc)
         all_feats []
  in
  String.cat (String.concat "," values) "\n"

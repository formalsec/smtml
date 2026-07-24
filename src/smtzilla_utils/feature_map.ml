(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2026 formalsec *)
(* Written by the Smtml programmers *)

type feat =
  (* Expr kinds *)
  | Val
  | Ptr
  | Symbol
  | List_expr
  | App
  | Unop_expr
  | Binop_expr
  | Triop_expr
  | Relop_expr
  | Cvtop_expr
  | Naryop_expr
  | Extract
  | Concat
  | Binder
  (* Unops *)
  | Neg
  | Not
  | Clz
  | Ctz
  | Popcnt
  | Abs
  | Sqrt
  | Is_normal
  | Is_subnormal
  | Is_negative
  | Is_positive
  | Is_infinite
  | Is_nan
  | Is_zero
  | Ceil
  | Floor
  | Trunc
  | Nearest
  | Head
  | Tail
  | Reverse
  | Length
  | Trim
  | Regexp_star
  | Regexp_loop
  | Regexp_plus
  | Regexp_opt
  | Regexp_comp
  (* Binops *)
  | Add
  | Sub
  | Mul
  | Div
  | DivU
  | Mod
  | Rem
  | RemU
  | Shl
  | ShrA
  | ShrL
  | And
  | Or
  | Xor
  | Implies
  | Pow
  | Min
  | Max
  | Copysign
  | Rotl
  | Rotr
  | Ext_rotl
  | Ext_rotr
  | At
  | List_cons
  | List_append
  | String_prefix
  | String_suffix
  | String_contains
  | String_last_index
  | String_in_re
  | Regexp_range
  | Regexp_inter
  | Regexp_diff
  (* Triops *)
  | Ite
  | List_set
  | String_extract
  | String_replace
  | String_index
  | String_replace_all
  | String_replace_re
  | String_replace_re_all
  (* Relops *)
  | Eq
  | Ne
  | Lt
  | LtU
  | Le
  | LeU
  (* Cvtops *)
  | ToString
  | OfString
  | ToBool
  | OfBool
  | Reinterpret_int
  | Reinterpret_float
  | DemoteF64
  | PromoteF32
  | ConvertSI32
  | ConvertUI32
  | ConvertSI64
  | ConvertUI64
  | TruncSF32
  | TruncUF32
  | TruncSF64
  | TruncUF64
  | Trunc_sat_f32_s
  | Trunc_sat_f32_u
  | Trunc_sat_f64_s
  | Trunc_sat_f64_u
  | WrapI64
  | Sign_extend
  | Zero_extend
  | String_to_code
  | String_from_code
  | String_to_int
  | String_from_int
  | String_to_float
  | String_to_re
  (* Naryops *)
  | Logand
  | Logor
  | Regexp_union
  | Distinct
  (* Types *)
  | Ty_app
  | Ty_bitv
  | Ty_bool
  | Ty_fp
  | Ty_int
  | Ty_list
  | Ty_none
  | Ty_real
  | Ty_str
  | Ty_unit
  | Ty_regexp
  | Ty_roundingMode
  (* Metadata *)
  | Depth
  | Max_depth
  | Time
  | Nb_queries
  | Mean_depth

module Map = struct
  include Map.Make (struct
    type t = feat

    let compare : feat -> feat -> int = Stdlib.compare
  end)

  let find_def0 k m = match find_opt k m with Some n -> n | None -> 0
end

type t = int Map.t

let empty = Map.empty

let union m1 m2 =
  Map.union
    (fun key v1 v2 ->
      match key with Depth -> Some (Int.max v1 v2) | _ -> Some (v1 + v2) )
    m1 m2

let incr_feat key m =
  Map.update key (function None -> Some 1 | Some c -> Some (c + 1)) m

let get_feat key m = Map.find_def0 key m

let add_time time m = Map.add Time time m

let add_depth depth m = Map.add Depth depth m

let get_depth m = Map.find_def0 Depth m

let add_nb_queries nb_exprs = Map.add Nb_queries nb_exprs

let add_mean_depth mean_depth = Map.add Mean_depth mean_depth

let rename_depth_to_max_depth m =
  Map.add Max_depth
    (match Map.find_opt Depth m with None -> assert false | Some v -> v)
    (Map.remove Depth m)

let feat_to_string = function
  | Val -> "Val"
  | Ptr -> "Ptr"
  | Symbol -> "Symbol"
  | List_expr -> "List"
  | App -> "App"
  | Unop_expr -> "Unop"
  | Binop_expr -> "Binop"
  | Triop_expr -> "Triop"
  | Relop_expr -> "Relop"
  | Cvtop_expr -> "Cvtop"
  | Naryop_expr -> "Naryop"
  | Extract -> "Extract"
  | Concat -> "Concat"
  | Binder -> "Binder"
  | Neg -> "Neg"
  | Not -> "Not"
  | Clz -> "Clz"
  | Ctz -> "Ctz"
  | Popcnt -> "Popcnt"
  | Abs -> "Abs"
  | Sqrt -> "Sqrt"
  | Is_normal -> "Is_normal"
  | Is_subnormal -> "Is_subnormal"
  | Is_negative -> "Is_negative"
  | Is_positive -> "Is_positive"
  | Is_infinite -> "Is_infinite"
  | Is_nan -> "Is_nan"
  | Is_zero -> "Is_zero"
  | Ceil -> "Ceil"
  | Floor -> "Floor"
  | Trunc -> "Trunc"
  | Nearest -> "Nearest"
  | Head -> "Head"
  | Tail -> "Tail"
  | Reverse -> "Reverse"
  | Length -> "Length"
  | Trim -> "Trim"
  | Regexp_star -> "Regexp_star"
  | Regexp_loop -> "Regexp_loop"
  | Regexp_plus -> "Regexp_plus"
  | Regexp_opt -> "Regexp_opt"
  | Regexp_comp -> "Regexp_comp"
  | Add -> "Add"
  | Sub -> "Sub"
  | Mul -> "Mul"
  | Div -> "Div"
  | DivU -> "DivU"
  | Mod -> "Mod"
  | Rem -> "Rem"
  | RemU -> "RemU"
  | Shl -> "Shl"
  | ShrA -> "ShrA"
  | ShrL -> "ShrL"
  | And -> "And"
  | Or -> "Or"
  | Xor -> "Xor"
  | Implies -> "Implies"
  | Pow -> "Pow"
  | Min -> "Min"
  | Max -> "Max"
  | Copysign -> "Copysign"
  | Rotl -> "Rotl"
  | Rotr -> "Rotr"
  | Ext_rotl -> "Ext_rotl"
  | Ext_rotr -> "Ext_rotr"
  | At -> "At"
  | List_cons -> "List_cons"
  | List_append -> "List_append"
  | String_prefix -> "String_prefix"
  | String_suffix -> "String_suffix"
  | String_contains -> "String_contains"
  | String_last_index -> "String_last_index"
  | String_in_re -> "String_in_re"
  | Regexp_range -> "Regexp_range"
  | Regexp_inter -> "Regexp_inter"
  | Regexp_diff -> "Regexp_diff"
  | Ite -> "Ite"
  | List_set -> "List_set"
  | String_extract -> "String_extract"
  | String_replace -> "String_replace"
  | String_index -> "String_index"
  | String_replace_all -> "String_replace_all"
  | String_replace_re -> "String_replace_re"
  | String_replace_re_all -> "String_replace_re_all"
  | Eq -> "Eq"
  | Ne -> "Ne"
  | Lt -> "Lt"
  | LtU -> "LtU"
  | Le -> "Le"
  | LeU -> "LeU"
  | ToString -> "ToString"
  | OfString -> "OfString"
  | ToBool -> "ToBool"
  | OfBool -> "OfBool"
  | Reinterpret_int -> "Reinterpret_int"
  | Reinterpret_float -> "Reinterpret_float"
  | DemoteF64 -> "DemoteF64"
  | PromoteF32 -> "PromoteF32"
  | ConvertSI32 -> "ConvertSI32"
  | ConvertUI32 -> "ConvertUI32"
  | ConvertSI64 -> "ConvertSI64"
  | ConvertUI64 -> "ConvertUI64"
  | TruncSF32 -> "TruncSF32"
  | TruncUF32 -> "TruncUF32"
  | TruncSF64 -> "TruncSF64"
  | TruncUF64 -> "TruncUF64"
  | Trunc_sat_f32_s -> "Trunc_sat_f32_s"
  | Trunc_sat_f32_u -> "Trunc_sat_f32_u"
  | Trunc_sat_f64_s -> "Trunc_sat_f64_s"
  | Trunc_sat_f64_u -> "Trunc_sat_f64_u"
  | WrapI64 -> "WrapI64"
  | Sign_extend -> "Sign_extend"
  | Zero_extend -> "Zero_extend"
  | String_to_code -> "String_to_code"
  | String_from_code -> "String_from_code"
  | String_to_int -> "String_to_int"
  | String_from_int -> "String_from_int"
  | String_to_float -> "String_to_float"
  | String_to_re -> "String_to_re"
  | Logand -> "Logand"
  | Logor -> "Logor"
  | Regexp_union -> "Regexp_union"
  | Distinct -> "Distinct"
  | Ty_app -> "Ty_app"
  | Ty_bitv -> "Ty_bitv"
  | Ty_bool -> "Ty_bool"
  | Ty_fp -> "Ty_fp"
  | Ty_int -> "Ty_int"
  | Ty_list -> "Ty_list"
  | Ty_none -> "Ty_none"
  | Ty_real -> "Ty_real"
  | Ty_str -> "Ty_str"
  | Ty_unit -> "Ty_unit"
  | Ty_regexp -> "Ty_regexp"
  | Ty_roundingMode -> "Ty_roundingMode"
  | Depth -> "depth"
  | Max_depth -> "max_depth"
  | Time -> "time"
  | Nb_queries -> "nb_queries"
  | Mean_depth -> "mean_depth"

let feat_of_string = function
  | "Val" -> Val
  | "Ptr" -> Ptr
  | "Symbol" -> Symbol
  | "List" -> List_expr
  | "App" -> App
  | "Unop" -> Unop_expr
  | "Binop" -> Binop_expr
  | "Triop" -> Triop_expr
  | "Relop" -> Relop_expr
  | "Cvtop" -> Cvtop_expr
  | "Naryop" -> Naryop_expr
  | "Extract" -> Extract
  | "Concat" -> Concat
  | "Binder" -> Binder
  | "Neg" -> Neg
  | "Not" -> Not
  | "Clz" -> Clz
  | "Ctz" -> Ctz
  | "Popcnt" -> Popcnt
  | "Abs" -> Abs
  | "Sqrt" -> Sqrt
  | "Is_normal" -> Is_normal
  | "Is_subnormal" -> Is_subnormal
  | "Is_negative" -> Is_negative
  | "Is_positive" -> Is_positive
  | "Is_infinite" -> Is_infinite
  | "Is_nan" -> Is_nan
  | "Is_zero" -> Is_zero
  | "Ceil" -> Ceil
  | "Floor" -> Floor
  | "Trunc" -> Trunc
  | "Nearest" -> Nearest
  | "Head" -> Head
  | "Tail" -> Tail
  | "Reverse" -> Reverse
  | "Length" -> Length
  | "Trim" -> Trim
  | "Regexp_star" -> Regexp_star
  | "Regexp_loop" -> Regexp_loop
  | "Regexp_plus" -> Regexp_plus
  | "Regexp_opt" -> Regexp_opt
  | "Regexp_comp" -> Regexp_comp
  | "Add" -> Add
  | "Sub" -> Sub
  | "Mul" -> Mul
  | "Div" -> Div
  | "DivU" -> DivU
  | "Mod" -> Mod
  | "Rem" -> Rem
  | "RemU" -> RemU
  | "Shl" -> Shl
  | "ShrA" -> ShrA
  | "ShrL" -> ShrL
  | "And" -> And
  | "Or" -> Or
  | "Xor" -> Xor
  | "Implies" -> Implies
  | "Pow" -> Pow
  | "Min" -> Min
  | "Max" -> Max
  | "Copysign" -> Copysign
  | "Rotl" -> Rotl
  | "Rotr" -> Rotr
  | "Ext_rotl" -> Ext_rotl
  | "Ext_rotr" -> Ext_rotr
  | "At" -> At
  | "List_cons" -> List_cons
  | "List_append" -> List_append
  | "String_prefix" -> String_prefix
  | "String_suffix" -> String_suffix
  | "String_contains" -> String_contains
  | "String_last_index" -> String_last_index
  | "String_in_re" -> String_in_re
  | "Regexp_range" -> Regexp_range
  | "Regexp_inter" -> Regexp_inter
  | "Regexp_diff" -> Regexp_diff
  | "Ite" -> Ite
  | "List_set" -> List_set
  | "String_extract" -> String_extract
  | "String_replace" -> String_replace
  | "String_index" -> String_index
  | "String_replace_all" -> String_replace_all
  | "String_replace_re" -> String_replace_re
  | "String_replace_re_all" -> String_replace_re_all
  | "Eq" -> Eq
  | "Ne" -> Ne
  | "Lt" -> Lt
  | "LtU" -> LtU
  | "Le" -> Le
  | "LeU" -> LeU
  | "ToString" -> ToString
  | "OfString" -> OfString
  | "ToBool" -> ToBool
  | "OfBool" -> OfBool
  | "Reinterpret_int" -> Reinterpret_int
  | "Reinterpret_float" -> Reinterpret_float
  | "DemoteF64" -> DemoteF64
  | "PromoteF32" -> PromoteF32
  | "ConvertSI32" -> ConvertSI32
  | "ConvertUI32" -> ConvertUI32
  | "ConvertSI64" -> ConvertSI64
  | "ConvertUI64" -> ConvertUI64
  | "TruncSF32" -> TruncSF32
  | "TruncUF32" -> TruncUF32
  | "TruncSF64" -> TruncSF64
  | "TruncUF64" -> TruncUF64
  | "Trunc_sat_f32_s" -> Trunc_sat_f32_s
  | "Trunc_sat_f32_u" -> Trunc_sat_f32_u
  | "Trunc_sat_f64_s" -> Trunc_sat_f64_s
  | "Trunc_sat_f64_u" -> Trunc_sat_f64_u
  | "WrapI64" -> WrapI64
  | "Sign_extend" -> Sign_extend
  | "Zero_extend" -> Zero_extend
  | "String_to_code" -> String_to_code
  | "String_from_code" -> String_from_code
  | "String_to_int" -> String_to_int
  | "String_from_int" -> String_from_int
  | "String_to_float" -> String_to_float
  | "String_to_re" -> String_to_re
  | "Logand" -> Logand
  | "Logor" -> Logor
  | "Regexp_union" -> Regexp_union
  | "Distinct" -> Distinct
  | "Ty_app" -> Ty_app
  | "Ty_bitv" -> Ty_bitv
  | "Ty_bool" -> Ty_bool
  | "Ty_fp" -> Ty_fp
  | "Ty_int" -> Ty_int
  | "Ty_list" -> Ty_list
  | "Ty_none" -> Ty_none
  | "Ty_real" -> Ty_real
  | "Ty_str" -> Ty_str
  | "Ty_unit" -> Ty_unit
  | "Ty_regexp" -> Ty_regexp
  | "Ty_roundingMode" -> Ty_roundingMode
  | "depth" -> Depth
  | "max_depth" -> Max_depth
  | "time" -> Time
  | "nb_queries" -> Nb_queries
  | "mean_depth" -> Mean_depth
  | s -> Fmt.failwith "Unknown feature: %s" s

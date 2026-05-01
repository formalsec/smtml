(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2026 formalsec *)
(* Written by the Smtml programmers *)

open Smtml_prelude.Result.Syntax

let string_of_unop (unop : Ty.Unop.t) : string =
  match unop with
  | Neg -> "Neg"
  | Not -> "Not"
  | Clz -> "Clz"
  | Ctz -> "Ctz"
  | Popcnt -> "Popcnt"
  (* Float *)
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
  (* String *)
  | Trim -> "Trim"
  (* RegExp *)
  | Regexp_star -> "Regexp_star"
  | Regexp_loop _ -> "Regexp_loop"
  | Regexp_plus -> "Regexp_plus"
  | Regexp_opt -> "Regexp_opt"
  | Regexp_comp -> "Regexp_comp"

let string_of_binop (binop : Ty.Binop.t) : string =
  match binop with
  | Add -> "Add"
  | Sub -> "Sub"
  | Mul -> "Mul"
  | Div -> "Div"
  | DivU -> "DivU"
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
  | At -> "At"
  | List_cons -> "List_cons"
  | List_append -> "List_append"
  (* String *)
  | String_prefix -> "String_prefix"
  | String_suffix -> "String_suffix"
  | String_contains -> "String_contains"
  | String_last_index -> "String_last_index"
  | String_in_re -> "String_in_re"
  (* Regexp *)
  | Regexp_range -> "Regexp_range"
  | Regexp_inter -> "Regexp_inter"
  | Regexp_diff -> "Regexp_diff"

let string_of_triop (triop : Ty.Triop.t) : string =
  match triop with
  | Ite -> "Ite"
  | List_set -> "List_set"
  (* String *)
  | String_extract -> "String_extract"
  | String_replace -> "String_replace"
  | String_index -> "String_index"
  | String_replace_all -> "String_replace_all"
  | String_replace_re -> "String_replace_re"
  | String_replace_re_all -> "String_replace_re_all"

let string_of_relop (relop : Ty.Relop.t) : string =
  match relop with
  | Eq -> "Eq"
  | Ne -> "Ne"
  | Lt -> "Lt"
  | LtU -> "LtU"
  | Le -> "Le"
  | LeU -> "LeU"

let string_of_cvtop (cvtop : Ty.Cvtop.t) : string =
  match cvtop with
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
  | Sign_extend _ -> "Sign_extend"
  | Zero_extend _ -> "Zero_extend"
  (* String *)
  | String_to_code -> "String_to_code"
  | String_from_code -> "String_from_code"
  | String_to_int -> "String_to_int"
  | String_from_int -> "String_from_int"
  | String_to_float -> "String_to_float"
  | String_to_re -> "String_to_re"

let string_of_ty (ty : Ty.t) : string =
  match ty with
  | Ty_app -> "Ty_app"
  | Ty_bitv _ -> "Ty_bitv"
  | Ty_bool -> "Ty_bool"
  | Ty_fp _ -> "Ty_fp"
  | Ty_int -> "Ty_int"
  | Ty_list -> "Ty_list"
  | Ty_none -> "Ty_none"
  | Ty_real -> "Ty_real"
  | Ty_str -> "Ty_str"
  | Ty_unit -> "Ty_unit"
  | Ty_regexp -> "Ty_regexp"
  | Ty_roundingMode -> "Ty_roundingMode"

let string_of_naryop (naryop : Ty.Naryop.t) : string =
  match naryop with
  | Logand -> "Logand"
  | Logor -> "Logor"
  | Concat -> "Concat"
  | Regexp_union -> "Regexp_union"
  | Distinct -> "Distinct"

let string_of_expr_kind (e : Expr.expr) _ty : string =
  match e with
  | Val _ -> "Val"
  | Ptr _ -> "Ptr"
  | Symbol _ -> "Symbol"
  | List _ -> "List"
  | App _ -> "App"
  | Unop _ -> "Unop"
  | Binop _ -> "Binop"
  | Triop _ -> "Triop"
  | Relop _ -> "Relop"
  | Cvtop _ -> "Cvtop"
  | Naryop _ -> "Naryop"
  | Extract _ -> "Extract"
  | Concat _ -> "Concat"
  | Binder _ -> "Binder"

(* Define all constructors you want to track *)
let ctor_names =
  let expr_kinds =
    [ "Val"
    ; "Ptr"
    ; "Symbol"
    ; "List"
    ; "App"
    ; "Unop"
    ; "Binop"
    ; "Triop"
    ; "Relop"
    ; "Cvtop"
    ; "Naryop"
    ; "Extract"
    ; "Concat"
    ; "Binder"
    ]
  in
  let unops =
    List.map string_of_unop
      [ Ty.Unop.Neg
      ; Not
      ; Clz
      ; Ctz
      ; Popcnt
      ; Abs (* Float *)
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
      ; Trim (* String *)
      ; Regexp_star (* RegExp *)
      ; Regexp_loop (0, 0)
      ; Regexp_plus
      ; Regexp_opt
      ; Regexp_comp
      ]
  in
  let binops =
    List.map string_of_binop
      [ Add
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
      ; List_append (* String *)
      ; String_prefix
      ; String_suffix
      ; String_contains
      ; String_last_index
      ; String_in_re (* Regexp *)
      ; Regexp_range
      ; Regexp_inter
      ; Regexp_diff
      ]
  in
  let triops =
    List.map string_of_triop
      [ Ite
      ; List_set (* String *)
      ; String_extract
      ; String_replace
      ; String_index
      ; String_replace_all
      ; String_replace_re
      ; String_replace_re_all
      ]
  in
  let relops = List.map string_of_relop [ Eq; Ne; Lt; LtU; Le; LeU ] in
  let cvtops =
    List.map string_of_cvtop
      [ ToString
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
      ; Sign_extend 0
      ; Zero_extend 0 (* String *)
      ; String_to_code
      ; String_from_code
      ; String_to_int
      ; String_from_int
      ; String_to_float
      ; String_to_re
      ]
  in
  let naryop =
    List.map string_of_naryop [ Logand; Logor; Concat; Regexp_union ]
  in
  let tys =
    List.map string_of_ty
      [ Ty_app
      ; Ty_bitv 0
      ; Ty_bool
      ; Ty_fp 0
      ; Ty_int
      ; Ty_list
      ; Ty_none
      ; Ty_real
      ; Ty_str
      ; Ty_unit
      ; Ty_regexp
      ; Ty_roundingMode
      ]
  in
  expr_kinds @ unops @ binops @ triops @ relops @ cvtops @ naryop @ tys

let final_names =
  "solver" :: "model" :: "max_depth" :: "mean_depth" :: "nb_queries" :: "time"
  :: ctor_names

(* initialize feature map with all zeros *)

let extract_feats_aux : Expr.t -> Features.t =
  let rec visit depth (feats : Features.t) (e : Expr.t) =
    let feats =
      Features.incr_feat (string_of_expr_kind e.node (Expr.ty e)) feats
    in
    match Expr.view e with
    | Val _ | Symbol _ -> (depth, feats)
    | Ptr { offset; _ } -> visit (depth + 1) feats offset
    | List lst ->
      List.fold_left
        (fun (depth, feats) e ->
          let depth', feats = visit depth feats e in
          (Int.max depth depth', feats) )
        (depth + 1, feats)
        lst
    | Naryop (ty, naryop, lst) ->
      let feats = Features.incr_feat (string_of_ty ty) feats in
      let feats = Features.incr_feat (string_of_naryop naryop) feats in
      List.fold_left
        (fun (depth, feats) e ->
          let depth', feats = visit depth feats e in
          (Int.max depth depth', feats) )
        (depth + 1, feats)
        lst
    | App (_, lst) ->
      List.fold_left
        (fun (depth, feats) e ->
          let depth', feats = visit depth feats e in
          (Int.max depth depth', feats) )
        (depth + 1, feats)
        lst
    | Unop (ty, unop, t) ->
      let feats = Features.incr_feat (string_of_ty ty) feats in
      let feats = Features.incr_feat (string_of_unop unop) feats in
      visit (depth + 1) feats t
    | Cvtop (ty, cvtop, t) ->
      let feats = Features.incr_feat (string_of_ty ty) feats in
      let feats = Features.incr_feat (string_of_cvtop cvtop) feats in
      visit (depth + 1) feats t
    | Extract (t, _, _) -> visit (depth + 1) feats t
    | Binop (ty, binop, e1, e2) ->
      let feats = Features.incr_feat (string_of_ty ty) feats in
      let feats = Features.incr_feat (string_of_binop binop) feats in
      let depth1, feats = visit (depth + 1) feats e1 in
      let depth2, feats = visit (depth + 1) feats e2 in
      (Int.max depth1 depth2, feats)
    | Relop (ty, relop, e1, e2) ->
      let feats = Features.incr_feat (string_of_ty ty) feats in
      let feats = Features.incr_feat (string_of_relop relop) feats in
      let depth1, feats = visit (depth + 1) feats e1 in
      let depth2, feats = visit (depth + 1) feats e2 in
      (Int.max depth1 depth2, feats)
    | Concat (e1, e2) ->
      let depth1, feats = visit (depth + 1) feats e1 in
      let depth2, feats = visit (depth + 1) feats e2 in
      (Int.max depth1 depth2, feats)
    | Triop (ty, triop, e1, e2, e3) ->
      let feats = Features.incr_feat (string_of_ty ty) feats in
      let feats = Features.incr_feat (string_of_triop triop) feats in
      let depth1, feats = visit (depth + 1) feats e1 in
      let depth2, feats = visit (depth + 1) feats e2 in
      let depth3, feats = visit (depth + 1) feats e3 in
      (Int.max (Int.max depth1 depth2) depth3, feats)
    | Binder (_, lst, t) ->
      List.fold_left
        (fun (depth, feats) e ->
          let depth', feats = visit depth feats e in
          (Int.max depth depth', feats) )
        (depth + 1, feats)
        (t :: lst)
  in
  fun expr ->
    let depth, feats = visit 1 Features.empty expr in
    Features.add_depth depth feats

let rec read_marshalled_queries results ic : unit =
  let res :
    (string * Expr.t list * bool * int64 * [ `Sat | `Unsat | `Unknown ]) list =
    Marshal.from_channel ic
  in
  Log.debug (fun k -> k "Read %d results@." (List.length res));
  results := List.rev_append res !results;
  read_marshalled_queries results ic

let read_marshalled_file (path : Fpath.t) =
  let results = ref [] in
  let res =
    Bos.OS.File.with_ic path
      (fun ic () ->
        try read_marshalled_queries results ic
        with End_of_file ->
          Log.debug (fun k -> k "Finished reading results@.") )
      ()
  in
  res >>| fun () -> List.rev !results

let extract_feats assertions : Features.t =
  let feats, depth_acc =
    List.fold_left
      (fun (feats_acc, depth_acc) expr ->
        let feats = extract_feats_aux expr in
        let depth_acc = depth_acc + Features.get_depth feats in
        let feats_acc = Features.union feats feats_acc in
        (feats_acc, depth_acc) )
      (Features.empty, 0) assertions
  in
  let nb_exprs = List.length assertions in
  Features.add_nb_queries nb_exprs
  @@ Features.add_mean_depth (depth_acc / nb_exprs)
  @@ Features.rename_depth_to_max_depth feats

let extract_feats_wtime assertions runtime =
  Features.add_time (Int64.to_int runtime) (extract_feats assertions)

let cmd marshalled_file output_csv =
  let res =
    read_marshalled_file marshalled_file >>| fun entries ->
    Bos.OS.File.with_oc output_csv
      (fun oc entries ->
        Out_channel.output_string oc
          (String.cat (String.concat "," final_names) "\n");
        List.iter
          (fun (solver_name, exprs, model, t, _) ->
            if List.compare_lengths exprs [] > 0 then
              let feats = extract_feats_wtime exprs t in
              let row =
                List.map
                  (fun name ->
                    if String.equal name "solver" then solver_name
                    else if String.equal name "model" then Bool.to_string model
                    else
                      let count = Features.get_feat name feats in
                      string_of_int count )
                  final_names
              in
              let row = String.cat (String.concat "," row) "\n" in
              Out_channel.output_string oc row )
          entries;
        Ok () )
      entries
  in
  match Result.join (Result.join res) with
  | Error (`Msg m) -> Fmt.failwith "%s" m
  | Ok () -> Log.debug (fun k -> k "Done writing to %a\n%!" Fpath.pp output_csv)

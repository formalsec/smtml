(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2024 formalsec *)
(* Written by the Smtml programmers *)

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
  | Gt -> "Gt"
  | GtU -> "GtU"
  | Le -> "Le"
  | LeU -> "LeU"
  | Ge -> "Ge"
  | GeU -> "GeU"

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

let string_of_expr_kind (e : Expr.expr) _ty : string =
  match e with
  | Val _ -> "Val"
  | Ptr _ -> "Ptr"
  | Loc _ -> "Loc"
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
  let relops =
    List.map string_of_relop [ Eq; Ne; Lt; LtU; Gt; GtU; Le; LeU; Ge; GeU ]
  in
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

module StringMap = struct
  include Map.Make (String)

  let find_def0 k m = match find_opt k m with Some n -> n | None -> 0
end

(* initialize feature map with all zeros *)

let extract_feats : Expr.t -> int StringMap.t =
  let incr_feat feats key =
    StringMap.update key
      (function None -> Some 1 | Some c -> Some (c + 1))
      feats
  in
  let rec visit depth feats (e : Expr.t) =
    let feats = incr_feat feats (string_of_expr_kind e.node (Expr.ty e)) in
    match e.node with
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
      let feats = incr_feat feats (string_of_ty ty) in
      let feats = incr_feat feats (string_of_naryop naryop) in
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
      let feats = incr_feat feats (string_of_ty ty) in
      let feats = incr_feat feats (string_of_unop unop) in
      visit (depth + 1) feats t
    | Cvtop (ty, cvtop, t) ->
      let feats = incr_feat feats (string_of_ty ty) in
      let feats = incr_feat feats (string_of_cvtop cvtop) in
      visit (depth + 1) feats t
    | Extract (t, _, _) -> visit (depth + 1) feats t
    | Binop (ty, binop, e1, e2) ->
      let feats = incr_feat feats (string_of_ty ty) in
      let feats = incr_feat feats (string_of_binop binop) in
      let depth1, feats = visit (depth + 1) feats e1 in
      let depth2, feats = visit (depth + 1) feats e2 in
      (Int.max depth1 depth2, feats)
    | Relop (ty, relop, e1, e2) ->
      let feats = incr_feat feats (string_of_ty ty) in
      let feats = incr_feat feats (string_of_relop relop) in
      let depth1, feats = visit (depth + 1) feats e1 in
      let depth2, feats = visit (depth + 1) feats e2 in
      (Int.max depth1 depth2, feats)
    | Concat (e1, e2) ->
      let depth1, feats = visit (depth + 1) feats e1 in
      let depth2, feats = visit (depth + 1) feats e2 in
      (Int.max depth1 depth2, feats)
    | Triop (ty, triop, e1, e2, e3) ->
      let feats = incr_feat feats (string_of_ty ty) in
      let feats = incr_feat feats (string_of_triop triop) in
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
    | Loc _ -> assert false
  in
  fun expr ->
    let depth, feats = visit 1 StringMap.empty expr in
    StringMap.add "depth" depth feats

let read_marshalled_file path : (string * Expr.t list * bool * int64) list =
  let results = ref [] in
  try
    let ic = In_channel.open_bin path in
    ( try
        while true do
          let res : (string * Expr.t list * bool * int64) list =
            Marshal.from_channel ic
          in
          Fmt.pr "Read %d results@." (List.length res);
          results := List.rev_append res !results
        done
      with End_of_file -> Fmt.pr "Finished reading results@." );
    In_channel.close ic;
    List.rev !results
  with e ->
    Fmt.epr "Failed to read %s\nBecause %s\n%!" path (Printexc.to_string e);
    []

let extract_feats_wtime assertions runtime =
  let feats, depth_acc =
    List.fold_left
      (fun (feats_acc, depth_acc) expr ->
        let feats = extract_feats expr in
        let depth_acc = depth_acc + StringMap.find_def0 "depth" feats in
        let feats_acc =
          StringMap.union
            (fun key v1 v2 ->
              match key with
              | "depth" -> Some (Int.max v1 v2) (* actually max_depth *)
              | _ -> Some (v1 + v2) )
            feats feats_acc
        in
        (feats_acc, depth_acc) )
      (StringMap.empty, 0) assertions
  in
  let nb_exprs = List.length assertions in
  StringMap.add "nb_queries" nb_exprs
  @@ StringMap.add "time" (Int64.to_int runtime)
  @@ StringMap.add "mean_depth" (depth_acc / nb_exprs)
  @@ StringMap.add "max_depth"
       ( match StringMap.find_opt "depth" feats with
       | None -> assert false
       | Some v -> v )
       (StringMap.remove "depth" feats)

let cmd directory output_csv =
  let entries = read_marshalled_file (Fpath.to_string directory) in
  let res : ((unit, [> Rresult.R.msg ]) result, [> Rresult.R.msg ]) result =
    Bos.OS.File.with_oc output_csv
      (fun oc entries ->
        Out_channel.output_string oc
          (String.cat (String.concat "," final_names) "\n");
        List.iter
          (fun (solver_name, exprs, model, t) ->
            if not (List.is_empty exprs) then
              let feats = extract_feats_wtime exprs t in
              let row =
                List.map
                  (fun name ->
                    if String.equal name "solver" then solver_name
                    else if String.equal name "model" then Bool.to_string model
                    else
                      let count = StringMap.find_def0 name feats in
                      string_of_int count )
                  final_names
              in
              let row = String.cat (String.concat "," row) "\n" in
              Out_channel.output_string oc row )
          entries;
        Ok () )
      entries
  in
  match Rresult.R.join res with
  | Error (`Msg m) -> Fmt.failwith "%s" m
  | Ok () -> Fmt.pr "Done writing to %a\n%!" Fpath.pp output_csv

(* let cmd directory output_csv =
  let directory = Fpath.to_string directory in
  let output_csv = Fpath.to_string output_csv in
  let entries = read_marshalled_file directory in
  let oc = open_out output_csv in

  (* en-tête CSV *)
  output_string oc (String.concat "," final_names ^ "\n");

  List.iter
    (fun (solver_name, exprs, model, t) ->
      if not (List.is_empty exprs) then
        let feats = extract_feats_wtime exprs t in
        let row =
          List.map
            (fun name ->
              if String.equal name "solver" then solver_name
              else if String.equal name "model" then Bool.to_string model
              else
                let count = StringMap.find_def0 name feats in
                string_of_int count )
            final_names
        in
        let row = String.concat "," row ^ "\n" in
        (* Fmt.epr "pp row: %s@." row; *)
        output_string oc row )
    entries;

  close_out oc;
  Fmt.pr "Done writing to %s\n%!" output_csv *)

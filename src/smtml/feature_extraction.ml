(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2026 formalsec *)
(* Written by the Smtml programmers *)

open Smtml_prelude.Result.Syntax
open Feature_map

(* initialize feature map with all zeros *)

let extract_feats_aux : Expr.t -> t =
  let rec visit depth (feats : t) (e : Expr.t) =
    let feats = incr_feat (Feature.of_expr_kind e.node (Expr.ty e)) feats in
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
      let feats = incr_feat (Feature.of_ty ty) feats in
      let feats = incr_feat (Feature.of_naryop naryop) feats in
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
      let feats = incr_feat (Feature.of_ty ty) feats in
      let feats = incr_feat (Feature.of_unop unop) feats in
      visit (depth + 1) feats t
    | Cvtop (ty, cvtop, t) ->
      let feats = incr_feat (Feature.of_ty ty) feats in
      let feats = incr_feat (Feature.of_cvtop cvtop) feats in
      visit (depth + 1) feats t
    | Extract (t, _, _) -> visit (depth + 1) feats t
    | Binop (ty, binop, e1, e2) ->
      let feats = incr_feat (Feature.of_ty ty) feats in
      let feats = incr_feat (Feature.of_binop binop) feats in
      let depth1, feats = visit (depth + 1) feats e1 in
      let depth2, feats = visit (depth + 1) feats e2 in
      (Int.max depth1 depth2, feats)
    | Relop (ty, relop, e1, e2) ->
      let feats = incr_feat (Feature.of_ty ty) feats in
      let feats = incr_feat (Feature.of_relop relop) feats in
      let depth1, feats = visit (depth + 1) feats e1 in
      let depth2, feats = visit (depth + 1) feats e2 in
      (Int.max depth1 depth2, feats)
    | Concat (e1, e2) ->
      let depth1, feats = visit (depth + 1) feats e1 in
      let depth2, feats = visit (depth + 1) feats e2 in
      (Int.max depth1 depth2, feats)
    | Triop (ty, triop, e1, e2, e3) ->
      let feats = incr_feat (Feature.of_ty ty) feats in
      let feats = incr_feat (Feature.of_triop triop) feats in
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
    let depth, feats = visit 1 empty expr in
    add_depth depth feats

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

let extract_feats assertions : t =
  let feats, depth_acc =
    List.fold_left
      (fun (feats_acc, depth_acc) expr ->
        let feats = extract_feats_aux expr in
        let depth_acc = depth_acc + get_depth feats in
        let feats_acc = union feats feats_acc in
        (feats_acc, depth_acc) )
      (empty, 0) assertions
  in
  let nb_exprs = List.length assertions in
  add_nb_queries nb_exprs
  @@ add_mean_depth (depth_acc / nb_exprs)
  @@ rename_depth_to_max_depth feats

let extract_feats_wtime assertions runtime =
  add_time (Int64.to_int runtime) (extract_feats assertions)

let cmd marshalled_file output_csv =
  let res =
    read_marshalled_file marshalled_file >>| fun entries ->
    Bos.OS.File.with_oc output_csv
      (fun oc entries ->
        Out_channel.output_string oc
          (String.cat (String.concat "," Feature.all_feature_names) "\n");
        List.iter
          (fun (solver_name, exprs, model, t, _) ->
            if List.compare_lengths exprs [] > 0 then
              let feats = extract_feats_wtime exprs t in
              let row = Feature.feats_to_str solver_name model feats in
              Out_channel.output_string oc row )
          entries;
        Ok () )
      entries
  in
  match Result.join (Result.join res) with
  | Error (`Msg m) -> Fmt.failwith "%s" m
  | Ok () -> Log.debug (fun k -> k "Done writing to %a\n%!" Fpath.pp output_csv)

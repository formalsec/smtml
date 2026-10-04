(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2026 formalsec *)
(* Written by the Smtml programmers *)

open Smtml_prelude.Result.Syntax
open Feature_map

(* Uses hashes to memoize feats and heights and not redo it for expression for
   which it was already done *)
let rec extract_feats_aux memo (feats : t) (e : Expr.t) : int * t =
  match Hashtbl.find_opt memo e.Hc.tag with
  | Some height -> (height, feats)
  | None ->
    let feats = incr_feat (Feature.of_expr_kind e.node (Expr.ty e)) feats in
    let feats, children =
      match Expr.view e with
      | Val _ | Symbol _ -> (feats, [])
      | Ptr { offset; _ } -> (feats, [ offset ])
      | List lst | App (_, lst) -> (feats, lst)
      | Naryop (ty, naryop, lst) ->
        let feats = incr_feat (Feature.of_ty ty) feats in
        (incr_feat (Feature.of_naryop naryop) feats, lst)
      | Unop (ty, unop, t) ->
        let feats = incr_feat (Feature.of_ty ty) feats in
        (incr_feat (Feature.of_unop unop) feats, [ t ])
      | Cvtop (ty, cvtop, t) ->
        let feats = incr_feat (Feature.of_ty ty) feats in
        (incr_feat (Feature.of_cvtop cvtop) feats, [ t ])
      | Extract (t, _, _) -> (feats, [ t ])
      | Binop (ty, binop, e1, e2) ->
        let feats = incr_feat (Feature.of_ty ty) feats in
        (incr_feat (Feature.of_binop binop) feats, [ e1; e2 ])
      | Relop (ty, relop, e1, e2) ->
        let feats = incr_feat (Feature.of_ty ty) feats in
        (incr_feat (Feature.of_relop relop) feats, [ e1; e2 ])
      | Concat (e1, e2) -> (feats, [ e1; e2 ])
      | Triop (ty, triop, e1, e2, e3) ->
        let feats = incr_feat (Feature.of_ty ty) feats in
        (incr_feat (Feature.of_triop triop) feats, [ e1; e2; e3 ])
      | Binder (_, lst, t) -> (feats, t :: lst)
    in
    let height, feats =
      List.fold_left
        (fun (height, feats) child ->
          let child_height, feats = extract_feats_aux memo feats child in
          (Int.max height child_height, feats) )
        (0, feats) children
    in
    let height = height + 1 in
    Hashtbl.add memo e.Hc.tag height;
    (height, feats)

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
  (* Memoization table for the heights and the features of visited expressions
   *)
  let memo = Hashtbl.create 64 in
  let feats, max_depth, depth_acc =
    List.fold_left
      (fun (feats, max_depth, depth_acc) expr ->
        let depth, feats = extract_feats_aux memo feats expr in
        (feats, Int.max max_depth depth, depth_acc + depth) )
      (empty, 0, 0) assertions
  in
  let nb_exprs = List.length assertions in
  add_nb_queries nb_exprs
  @@ add_mean_depth (depth_acc / nb_exprs)
  @@ rename_depth_to_max_depth (add_depth max_depth feats)

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

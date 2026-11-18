(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2024 formalsec *)
(* Written by the Smtml programmers *)

module ExprTbl = Hashtbl.Make (Expr)
module SymMap = Map.Make (Symbol)

module ExprUnionFind = struct
  type t =
    { parents : Expr.t ExprTbl.t
    ; ranks : int ExprTbl.t
    }

  let create () = { parents = ExprTbl.create 16; ranks = ExprTbl.create 16 }

  let rec find (tbl : t) (e : Expr.t) : Expr.t =
    match ExprTbl.find_opt tbl.parents e with
    | Some e' when Expr.equal e e' -> e
    | Some e' ->
      let root = find tbl e' in
      ExprTbl.replace tbl.parents e root;
      root
    | None ->
      ExprTbl.replace tbl.parents e e;
      ExprTbl.replace tbl.ranks e 0;
      e

  let union (tbl : t) (e1 : Expr.t) (e2 : Expr.t) : unit =
    let root1 = find tbl e1 in
    let root2 = find tbl e2 in
    if not (Expr.equal root1 root2) then
      let r1 = ExprTbl.find_opt tbl.ranks root1 |> Option.value ~default:0 in
      let r2 = ExprTbl.find_opt tbl.ranks root2 |> Option.value ~default:0 in
      if r1 < r2 then ExprTbl.replace tbl.parents root1 root2
      else if r1 > r2 then ExprTbl.replace tbl.parents root2 root1
      else begin
        ExprTbl.replace tbl.parents root2 root1;
        ExprTbl.replace tbl.ranks root1 (r1 + 1)
      end
end

(* -------------------------------------------------------------------------------- *)

let get_expr_symbol_map (xs : (Expr.t * Symbol.t list) list) :
  Expr.t list SymMap.t =
  let update_f e = function None -> Some [ e ] | Some es -> Some (e :: es) in
  List.fold_left
    (fun map (e, xs) ->
      List.fold_left (fun map' x -> SymMap.update x (update_f e) map') map xs )
    SymMap.empty xs

let merge_expr_by_index (es_tbl : ExprUnionFind.t)
  (es_idx : Expr.t list SymMap.t) : unit =
  SymMap.iter
    (fun _ -> function
      | [] -> ()
      | e :: [] -> ExprUnionFind.find es_tbl e |> fun _ -> ()
      | e :: es -> List.iter (ExprUnionFind.union es_tbl e) es )
    es_idx

let extract_expr_groups (es_tbl : ExprUnionFind.t) : Expr.Set.t list =
  let groups = ExprTbl.create (ExprTbl.length es_tbl.parents) in
  ExprTbl.iter
    (fun e _ ->
      Log.debug (fun k -> k "%a" Expr.pp e);
      let root = ExprUnionFind.find es_tbl e in
      let members = ExprTbl.find_opt groups root in
      let members' = Option.value ~default:Expr.Set.empty members in
      ExprTbl.replace groups root (Expr.Set.add e members') )
    es_tbl.parents;
  ExprTbl.to_seq_values groups |> List.of_seq

let slice (es : Expr.Set.t) : Expr.Set.t list =
  let split_f (_, l) = not (List.is_empty l) in
  let es_ands = Expr.Set.to_list es in
  let es_symbols = List.map (fun e -> (e, Expr.get_symbols [ e ])) es_ands in
  let es_symbols', es_rest = List.partition split_f es_symbols in
  let es_rest' = List.map (fun (e, _) -> Expr.Set.singleton e) es_rest in
  let es_index = get_expr_symbol_map es_symbols' in
  let es_groups = ExprUnionFind.create () in
  merge_expr_by_index es_groups es_index;
  extract_expr_groups es_groups @ es_rest'

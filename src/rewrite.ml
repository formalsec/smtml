(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2024 formalsec *)
(* Written by the Smtml programmers *)

(** Module that performs two 'important' rewritings:

    1. Replace symbols' [Ty_none] with the correct type specified in
    {e declare-const}.

    2. Propagate the correct theory encoding for [Unop], [Binop], [Relop], and
    [Triop].

    3. Inlines [Let_in] binders into a single big expr *)

module Symb_map = Map.Make (Symbol)

(* TODO: Add proper logs *)
let debug = false

let debug fmt k = if debug then k (Fmt.epr fmt)

(* FIXME: This is a very basic way to infer types. I'm surprised it even works *)
let rewrite_ty unknown_ty tys =
  debug "  rewrite_ty: %a@." (fun k -> k Ty.pp unknown_ty);
  match (unknown_ty, tys) with
  | Ty.Ty_none, [ ty ] -> ty
  | Ty.Ty_none, [ ty1; ty2 ] ->
    debug "  rewrite_ty: %a %a@." (fun k -> k Ty.pp ty1 Ty.pp ty2);
    assert (Ty.equal ty1 ty2);
    ty1
  | Ty.Ty_none, _ -> assert false
  | ty, _ -> ty

(** Propagates types in [type_map] and inlines [Let_in] binders *)
let rec rewrite_expr (type_map, expr_map) hte =
  debug "rewrite_expr: %a@." (fun k -> k Expr.pp hte);
  match Expr.view hte with
  | Val _ -> hte
  | Ptr { base; offset } ->
    Expr.ptr base (rewrite_expr (type_map, expr_map) offset)
  | Symbol sym -> (
    match Symb_map.find_opt sym type_map with
    | None -> (
      match Symb_map.find_opt sym expr_map with
      | None -> Fmt.failwith "Undefined symbol: %a" Symbol.pp sym
      | Some expr -> expr )
    | Some ty -> Expr.symbol { sym with ty } )
  | List htes ->
    Expr.make (List (List.map (rewrite_expr (type_map, expr_map)) htes))
  | App (op, htes) ->
    Expr.make (App (op, List.map (rewrite_expr (type_map, expr_map)) htes))
  | Unop (ty, op, hte) ->
    let hte = rewrite_expr (type_map, expr_map) hte in
    let ty = rewrite_ty ty [ Expr.ty hte ] in
    Expr.unop ty op hte
  | Binop (ty, op, hte1, hte2) ->
    let hte1 = rewrite_expr (type_map, expr_map) hte1 in
    let hte2 = rewrite_expr (type_map, expr_map) hte2 in
    let ty = rewrite_ty ty [ Expr.ty hte1; Expr.ty hte2 ] in
    Expr.binop ty op hte1 hte2
  | Triop (ty, op, hte1, hte2, hte3) ->
    let hte1 = rewrite_expr (type_map, expr_map) hte1 in
    let hte2 = rewrite_expr (type_map, expr_map) hte2 in
    let hte3 = rewrite_expr (type_map, expr_map) hte3 in
    Expr.triop ty op hte1 hte2 hte3
  | Relop (ty, ((Eq | Ne) as op), hte1, hte2) ->
    let hte1 = rewrite_expr (type_map, expr_map) hte1 in
    let hte2 = rewrite_expr (type_map, expr_map) hte2 in
    Expr.relop ty op hte1 hte2
  | Relop (ty, op, hte1, hte2) ->
    let hte1 = rewrite_expr (type_map, expr_map) hte1 in
    let hte2 = rewrite_expr (type_map, expr_map) hte2 in
    let ty = rewrite_ty ty [ Expr.ty hte1; Expr.ty hte2 ] in
    Expr.relop ty op hte1 hte2
  | Cvtop (ty, op, hte) ->
    let hte = rewrite_expr (type_map, expr_map) hte in
    let ty = rewrite_ty ty [ Expr.ty hte ] in
    Expr.cvtop ty op hte
  | Naryop (ty, op, htes) ->
    let htes = List.map (rewrite_expr (type_map, expr_map)) htes in
    Expr.make (Naryop (ty, op, htes))
  | Extract (hte, h, l) ->
    let hte = rewrite_expr (type_map, expr_map) hte in
    Expr.extract hte ~high:h ~low:l
  | Concat (hte1, hte2) ->
    let hte1 = rewrite_expr (type_map, expr_map) hte1 in
    let hte2 = rewrite_expr (type_map, expr_map) hte2 in
    Expr.concat hte1 hte2
  | Binder (Let_in, vars, e) ->
    (* Then, we rewrite the types of the expr *)
    let expr_map =
      List.fold_left
        (fun map e ->
          match Expr.view e with
          | App (sym, [ e ]) ->
            (* Searches the outer expr_map. Because I don't think the list of
               var bindings are in scope for themselves? *)
            let e = rewrite_expr (type_map, expr_map) e in
            Symb_map.add sym e map
          | _ -> assert false )
        expr_map vars
    in
    rewrite_expr (type_map, expr_map) e
  | Binder (_, _, _) -> assert false

(** Acccumulates types of symbols in [type_map] and calls rewrite_expr *)
let rewrite_cmd type_map cmd =
  debug " rewrite_cmd: %a@." (fun k -> k Ast.pp cmd);
  match cmd with
  | Ast.Assert hte ->
    let hte = rewrite_expr (type_map, Symb_map.empty) hte in
    (type_map, Ast.Assert hte)
  | Check_sat htes ->
    let htes = List.map (rewrite_expr (type_map, Symb_map.empty)) htes in
    (type_map, Check_sat htes)
  | Declare_const { id; sort } as cmd -> (Symb_map.add id sort.ty type_map, cmd)
  | Get_value htes ->
    let htes = List.map (rewrite_expr (type_map, Symb_map.empty)) htes in
    (type_map, Get_value htes)
  | cmd -> (type_map, cmd)

let rewrite script =
  let _, cmds =
    List.fold_left
      (fun (type_map, cmds) cmd ->
        let type_map, new_cmd = rewrite_cmd type_map cmd in
        debug "     new_cmd: %a@." (fun k -> k Ast.pp new_cmd);
        (type_map, new_cmd :: cmds) )
      (Symb_map.empty, []) script
  in
  List.rev cmds

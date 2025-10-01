(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2024 formalsec *)
(* Written by the Smtml programmers *)

type t = expr Hc.hash_consed

and expr =
  | Val of Value.t
  | Ptr of
      { base : Bitvector.t
      ; offset : t
      }
  | Loc of Loc.t
  | Symbol of Symbol.t
  | List of t list
  | App of Symbol.t * t list
  | Unop of Ty.t * Ty.Unop.t * t
  | Binop of Ty.t * Ty.Binop.t * t * t
  | Triop of Ty.t * Ty.Triop.t * t * t * t
  | Relop of Ty.t * Ty.Relop.t * t * t
  | Cvtop of Ty.t * Ty.Cvtop.t * t
  | Naryop of Ty.t * Ty.Naryop.t * t list
  | Extract of t * int * int
  | Concat of t * t
  | Binder of Binder.t * t list * t

module Expr = struct
  type t = expr

  let list_eq (l1 : 'a list) (l2 : 'a list) : bool =
    if List.compare_lengths l1 l2 = 0 then List.for_all2 phys_equal l1 l2
    else false

  let equal (e1 : expr) (e2 : expr) : bool =
    match (e1, e2) with
    | Val v1, Val v2 -> Value.equal v1 v2
    | Loc a, Loc b -> Loc.compare a b = 0
    | Ptr { base = b1; offset = o1 }, Ptr { base = b2; offset = o2 } ->
      Bitvector.equal b1 b2 && phys_equal o1 o2
    | Symbol s1, Symbol s2 -> Symbol.equal s1 s2
    | List l1, List l2 -> list_eq l1 l2
    | App (s1, l1), App (s2, l2) -> Symbol.equal s1 s2 && list_eq l1 l2
    | Unop (t1, op1, e1), Unop (t2, op2, e2) ->
      Ty.equal t1 t2 && Ty.Unop.equal op1 op2 && phys_equal e1 e2
    | Binop (t1, op1, e1, e3), Binop (t2, op2, e2, e4) ->
      Ty.equal t1 t2 && Ty.Binop.equal op1 op2 && phys_equal e1 e2
      && phys_equal e3 e4
    | Relop (t1, op1, e1, e3), Relop (t2, op2, e2, e4) ->
      Ty.equal t1 t2 && Ty.Relop.equal op1 op2 && phys_equal e1 e2
      && phys_equal e3 e4
    | Triop (t1, op1, e1, e3, e5), Triop (t2, op2, e2, e4, e6) ->
      Ty.equal t1 t2 && Ty.Triop.equal op1 op2 && phys_equal e1 e2
      && phys_equal e3 e4 && phys_equal e5 e6
    | Cvtop (t1, op1, e1), Cvtop (t2, op2, e2) ->
      Ty.equal t1 t2 && Ty.Cvtop.equal op1 op2 && phys_equal e1 e2
    | Naryop (t1, op1, l1), Naryop (t2, op2, l2) ->
      Ty.equal t1 t2 && Ty.Naryop.equal op1 op2 && list_eq l1 l2
    | Extract (e1, h1, l1), Extract (e2, h2, l2) ->
      phys_equal e1 e2 && h1 = h2 && l1 = l2
    | Concat (e1, e3), Concat (e2, e4) -> phys_equal e1 e2 && phys_equal e3 e4
    | Binder (binder1, vars1, e1), Binder (binder2, vars2, e2) ->
      Binder.equal binder1 binder2 && list_eq vars1 vars2 && phys_equal e1 e2
    | ( ( Val _ | Ptr _ | Loc _ | Symbol _ | List _ | App _ | Unop _ | Binop _
        | Triop _ | Relop _ | Cvtop _ | Naryop _ | Extract _ | Concat _
        | Binder _ )
      , _ ) ->
      false

  let hash (e : expr) : int =
    let h x = Hashtbl.hash x in
    match e with
    | Val v -> h v
    | Ptr { base; offset } -> h (base, offset.tag)
    | Loc l -> h l
    | Symbol s -> h s
    | List v -> h v
    | App (x, es) -> h (x, es)
    | Unop (ty, op, e) -> h (ty, op, e.tag)
    | Cvtop (ty, op, e) -> h (ty, op, e.tag)
    | Binop (ty, op, e1, e2) -> h (ty, op, e1.tag, e2.tag)
    | Relop (ty, op, e1, e2) -> h (ty, op, e1.tag, e2.tag)
    | Triop (ty, op, e1, e2, e3) -> h (ty, op, e1.tag, e2.tag, e3.tag)
    | Naryop (ty, op, es) -> h (ty, op, es)
    | Extract (e, hi, lo) -> h (e.tag, hi, lo)
    | Concat (e1, e2) -> h (e1.tag, e2.tag)
    | Binder (b, vars, e) -> h (b, vars, e.tag)
end

module Hc = Hc.Make [@inlined hint] (Expr)

let equal (hte1 : t) (hte2 : t) = phys_equal hte1 hte2 [@@inline]

let hash (hte : t) = hte.tag [@@inline]

module Key = struct
  type nonrec t = t

  let to_int hte = hash hte
end

let[@inline] make e = Hc.hashcons e

let[@inline] view (hte : t) = hte.node

let[@inline] compare (hte1 : t) (hte2 : t) = compare hte1.tag hte2.tag

let value (v : Value.t) : t = make (Val v) [@@inline]

let list l = make (List l)

let app symbol args = make (App (symbol, args))

let raw_unop ty op hte = make (Unop (ty, op, hte)) [@@inline]

let raw_binop ty op hte1 hte2 = make (Binop (ty, op, hte1, hte2)) [@@inline]

let raw_triop ty op e1 e2 e3 = make (Triop (ty, op, e1, e2, e3)) [@@inline]

let raw_naryop ty op es = make (Naryop (ty, op, es)) [@@inline]

let raw_relop ty op hte1 hte2 = make (Relop (ty, op, hte1, hte2)) [@@inline]


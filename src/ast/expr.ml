(***************************************************************************)
(* This file is part of the third-party OCaml library `smtml`.             *)
(* Copyright (C) 2023-2024 formalsec                                       *)
(*                                                                         *)
(* This program is free software: you can redistribute it and/or modify    *)
(* it under the terms of the GNU General Public License as published by    *)
(* the Free Software Foundation, either version 3 of the License, or       *)
(* (at your option) any later version.                                     *)
(*                                                                         *)
(* This program is distributed in the hope that it will be useful,         *)
(* but WITHOUT ANY WARRANTY; without even the implied warranty of          *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the           *)
(* GNU General Public License for more details.                            *)
(*                                                                         *)
(* You should have received a copy of the GNU General Public License       *)
(* along with this program.  If not, see <https://www.gnu.org/licenses/>.  *)
(***************************************************************************)

open Ty

type t = expr Hc.hash_consed

and expr =
  | Val of Value.t
  | Ptr of
      { base : int32
      ; offset : t
      }
  | Symbol of Symbol.t
  | List of t list
  | App of Symbol.t * t list
  | Unop of Ty.t * unop * t
  | Binop of Ty.t * binop * t * t
  | Triop of Ty.t * triop * t * t * t
  | Relop of Ty.t * relop * t * t
  | Cvtop of Ty.t * cvtop * t
  | Naryop of Ty.t * naryop * t list
  | Extract of t * int * int
  | Concat of t * t

module Expr = struct
  type t = expr

  let list_eq (l1 : 'a list) (l2 : 'a list) : bool =
    if List.compare_lengths l1 l2 = 0 then List.for_all2 phys_equal l1 l2
    else false

  let equal (e1 : expr) (e2 : expr) : bool =
    match (e1, e2) with
    | Val v1, Val v2 -> Value.equal v1 v2
    | Ptr { base = b1; offset = o1 }, Ptr { base = b2; offset = o2 } ->
      Int32.equal b1 b2 && phys_equal o1 o2
    | Symbol s1, Symbol s2 -> Symbol.equal s1 s2
    | List l1, List l2 -> list_eq l1 l2
    | App (s1, l1), App (s2, l2) -> Symbol.equal s1 s2 && list_eq l1 l2
    | Unop (t1, op1, e1), Unop (t2, op2, e2) ->
      Ty.equal t1 t2 && Ty.unop_equal op1 op2 && phys_equal e1 e2
    | Binop (t1, op1, e1, e3), Binop (t2, op2, e2, e4) ->
      Ty.equal t1 t2 && Ty.binop_equal op1 op2 && phys_equal e1 e2
      && phys_equal e3 e4
    | Relop (t1, op1, e1, e3), Relop (t2, op2, e2, e4) ->
      Ty.equal t1 t2 && Ty.relop_equal op1 op2 && phys_equal e1 e2
      && phys_equal e3 e4
    | Triop (t1, op1, e1, e3, e5), Triop (t2, op2, e2, e4, e6) ->
      Ty.equal t1 t2 && Ty.triop_equal op1 op2 && phys_equal e1 e2
      && phys_equal e3 e4 && phys_equal e5 e6
    | Cvtop (t1, op1, e1), Cvtop (t2, op2, e2) ->
      Ty.equal t1 t2 && Ty.cvtop_equal op1 op2 && phys_equal e1 e2
    | Naryop (t1, op1, l1), Naryop (t2, op2, l2) ->
      Ty.equal t1 t2 && Ty.naryop_equal op1 op2 && list_eq l1 l2
    | Extract (e1, h1, l1), Extract (e2, h2, l2) ->
      phys_equal e1 e2 && h1 = h2 && l1 = l2
    | Concat (e1, e3), Concat (e2, e4) -> phys_equal e1 e2 && phys_equal e3 e4
    | ( ( Val _ | Ptr _ | Symbol _ | List _ | App _ | Unop _ | Binop _ | Triop _
        | Relop _ | Cvtop _ | Naryop _ | Extract _ | Concat _ )
      , _ ) ->
      false

  let hash (e : expr) : int =
    let h x = Hashtbl.hash x in
    match e with
    | Val v -> h v
    | Ptr { base; offset } -> h (base, offset.tag)
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
end

module Hc = Hc.Make [@inlined hint] (Expr)

let equal (hte1 : t) (hte2 : t) = Int.equal hte1.tag hte2.tag [@@inline]

let hash (hte : t) = hte.tag [@@inline]

module Key = struct
  type nonrec t = t

  let to_int hte = hash hte
end

module Set = PatriciaTree.MakeHashconsedSet (Key) ()

let make (e : expr) = Hc.hashcons e [@@inline]

let ( @: ) e _ = make e

let view (hte : t) : expr = hte.node [@@inline]

let symbol s = make (Symbol s)

let mk_symbol s = make (Symbol s)

let is_num (e : t) = match view e with Val (Num _) -> true | _ -> false

let rec ty (hte : t) : Ty.t =
  match view hte with
  | Val x -> Value.type_of x
  | Ptr _ -> Ty_bitv 32
  | Symbol x -> Symbol.type_of x
  | List _ -> Ty_list
  | App _ -> Ty_app
  | Unop (ty, _, _) -> ty
  | Binop (ty, _, _, _) -> ty
  | Triop (ty, _, _, _, _) -> ty
  | Relop (ty, _, _, _) -> ty
  | Cvtop (ty, _, _) -> ty
  | Naryop (ty, _, _) -> ty
  | Extract (_, h, l) -> Ty_bitv ((h - l) * 8)
  | Concat (e1, e2) -> (
    match (ty e1, ty e2) with
    | Ty_bitv n1, Ty_bitv n2 -> Ty_bitv (n1 + n2)
    | t1, t2 ->
      Fmt.failwith "Invalid concat of (%a) with (%a)" Ty.pp t1 Ty.pp t2 )

let rec is_symbolic (v : t) : bool =
  match view v with
  | Val _ -> false
  | Symbol _ -> true
  | Ptr { offset; _ } -> is_symbolic offset
  | List vs -> List.exists is_symbolic vs
  | App (_, vs) -> List.exists is_symbolic vs
  | Unop (_, _, v) -> is_symbolic v
  | Binop (_, _, v1, v2) -> is_symbolic v1 || is_symbolic v2
  | Triop (_, _, v1, v2, v3) ->
    is_symbolic v1 || is_symbolic v2 || is_symbolic v3
  | Cvtop (_, _, v) -> is_symbolic v
  | Relop (_, _, v1, v2) -> is_symbolic v1 || is_symbolic v2
  | Naryop (_, _, vs) -> List.exists is_symbolic vs
  | Extract (e, _, _) -> is_symbolic e
  | Concat (e1, e2) -> is_symbolic e1 || is_symbolic e2

let get_symbols (hte : t list) =
  let tbl = Hashtbl.create 64 in
  let rec symbols (hte : t) =
    match view hte with
    | Val _ -> ()
    | Ptr { offset; _ } -> symbols offset
    | Symbol s -> Hashtbl.replace tbl s ()
    | List es -> List.iter symbols es
    | App (_, es) -> List.iter symbols es
    | Unop (_, _, e1) -> symbols e1
    | Binop (_, _, e1, e2) ->
      symbols e1;
      symbols e2
    | Triop (_, _, e1, e2, e3) ->
      symbols e1;
      symbols e2;
      symbols e3
    | Relop (_, _, e1, e2) ->
      symbols e1;
      symbols e2
    | Cvtop (_, _, e) -> symbols e
    | Naryop (_, _, es) -> List.iter symbols es
    | Extract (e, _, _) -> symbols e
    | Concat (e1, e2) ->
      symbols e1;
      symbols e2
  in
  List.iter symbols hte;
  Hashtbl.fold (fun k () acc -> k :: acc) tbl []

let negate_relop (hte : t) : (t, string) Result.t =
  let e =
    match view hte with
    | Relop (ty, Eq, e1, e2) -> Ok (Relop (ty, Ne, e1, e2))
    | Relop (ty, Ne, e1, e2) -> Ok (Relop (ty, Eq, e1, e2))
    | Relop (ty, Lt, e1, e2) -> Ok (Relop (ty, Ge, e1, e2))
    | Relop (ty, LtU, e1, e2) -> Ok (Relop (ty, GeU, e1, e2))
    | Relop (ty, Le, e1, e2) -> Ok (Relop (ty, Gt, e1, e2))
    | Relop (ty, LeU, e1, e2) -> Ok (Relop (ty, GtU, e1, e2))
    | Relop (ty, Gt, e1, e2) -> Ok (Relop (ty, Le, e1, e2))
    | Relop (ty, GtU, e1, e2) -> Ok (Relop (ty, LeU, e1, e2))
    | Relop (ty, Ge, e1, e2) -> Ok (Relop (ty, Lt, e1, e2))
    | Relop (ty, GeU, e1, e2) -> Ok (Relop (ty, LtU, e1, e2))
    | _ -> Error "negate_relop: not a relop."
  in
  Result.map make e

module Pp = struct
  let rec pp fmt (hte : t) =
    match view hte with
    | Val v -> Value.pp fmt v
    | Ptr { base; offset } -> Fmt.pf fmt "(Ptr (i32 %ld) %a)" base pp offset
    | Symbol s -> Symbol.pp fmt s
    | List v -> Fmt.pf fmt "@[<hov 1>[%a]@]" (Fmt.list ~sep:Fmt.comma pp) v
    | App (s, v) ->
      Fmt.pf fmt "@[<hov 1>(%a@ %a)@]" Symbol.pp s
        (Fmt.list ~sep:Fmt.comma pp)
        v
    | Unop (ty, op, e) ->
      Fmt.pf fmt "@[<hov 1>(%a.%a@ %a)@]" Ty.pp ty pp_unop op pp e
    | Binop (ty, op, e1, e2) ->
      Fmt.pf fmt "@[<hov 1>(%a.%a@ %a@ %a)@]" Ty.pp ty pp_binop op pp e1 pp e2
    | Triop (ty, op, e1, e2, e3) ->
      Fmt.pf fmt "@[<hov 1>(%a.%a@ %a@ %a@ %a)@]" Ty.pp ty pp_triop op pp e1 pp
        e2 pp e3
    | Relop (ty, op, e1, e2) ->
      Fmt.pf fmt "@[<hov 1>(%a.%a@ %a@ %a)@]" Ty.pp ty pp_relop op pp e1 pp e2
    | Cvtop (ty, op, e) ->
      Fmt.pf fmt "@[<hov 1>(%a.%a@ %a)@]" Ty.pp ty pp_cvtop op pp e
    | Naryop (ty, op, es) ->
      Fmt.pf fmt "@[<hov 1>(%a.%a@ (%a))@]" Ty.pp ty pp_naryop op
        (Fmt.list ~sep:Fmt.comma pp)
        es
    | Extract (e, h, l) ->
      Fmt.pf fmt "@[<hov 1>(extract@ %a@ %d@ %d)@]" pp e l h
    | Concat (e1, e2) -> Fmt.pf fmt "@[<hov 1>(++@ %a@ %a)@]" pp e1 pp e2

  let pp_list fmt (es : t list) = Fmt.hovbox (Fmt.list ~sep:Fmt.comma pp) fmt es

  let pp_smt fmt (es : t list) : unit =
    let pp_symbols fmt syms =
      Fmt.list ~sep:Fmt.semi
        (fun fmt sym ->
          let t = Symbol.type_of sym in
          Fmt.pf fmt "(let-const %a %a)" Symbol.pp sym Ty.pp t )
        fmt syms
    in
    let pp_asserts fmt es =
      Fmt.list ~sep:Fmt.semi
        (fun fmt e -> Fmt.pf fmt "(assert @[<h 2>%a@])" pp e)
        fmt es
    in
    let syms = get_symbols es in
    if List.length syms > 0 then Fmt.pf fmt "%a@\n" pp_symbols syms;
    if List.length es > 0 then Fmt.pf fmt "%a@\n" pp_asserts es;
    Fmt.string fmt "(check-sat)"
end

let pp = Pp.pp

let pp_list = Pp.pp_list

let pp_smt = Pp.pp_smt

let to_string e = Fmt.str "%a" pp e

let value (v : Value.t) : t = make (Val v) [@@inline]

let ptr base offset = make (Ptr { base; offset })

let app symbol args = make (App (symbol, args))

let unop' (ty : Ty.t) (op : unop) (hte : t) : t = make (Unop (ty, op, hte))
[@@inline]

let unop (ty : Ty.t) (op : unop) (hte : t) : t =
  match (op, view hte) with
  | _, Val v -> value (Eval.unop ty op v)
  | Not, Unop (_, Not, hte') -> hte'
  | Neg, Unop (_, Neg, hte') -> hte'
  | Trim, Cvtop (Ty_real, ToString, _) -> hte
  | Head, List (hd :: _) -> hd
  | Tail, List (_ :: tl) -> make (List tl)
  | Reverse, List es -> make (List (List.rev es))
  | Length, List es -> value (Int (List.length es))
  | _ -> unop' ty op hte

let binop' (ty : Ty.t) (op : binop) (hte1 : t) (hte2 : t) : t =
  make (Binop (ty, op, hte1, hte2))
[@@inline]

let rec binop ty (op : binop) (hte1 : t) (hte2 : t) : t =
  match (op, view hte1, view hte2) with
  | op, Val v1, Val v2 -> value (Eval.binop ty op v1 v2)
  | Sub, Ptr { base = b1; offset = os1 }, Ptr { base = b2; offset = os2 } ->
    if Int32.equal b1 b2 then binop ty Sub os1 os2 else binop' ty op hte1 hte2
  | Add, Ptr { base; offset }, _ ->
    ptr base (binop (Ty_bitv 32) Add offset hte2)
  | Sub, Ptr { base; offset }, _ ->
    ptr base (binop (Ty_bitv 32) Sub offset hte2)
  | Rem, Ptr { base; offset }, _ ->
    let rhs = value (Num (I32 base)) in
    let addr = binop (Ty_bitv 32) Add rhs offset in
    binop ty Rem addr hte2
  | Add, _, Ptr { base; offset } ->
    ptr base (binop (Ty_bitv 32) Add offset hte1)
  | Sub, _, Ptr { base; offset } ->
    binop ty Sub hte1 (binop (Ty_bitv 32) Add (value (Num (I32 base))) offset)
  | (Add | Or), Val (Num (I32 0l)), _ -> hte2
  | (And | Div | DivU | Mul | Rem | RemU), Val (Num (I32 0l)), _ -> hte1
  | (Add | Or), _, Val (Num (I32 0l)) -> hte1
  | (And | Mul), _, Val (Num (I32 0l)) -> hte2
  | Add, Binop (ty, Add, x, { node = Val v1; _ }), Val v2 ->
    let v = value (Eval.binop ty Add v1 v2) in
    binop' ty Add x v
  | Sub, Binop (ty, Sub, x, { node = Val v1; _ }), Val v2 ->
    let v = value (Eval.binop ty Add v1 v2) in
    binop' ty Sub x v
  | Mul, Binop (ty, Mul, x, { node = Val v1; _ }), Val v2 ->
    let v = value (Eval.binop ty Mul v1 v2) in
    binop' ty Mul x v
  | Add, Val v1, Binop (ty, Add, x, { node = Val v2; _ }) ->
    let v = value (Eval.binop ty Add v1 v2) in
    binop' ty Add v x
  | Mul, Val v1, Binop (ty, Mul, x, { node = Val v2; _ }) ->
    let v = value (Eval.binop ty Mul v1 v2) in
    binop' ty Mul v x
  | At, List es, Val (Int n) -> List.nth es n
  | List_append_last, List es, _ -> make (List (es @ [ hte2 ]))
  | List_append, List es, _ -> make (List (hte2 :: es))
  | _ -> binop' ty op hte1 hte2

let triop' (ty : Ty.t) (op : triop) (e1 : t) (e2 : t) (e3 : t) : t =
  make (Triop (ty, op, e1, e2, e3))
[@@inline]

let triop ty (op : triop) (e1 : t) (e2 : t) (e3 : t) : t =
  match (op, view e1, view e2, view e3) with
  | Ite, Val True, _, _ -> e2
  | Ite, Val False, _, _ -> e3
  | op, Val v1, Val v2, Val v3 -> value (Eval.triop ty op v1 v2 v3)
  | _ -> triop' ty op e1 e2 e3

let relop' (ty : Ty.t) (op : relop) (hte1 : t) (hte2 : t) : t =
  make (Relop (ty, op, hte1, hte2))
[@@inline]

let rec relop ty (op : relop) (hte1 : t) (hte2 : t) : t =
  match (op, view hte1, view hte2) with
  | op, Val v1, Val v2 -> value (if Eval.relop ty op v1 v2 then True else False)
  | Ne, Val (Real v), _ | Ne, _, Val (Real v) ->
    if Float.is_nan v || Float.is_infinite v then value True
    else relop' ty op hte1 hte2
  | _, Val (Real v), _ | _, _, Val (Real v) ->
    if Float.is_nan v || Float.is_infinite v then value False
    else relop' ty op hte1 hte2
  | Eq, _, Val Nothing | Eq, Val Nothing, _ -> value False
  | Ne, _, Val Nothing | Ne, Val Nothing, _ -> value True
  | Eq, _, Val (App (`Op "symbol", [ Str _ ]))
  | Eq, Val (App (`Op "symbol", [ Str _ ])), _ ->
    value False
  | Ne, _, Val (App (`Op "symbol", [ Str _ ]))
  | Ne, Val (App (`Op "symbol", [ Str _ ])), _ ->
    value True
  | Eq, Ptr { base = b1; offset = os1 }, Ptr { base = b2; offset = os2 } ->
    if Int32.equal b1 b2 then relop Ty_bool Eq os1 os2 else value False
  | Ne, Ptr { base = b1; offset = os1 }, Ptr { base = b2; offset = os2 } ->
    if Int32.equal b1 b2 then relop Ty_bool Ne os1 os2 else value True
  | ( (LtU | LeU | GtU | GeU)
    , Ptr { base = b1; offset = os1 }
    , Ptr { base = b2; offset = os2 } ) ->
    if Int32.equal b1 b2 then relop ty op os1 os2
    else
      value
        (if Eval.relop ty op (Num (I32 b1)) (Num (I32 b2)) then True else False)
  | op, Val (Num _ as n), Ptr { base; offset = { node = Val (Num _ as o); _ } }
    ->
    let base = Eval.binop (Ty_bitv 32) Add (Num (I32 base)) o in
    value (if Eval.relop ty op n base then True else False)
  | op, Ptr { base; offset = { node = Val (Num _ as o); _ } }, Val (Num _ as n)
    ->
    let base = Eval.binop (Ty_bitv 32) Add (Num (I32 base)) o in
    value (if Eval.relop ty op base n then True else False)
  | op, List l1, List l2 -> relop_list op l1 l2
  | _, _, _ -> relop' ty op hte1 hte2

and relop_list op l1 l2 =
  match (op, l1, l2) with
  | Eq, [], [] -> value True
  | Eq, _, [] | Eq, [], _ -> value False
  | Eq, l1, l2 ->
    if not (List.compare_lengths l1 l2 = 0) then value False
    else
      List.fold_left2
        (fun acc a b ->
          binop Ty_bool And acc
          @@
          match (ty a, ty b) with
          | Ty_real, Ty_real -> relop Ty_real Eq a b
          | _ -> relop Ty_bool Eq a b )
        (value True) l1 l2
  | Ne, _, _ -> unop Ty_bool Not @@ relop_list Eq l1 l2
  | (Lt | LtU | Gt | GtU | Le | LeU | Ge | GeU), _, _ -> assert false

let cvtop' (ty : Ty.t) (op : cvtop) (hte : t) : t = make (Cvtop (ty, op, hte))
[@@inline]

let cvtop ty (op : cvtop) (hte : t) : t =
  match (op, view hte) with
  | _, Val v -> value (Eval.cvtop ty op v)
  | String_to_float, Cvtop (Ty_real, ToString, real) -> real
  | _ -> cvtop' ty op hte

let naryop' (ty : Ty.t) (op : naryop) (es : t list) : t =
  make (Naryop (ty, op, es))
[@@inline]

let naryop (ty : Ty.t) (op : naryop) (es : t list) : t =
  if List.for_all (fun e -> match view e with Val _ -> true | _ -> false) es
  then
    let vs =
      List.map (fun e -> match view e with Val v -> v | _ -> assert false) es
    in
    value (Eval.naryop ty op vs)
  else naryop' ty op es

let nland64 (x : int64) (n : int) =
  let rec loop x' n' acc =
    if n' = 0 then Int64.logand x' acc
    else loop x' (n' - 1) Int64.(logor (shift_left acc 8) 0xffL)
  in
  loop x n 0L

let nland32 (x : int32) (n : int) =
  let rec loop x' n' acc =
    if n' = 0 then Int32.logand x' acc
    else loop x' (n' - 1) Int32.(logor (shift_left acc 8) 0xffl)
  in
  loop x n 0l

let extract' (hte : t) ~(high : int) ~(low : int) : t =
  make (Extract (hte, high, low))
[@@inline]

let extract (hte : t) ~(high : int) ~(low : int) : t =
  match view hte with
  | Val (Num (I64 x)) ->
    let x' = nland64 (Int64.shift_right x (low * 8)) (high - low) in
    value (Num (I64 x'))
  | _ -> if high - low = size (ty hte) then hte else extract' hte ~high ~low

let concat' (msb : t) (lsb : t) : t = make (Concat (msb, lsb)) [@@inline]

let concat (msb : t) (lsb : t) : t =
  match (view msb, view lsb) with
  | ( Extract ({ node = Val (Num (I64 x2)); _ }, h2, l2)
    , Extract ({ node = Val (Num (I64 x1)); _ }, h1, l1) ) ->
    let d1 = h1 - l1 in
    let d2 = h2 - l2 in
    let x1' = nland64 (Int64.shift_right x1 (l1 * 8)) d1 in
    let x2' = nland64 (Int64.shift_right x2 (l2 * 8)) d2 in
    let x = Int64.(logor (shift_left x2' (d1 * 8)) x1') in
    extract' (value (Num (I64 x))) ~high:(d1 + d2) ~low:0
  | ( Extract ({ node = Val (Num (I32 x2)); _ }, h2, l2)
    , Extract ({ node = Val (Num (I32 x1)); _ }, h1, l1) ) ->
    let d1 = h1 - l1 in
    let d2 = h2 - l2 in
    let x1' = nland32 (Int32.shift_right x1 (l1 * 8)) d1 in
    let x2' = nland32 (Int32.shift_right x2 (l2 * 8)) d2 in
    let x = Int32.(logor (shift_left x2' (d1 * 8)) x1') in
    extract' (value (Num (I32 x))) ~high:(d1 + d2) ~low:0
  | Extract (s1, h, m1), Extract (s2, m2, l) when equal s1 s2 && m1 = m2 ->
    extract' s1 ~high:h ~low:l
  | ( Extract ({ node = Val (Num (I64 x2)); _ }, h2, l2)
    , Concat
        ({ node = Extract ({ node = Val (Num (I64 x1)); _ }, h1, l1); _ }, se) )
    when not (is_num se) ->
    let d1 = h1 - l1 in
    let d2 = h2 - l2 in
    let x1' = nland64 (Int64.shift_right x1 (l1 * 8)) d1 in
    let x2' = nland64 (Int64.shift_right x2 (l2 * 8)) d2 in
    let x = Int64.(logor (shift_left x2' (d1 * 8)) x1') in
    concat' (extract' (value (Num (I64 x))) ~high:(d1 + d2) ~low:0) se
  | _ -> concat' msb lsb

let rec simplify_expr ?(rm_extract = true) (hte : t) : t =
  match view hte with
  | Val _ | Symbol _ -> hte
  | Ptr { base; offset } -> ptr base (simplify_expr offset)
  | List es -> make @@ List (List.map simplify_expr es)
  | App (x, es) -> make @@ App (x, List.map simplify_expr es)
  | Unop (ty, op, e) ->
    let e = simplify_expr e in
    unop ty op e
  | Binop (ty, op, e1, e2) ->
    let e1 = simplify_expr e1 in
    let e2 = simplify_expr e2 in
    binop ty op e1 e2
  | Relop (ty, op, e1, e2) ->
    let e1 = simplify_expr e1 in
    let e2 = simplify_expr e2 in
    relop ty op e1 e2
  | Triop (ty, op, c, e1, e2) ->
    let c = simplify_expr c in
    let e1 = simplify_expr e1 in
    let e2 = simplify_expr e2 in
    triop ty op c e1 e2
  | Cvtop (ty, op, e) ->
    let e = simplify_expr e in
    cvtop ty op e
  | Naryop (ty, op, es) ->
    let es = List.map (simplify_expr ~rm_extract:false) es in
    naryop ty op es
  | Extract (s, high, low) ->
    if not rm_extract then hte else extract s ~high ~low
  | Concat (e1, e2) ->
    let msb = simplify_expr ~rm_extract:false e1 in
    let lsb = simplify_expr ~rm_extract:false e2 in
    concat msb lsb

let simplify (hte : t) : t =
  let rec loop x =
    let simpl_x = simplify_expr x in
    if equal x simpl_x then simpl_x else loop simpl_x
  in
  loop hte

module Bool = struct
  let of_val = function
    | Val True -> Some true
    | Val False -> Some false
    | _ -> None

  let true_ = value True

  let false_ = value False

  let to_val b = if b then true_ else false_

  let v b = to_val b [@@inline]

  let not (b : t) =
    let bexpr = view b in
    match of_val bexpr with
    | Some b -> to_val (not b)
    | None -> (
      match bexpr with
      | Unop (Ty_bool, Not, cond) -> cond
      | _ -> unop Ty_bool Not b )

  let equal (b1 : t) (b2 : t) =
    match (view b1, view b2) with
    | Val True, Val True | Val False, Val False -> true_
    | _ -> relop Ty_bool Eq b1 b2

  let distinct (b1 : t) (b2 : t) =
    match (view b1, view b2) with
    | Val True, Val False | Val False, Val True -> true_
    | _ -> relop Ty_bool Ne b1 b2

  let and_ (b1 : t) (b2 : t) =
    match (of_val (view b1), of_val (view b2)) with
    | Some true, _ -> b2
    | _, Some true -> b1
    | Some false, _ | _, Some false -> false_
    | _ -> binop Ty_bool And b1 b2

  let or_ (b1 : t) (b2 : t) =
    match (of_val (view b1), of_val (view b2)) with
    | Some false, _ -> b2
    | _, Some false -> b1
    | Some true, _ | _, Some true -> true_
    | _ -> binop Ty_bool Or b1 b2

  let ite (c : t) (r1 : t) (r2 : t) = triop Ty_bool Ite c r1 r2
end

module Make (T : sig
  type elt

  val ty : Ty.t

  val num : elt -> Num.t
end) =
struct
  let v i = value (Num (T.num i))

  let sym x = mk_symbol Symbol.(x @: T.ty)

  let ( ~- ) e = unop T.ty Neg e

  let ( = ) e1 e2 = relop Ty_bool Eq e1 e2

  let ( != ) e1 e2 = relop Ty_bool Ne e1 e2

  let ( > ) e1 e2 = relop T.ty Gt e1 e2

  let ( >= ) e1 e2 = relop T.ty Ge e1 e2

  let ( < ) e1 e2 = relop T.ty Lt e1 e2

  let ( <= ) e1 e2 = relop T.ty Le e1 e2
end

module Bitv = struct
  module I8 = Make (struct
    type elt = int

    let ty = Ty_bitv 8

    let num i = Num.I8 i
  end)

  module I32 = Make (struct
    type elt = int32

    let ty = Ty_bitv 32

    let num i = Num.I32 i
  end)

  module I64 = Make (struct
    type elt = int64

    let ty = Ty_bitv 64

    let num i = Num.I64 i
  end)
end

module Fpa = struct
  module F32 = struct
    include Make (struct
      type elt = float

      let ty = Ty_fp 32

      let num f = Num.F32 (Int32.bits_of_float f)
    end)

    (* Redeclare equality due to incorrect theory annotation *)
    let ( = ) e1 e2 = relop (Ty_fp 32) Eq e1 e2

    let ( != ) e1 e2 = relop (Ty_fp 32) Ne e1 e2
  end

  module F64 = struct
    include Make (struct
      type elt = float

      let ty = Ty_fp 64

      let num f = Num.F64 (Int64.bits_of_float f)
    end)

    (* Redeclare equality due to incorrect theory annotation *)
    let ( = ) e1 e2 = relop (Ty_fp 64) Eq e1 e2

    let ( != ) e1 e2 = relop (Ty_fp 64) Ne e1 e2
  end
end

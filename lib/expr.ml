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

module StringSet = Set.Make(String)

type t = expr Hc.hash_consed

and expr =
  | Val of Value.t
  | Ptr of
      { base : int32
      ; offset : t
      }
  | Symbol of Symbol.t
  | List of t list
  | App : [> `Op of string ] * t list -> expr
  | Unop of Ty.t * unop * t
  | Binop of Ty.t * binop * t * t
  | Triop of Ty.t * triop * t * t * t
  | Relop of Ty.t * relop * t * t
  | Cvtop of Ty.t * cvtop * t
  | Naryop of Ty.t * naryop * t list
  | Stringop of Ty.t * stringop * t * StringSet.t
  | Extract of t * int * int
  | Concat of t * t

module Expr = struct
  type t = expr

  let list_eq (l1 : 'a list) (l2 : 'a list) : bool =
    if List.compare_lengths l1 l2 = 0 then List.for_all2 ( == ) l1 l2 else false

  let equal (e1 : expr) (e2 : expr) : bool =
    match (e1, e2) with
    | Val v1, Val v2 -> Value.equal v1 v2
    | Ptr { base = b1; offset = o1 }, Ptr { base = b2; offset = o2 } ->
      Int32.equal b1 b2 && o1 == o2
    | Symbol s1, Symbol s2 -> Symbol.equal s1 s2
    | List l1, List l2 -> list_eq l1 l2
    | App (`Op x1, l1), App (`Op x2, l2) -> String.equal x1 x2 && list_eq l1 l2
    | Unop (t1, op1, e1), Unop (t2, op2, e2) ->
      Ty.equal t1 t2 && op1 = op2 && e1 == e2
    | Binop (t1, op1, e1, e3), Binop (t2, op2, e2, e4) ->
      Ty.equal t1 t2 && op1 = op2 && e1 == e2 && e3 == e4
    | Relop (t1, op1, e1, e3), Relop (t2, op2, e2, e4) ->
      Ty.equal t1 t2 && op1 = op2 && e1 == e2 && e3 == e4
    | Triop (t1, op1, e1, e3, e5), Triop (t2, op2, e2, e4, e6) ->
      Ty.equal t1 t2 && op1 = op2 && e1 == e2 && e3 == e4 && e5 == e6
    | Cvtop (t1, op1, e1), Cvtop (t2, op2, e2) ->
      Ty.equal t1 t2 && op1 = op2 && e1 == e2
    | Naryop (t1, op1, l1), Naryop (t2, op2, l2) ->
      Ty.equal t1 t2 && op1 = op2 && list_eq l1 l2
    | Stringop (t1, op1, e1, s1), Stringop (t2, op2, e2, s2) ->
      Ty.equal t1 t2 && op1 = op2 && e1 == e2 && StringSet.equal s1 s2
    | Extract (e1, h1, l1), Extract (e2, h2, l2) ->
      e1 == e2 && h1 = h2 && l1 = l2
    | Concat (e1, e3), Concat (e2, e4) -> e1 == e2 && e3 == e4
    | _ -> false

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
    | Stringop (ty, op, e, s) -> h (ty, op, e.tag, s)
    | Extract (e, hi, lo) -> h (e.tag, hi, lo)
    | Concat (e1, e2) -> h (e1.tag, e2.tag)
end

module Hc = Hc.Make [@inlined hint] (Expr)

let equal (hte1 : t) (hte2 : t) = hte1.tag == hte2.tag

let hash (hte : t) = hte.tag

let make (e : expr) = Hc.hashcons e

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
  | Stringop (_, _, _, _) -> Ty_bool (* This is type bool but the module Ty_str *)
  | Extract (_, h, l) -> Ty_bitv ((h - l) * 8)
  | Concat (e1, e2) -> (
    match (ty e1, ty e2) with
    | Ty_bitv n1, Ty_bitv n2 -> Ty_bitv (n1 + n2)
    | t1, t2 -> Log.err "Invalid concat of (%a) with (%a)" Ty.pp t1 Ty.pp t2 )

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
  | Stringop (_, _, v, _) -> is_symbolic v
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
    | Stringop (_, _, e, _) -> symbols e
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
  open Format

  let rec pp fmt (hte : t) =
    match view hte with
    | Val v -> Value.pp fmt v
    | Ptr { base; offset } -> fprintf fmt "(Ptr (i32 %ld) %a)" base pp offset
    | Symbol s -> Symbol.pp fmt s
    | List v -> fprintf fmt "(%a)" (pp_print_list pp) v
    | App (`Op x, v) -> fprintf fmt "(%s %a)" x (pp_print_list pp) v
    | Unop (ty, op, e) -> fprintf fmt "(%a.%a %a)" Ty.pp ty pp_unop op pp e
    | Binop (ty, op, e1, e2) ->
      fprintf fmt "(%a.%a %a %a)" Ty.pp ty pp_binop op pp e1 pp e2
    | Triop (ty, op, e1, e2, e3) ->
      fprintf fmt "(%a.%a %a %a %a)" Ty.pp ty pp_triop op pp e1 pp e2 pp e3
    | Relop (ty, op, e1, e2) ->
      fprintf fmt "(%a.%a %a %a)" Ty.pp ty pp_relop op pp e1 pp e2
    | Cvtop (ty, op, e) -> fprintf fmt "(%a.%a %a)" Ty.pp ty pp_cvtop op pp e
    | Naryop (ty, op, es) ->
      fprintf fmt "(%a.%a (%a))" Ty.pp ty pp_naryop op (pp_print_list pp) es
    | Stringop (ty, op, e, s) -> fprintf fmt "(%a.%a %a {%a})" Ty.pp ty pp_stringop op pp e (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "; ") pp_print_string) (StringSet.elements s)
    | Extract (e, h, l) -> fprintf fmt "(extract %a %d %d)" pp e l h
    | Concat (e1, e2) -> fprintf fmt "(++ %a %a)" pp e1 pp e2
    | App _ -> assert false

  let pp_list fmt (es : t list) = pp_print_list ~pp_sep:pp_print_space pp fmt es

  let pp_smt fmt (es : t list) : unit =
    let pp_symbols fmt syms =
      pp_print_list ~pp_sep:pp_print_newline
        (fun fmt sym ->
          let t = Symbol.type_of sym in
          fprintf fmt "(let-const %a %a)" Symbol.pp sym Ty.pp t )
        fmt syms
    in
    let pp_asserts fmt es =
      pp_print_list ~pp_sep:pp_print_newline
        (fun fmt e -> fprintf fmt "(assert @[<h 2>%a@])" pp e)
        fmt es
    in
    let syms = get_symbols es in
    if List.length syms > 0 then fprintf fmt "%a@\n" pp_symbols syms;
    if List.length es > 0 then fprintf fmt "%a@\n" pp_asserts es;
    pp_print_string fmt "(check-sat)"
end

let pp = Pp.pp

let pp_list = Pp.pp_list

let pp_smt = Pp.pp_smt

let to_string e = Format.asprintf "%a" pp e

let is_list (hte : t) : bool =
  match (ty hte, view hte) with Ty_list, _ | _, List _ -> true | _ -> false

let value (v : Value.t) : t = make (Val v) [@@inline]

let ptr base offset = make (Ptr { base; offset })

let triop' (ty : Ty.t) (op : triop) (e1 : t) (e2 : t) (e3 : t) : t =
  make (Triop (ty, op, e1, e2, e3))
[@@inline]

let unop' (ty : Ty.t) (op : unop) (hte : t) : t = make (Unop (ty, op, hte))
[@@inline]

let binop' (ty : Ty.t) (op : binop) (hte1 : t) (hte2 : t) : t =
  make (Binop (ty, op, hte1, hte2))
[@@inline]

let relop' (ty : Ty.t) (op : relop) (hte1 : t) (hte2 : t) : t =
  make (Relop (ty, op, hte1, hte2))
[@@inline]

let naryop' (ty : Ty.t) (op : naryop) (es : t list) : t =
  make (Naryop (ty, op, es))
[@@inline]

let stringop' (ty : Ty.t) (op : stringop) (hte : t) (s : StringSet.t) : t =
  make (Stringop (ty, op, hte, s))
[@@inline]

let stringop (ty : Ty.t) (op : stringop) (hte : t) (s : StringSet.t) : t = stringop' ty op hte s

let rec triop ty (op : triop) (e1 : t) (e2 : t) (e3 : t) : t =
  match (op, view e1, view e2, view e3) with
  | Ite, Val True, _, _ -> e2
  | Ite, Val False, _, _ -> e3
  | Ite, _, _, _ when equal e2 e3 -> e2
  | Ite, _, Val True, Val False -> e1
  | Ite, _, Val False, Val True -> unop Ty_bool Not e1
  | Ite, _, Val False, _ -> binop Ty_bool And (unop Ty_bool Not e1) e3
  | Ite, _, _ , Val False -> binop Ty_bool And e1 e2
  | Ite, _, Val True, _ -> binop Ty_bool Or e1 e3
  | Ite, _, _, Val True -> binop Ty_bool Or (unop Ty_bool Not e1) e2
  | Ite, _, _, _ when equal e1 e2 -> binop Ty_bool Or e1 e3
  | Ite, _, _, _ when equal e1 e3 -> binop Ty_bool And e1 e2
  | op, Val v1, Val v2, Val v3 -> value (Eval.triop ty op v1 v2 v3)
  | _ -> triop' ty op e1 e2 e3

and

  unop (ty : Ty.t) (op : unop) (hte : t) : t =
  match (op, view hte) with
  | _, Val v -> value (Eval.unop ty op v)
  | Not, Unop (_, Not, hte') -> hte'
  | Not, Relop _ -> (match negate_relop hte with | Ok e -> e | Error msg -> failwith msg)
  | Not, Stringop (_, In, e, es) -> stringop Ty_str NotIn e es
  | Not, Stringop (_, NotIn, e, es) -> stringop Ty_str In e es
  | Neg, Unop (_, Neg, hte') -> hte'
  | Reverse, Unop (_, Reverse, hte') -> hte'
  | Head, List (hd :: _) -> hd
  | Tail, List (_ :: tl) -> make (List tl)
  | Reverse, List es -> make (List (List.rev es))
  | Length, List es -> value (Int (List.length es))
  | Length, Triop (ty', Ite, cond, hte1, hte2) ->
    triop ty' Ite cond (unop ty Length hte1) (unop ty Length hte2)
  | Head, Triop (ty', Ite, cond, hte1, hte2) ->
    triop ty' Ite cond (unop ty Head hte1) (unop ty Head hte2)
  | Tail, Triop (ty', Ite, cond, hte1, hte2) ->
    triop ty' Ite cond (unop ty Tail hte1) (unop ty Tail hte2)
  | _ -> unop' ty op hte

and 

  binop ty (op : binop) (hte1 : t) (hte2 : t) : t =
  match (op, view hte1, view hte2) with
  | _, Val v1, Val v2 -> value (Eval.binop ty op v1 v2)
  | _, Ptr { base = b1; offset = os1 }, Ptr { base = b2; offset = os2 } -> (
    match op with
    | Sub when b1 = b2 -> binop ty Sub os1 os2
    | _ ->
      (* TODO: simplify to i32 here *)
      binop' ty op hte1 hte2 )
  | _, Ptr { base; offset }, _ -> (
    match op with
    | Add ->
      let new_offset = binop (Ty_bitv 32) Add offset hte2 in
      ptr base new_offset
    | Sub ->
      let new_offset = binop (Ty_bitv 32) Sub offset hte2 in
      ptr base new_offset
    | Rem ->
      let rhs = value (Num (I32 base)) in
      let addr = binop (Ty_bitv 32) Add rhs offset in
      binop ty Rem addr hte2
    | _ -> binop' ty op hte1 hte2 )
  | _, _, Ptr { base; offset } -> (
    match op with
    | Add -> ptr base (binop (Ty_bitv 32) Add offset hte1)
    | _ -> binop' ty op hte1 hte2 )
  | _, Val (Num (I32 0l)), _ -> (
    match op with
    | Add | Or -> hte2
    | And | Div | DivU | Mul | Rem | RemU -> hte1
    | _ -> binop' ty op hte1 hte2 )
  | _, _, Val (Num (I32 0l)) -> (
    match op with
    | Add | Or | Sub -> hte1
    | And | Mul -> hte2
    | _ -> binop' ty op hte1 hte2 )
  | _, Binop (ty, op2, x, { node = Val v1; _ }), Val v2 -> (
    match (op, op2) with
    | Add, Add ->
      let v = value (Eval.binop ty Add v1 v2) in
      binop' ty Add x v
    (* | Add, Sub | Sub, Add -> *)
    (*   let v = Eval_numeric.binop (I32 Sub) v1 v2 in *)
    (*   Binop (I32 Add, x, Val (Num v)) *)
    | Sub, Sub ->
      let v = value (Eval.binop ty Add v1 v2) in
      binop' ty Sub x v
    | Mul, Mul ->
      let v = value (Eval.binop ty Mul v1 v2) in
      binop' ty Mul x v
    | _, _ -> binop' ty op hte1 hte2 )
  | _, Val v1, Binop (ty, op2, x, { node = Val v2; _ }) -> (
    match (op, op2) with
    | Add, Add ->
      let v = value (Eval.binop ty Add v1 v2) in
      binop' ty Add v x
    | Mul, Mul ->
      let v = value (Eval.binop ty Mul v1 v2) in
      binop' ty Mul v x
    | _, _ -> binop' ty op hte1 hte2 )
  | _, List es, Val (Int n) -> (
    match op with
    | At -> List.nth es n
    | List_append_last -> make (List (es @ [ hte2 ]))
    | List_append -> make (List (hte2 :: es))
    | _ -> binop' ty op hte1 hte2 )
  | _, List es, _ -> (
    match op with
    | List_append_last -> make (List (es @ [ hte2 ]))
    | List_append -> make (List (hte2 :: es))
    | _ -> binop' ty op hte1 hte2 )
  | _, Triop (ty', Ite, cond, hte1', hte2'), Val _ ->
    triop ty' Ite cond (binop ty op hte1' hte2) (binop ty op hte2' hte2)
  | And, _, _ when equal hte1 hte2 -> hte1
  | And, _, Binop (_, Or, e3, e4) when equal hte1 e3 || equal hte1 e4 -> hte1
  | And, Binop (_, Or, e1, e2), _ when equal hte2 e1 || equal hte2 e2 -> hte2
  | And, _, Naryop (_, Logor, es) when List.exists (equal hte1) es -> hte1
  | And, Naryop (_, Logor, es), _ when List.exists (equal hte2) es -> hte2
  | And, _ , Binop (_, And, e3, e4) -> 
    if equal hte1 e3 then 
      binop ty op hte1 e4
    else if equal hte1 e4 then
      binop ty op hte1 e3
    else
      binop ty op hte1 (naryop Ty_bool Logand [ e3; e4 ])
  | And, Binop (_, And, e1, e2), _ -> binop ty op hte2 (naryop Ty_bool Logand [ e1; e2 ])
  | And, _, Naryop (_, Logand, es) -> naryop Ty_bool Logand (hte1 :: es)
  | And, Naryop (_, Logand, es), _ -> naryop Ty_bool Logand (hte2 :: es)
  | And, e1, e2 -> (
    match e1, e2 with
    | Stringop (_, NotIn, e1, es1), Stringop (_, NotIn, e2, es2) when
      equal e1 e2-> 
        stringop Ty_str NotIn e1 (StringSet.union es1 es2)
    | Stringop (_, In, e1, es1), Stringop (_, In, e2, es2) when
      equal e1 e2 -> 
        stringop Ty_str In e1 (StringSet.inter es1 es2)
    | (Stringop (_, In, e1, es1), Stringop (_, NotIn, e2, es2)
    | Stringop (_, NotIn, e2, es2), Stringop (_, In, e1, es1)) when
      equal e1 e2 && (StringSet.is_empty (StringSet.inter es1 es2)) -> 
      stringop Ty_str In e1 es1
    | (Relop (_, Ne, e1, e2), Relop (_, Eq, e3, e4)
    | Relop (_, Eq, e3, e4), Relop (_, Ne, e1, e2)) -> (
      match view e1, view e2, view e3, view e4 with
      | (Symbol s1, Val v1, Symbol s2, Val v2 
      | Val v1, Symbol s1, Val v2, Symbol s2) when Symbol.equal s1 s2 ->
        if Value.equal v1 v2 then value False else relop Ty_bool Eq e3 e4
      | _ -> binop' ty op hte1 hte2)
    | (Relop (_, Ne, e1, e2), Relop (ty, (Lt | Le) , e3, e4)
    | Relop (ty, (Lt | Le), e3, e4), Relop (_, Ne, e1, e2)) when
      (equal e1 e3 && equal e2 e4 || equal e1 e4 && equal e2 e3) -> relop ty Lt e3 e4
    | (Relop (_, Ne, e1, e2), Relop (ty, (Gt | Ge), e3, e4)
    | Relop (ty, (Gt | Ge), e3, e4), Relop (_, Ne, e1, e2)) when
      (equal e1 e3 && equal e2 e4 || equal e1 e4 && equal e2 e3) -> relop ty Gt e3 e4
    | (Relop (_, Eq, e1, e2), Relop (ty, (Lt | Le) , e3, e4)
    | Relop (ty, (Lt | Le), e3, e4), Relop (_, Eq, e1, e2)) when
      (equal e1 e3 && equal e2 e4 || equal e1 e4 && equal e2 e3) -> relop ty Le e3 e4
    | (Relop (_, Eq, e1, e2), Relop (ty, (Gt | Ge) , e3, e4)
    | Relop (ty, (Gt | Ge), e3, e4), Relop (_, Eq, e1, e2)) when
      (equal e1 e3 && equal e2 e4 || equal e1 e4 && equal e2 e3) -> relop ty Ge e3 e4
    | _ -> binop' ty op hte1 hte2 
  )
  | Or, _, _ when equal hte1 hte2 -> hte1
  | Or, Val False, _ -> hte2
  | Or, _, Val False -> hte1
  | Or, _, Binop (_, And, e3, e4) when equal hte1 e3 || equal hte1 e4 -> hte1
  | Or, Stringop (_, In, e1, es1), Stringop (_, In, e2, es2) when 
    equal e1 e2 -> stringop Ty_str In e1 (StringSet.union es1 es2)
  | Or, Stringop (_, NotIn, e1, es1), Stringop (_, NotIn, e2, es2) when 
    equal e1 e2 -> stringop Ty_str NotIn e1 (StringSet.inter es1 es2)
  | (Or, Stringop (_, In, e1, es1), Stringop (_, NotIn, e2, es2)
  | Or, Stringop (_, NotIn, e2, es2), Stringop (_, In, e1, es1)) when 
    equal e1 e2 && (StringSet.subset es1 es2) -> 
    stringop Ty_str NotIn e2 (StringSet.diff es2 es1)
  | Or, _, Binop (_, Or, e3, e4) -> binop ty op hte1 (naryop Ty_bool Logor [ e3; e4 ])
  | Or, Binop (_, Or, e1, e2), _ -> binop ty op hte2 (naryop Ty_bool Logor [ e1; e2 ])
  | Or, _, Naryop (_, Logor, es) -> naryop Ty_bool Logor (hte1 :: es)
  (* FIXME: this seems wrong? *)
  (* | Binop (_, And, _, _), Val (Num (I32 1l)) -> hte1 *)
  (* | Val (Num (I32 1l)), Binop (_, And, _, _) -> hte2 *)
  | _ -> binop' ty op hte1 hte2

and

relop ty (op : relop) (hte1 : t) (hte2 : t) : t =
  match (op, view hte1, view hte2) with
  | Eq, Symbol s, Val (Str s1) when Ty.equal (Symbol.type_of s) Ty_str ->
    stringop Ty_str In hte1 (StringSet.singleton s1)
  | Ne, Symbol s, Val (Str s1) when Ty.equal (Symbol.type_of s) Ty_str ->
      stringop Ty_str NotIn hte1 (StringSet.singleton s1)
  | Eq, Val (App (`Op s1, l1)), Val (App (`Op s2, l2)) ->
    if String.equal s1 s2 && List.equal Value.equal l1 l2 then value True
    else value False
  | Ne, Val (App (`Op s1, l1)), Val (App (`Op s2, l2)) ->
    if String.equal s1 s2 && List.equal Value.equal l1 l2 then value False
    else value True
  | Eq, Val (App _), Val _ | Eq, Val _, Val (App _) -> value False
  | Ne, Val (App _), Val _ | Ne, Val _, Val (App _) -> value True
  | Eq, Relop _, Val (App _) | Eq, Val (App _),  Relop _ -> value False
  | Eq, Symbol _, Val (App _) | Eq, Val (App _), Symbol _ -> value False
  | (Eq, Symbol s, Val v | Eq, Val v, Symbol s) 
    when not (Ty.equal (Symbol.type_of s) (Value.type_of v)) -> value False
  | Ne, Val (Real v), _ when Float.is_nan v || Float.is_infinite v -> value True
  | Ne, _, Val (Real v) when Float.is_nan v || Float.is_infinite v -> value True
  | _, Val (Real v), _ when Float.is_nan v || Float.is_infinite v -> value False
  | _, _, Val (Real v) when Float.is_nan v || Float.is_infinite v -> value False
  | _, Val v1, Val v2 -> value (if Eval.relop ty op v1 v2 then True else False)
  | _, Ptr { base = b1; offset = os1 }, Ptr { base = b2; offset = os2 } -> (
    match op with
    | Eq -> if b1 = b2 then relop' ty Eq os1 os2 else value False
    | Ne -> if b1 = b2 then relop' ty Ne os1 os2 else value True
    | (LtU | LeU | GtU | GeU) as op ->
      if b1 = b2 then relop ty op os1 os2
      else
        value
          ( if Eval.relop ty op (Num (I32 b1)) (Num (I32 b2)) then True
            else False )
    | _ -> relop' ty op hte1 hte2 )
  | _, Val (Num _ as n), Ptr { base; offset = { node = Val (Num _ as o); _ } }
    ->
    let base = Eval.binop (Ty_bitv 32) Add (Num (I32 base)) o in
    value (if Eval.relop ty op n base then True else False)
  | _, Ptr { base; offset = { node = Val (Num _ as o); _ } }, Val (Num _ as n)
    ->
    let base = Eval.binop (Ty_bitv 32) Add (Num (I32 base)) o in
    value (if Eval.relop ty op base n then True else False)
  | Eq, List l1, List l2 ->
    if List.length l1 <> List.length l2 then value False
    else
      let l =
        List.fold_left2 (fun l' e1 e2 -> relop Ty_bool Eq e1 e2 :: l') [] l1 l2
      in
      naryop Ty_bool Logand l
  | Ne, List l1, List l2 ->
    if List.length l1 <> List.length l2 then value True
    else
      let l =
        List.fold_left2 (fun l' e1 e2 -> relop Ty_bool Ne e1 e2 :: l') [] l1 l2
      in
      naryop Ty_bool Logand l
  | _, List _, _ ->
    assert (is_list hte2 |> not);
    value False
  | _, _, List _ ->
    assert (is_list hte1 |> not);
    value False
  | Eq, _ , Val True -> hte1
  | Eq, Val True, _ -> hte2
  | Eq, Val False, _ -> unop Ty_bool Not hte2
  | Eq, _, Val False -> unop Ty_bool Not hte1
  | Eq, Triop (ty', Ite, cond, hte1', hte2'), Val _ -> 
    triop ty' Ite cond (relop ty Eq hte1' hte2) (relop ty Eq hte2' hte2)
  | Eq, Val _, Triop (ty', Ite, cond, hte1', hte2') -> 
    triop ty' Ite cond (relop ty Eq hte1 hte1') (relop ty Eq hte1 hte2')
  | _, _, _ -> relop' ty op hte1 hte2

and

naryop (ty : Ty.t) (op : naryop) (es : t list) : t =
  if List.for_all (fun e -> match view e with Val _ -> true | _ -> false) es
  then
    let vs =
      List.map (fun e -> match view e with Val v -> v | _ -> assert false) es
    in
    value (Eval.naryop ty op vs)
  else (
    match op, es with
    | _, [ e ] -> e
    | _, _ -> naryop' ty op es)
    

let cvtop' (ty : Ty.t) (op : cvtop) (hte : t) : t = make (Cvtop (ty, op, hte))
[@@inline]

let cvtop ty (op : cvtop) (hte : t) : t =
  match view hte with
  | Val v -> value (Eval.cvtop ty op v)
  | _ -> cvtop' ty op hte

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
  | Stringop (ty, op, e, s) ->
    let e = simplify_expr e in
    stringop ty op e s
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

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
    | ( ( Val _ | Ptr _ | Symbol _ | List _ | App _ | Unop _ | Binop _ | Triop _
        | Relop _ | Cvtop _ | Naryop _ | Extract _ | Concat _ | Binder _ )
      , _ ) ->
      false

  (* Optimized mixer (DJB2 variant). Inlines to simple arithmetic. *)
  let[@inline] combine h v = (h * 33) + v

  let hash (e : expr) : int =
    match e with
    | Val v -> Value.hash v
    | Ptr { base; offset } -> combine (Bitvector.hash base) offset.tag
    | Symbol s -> Symbol.hash s
    | List l -> List.fold_left (fun acc x -> combine acc x.Hc.tag) 0 l
    | App (s, es) ->
      let h_s = Symbol.hash s in
      List.fold_left (fun acc x -> combine acc x.Hc.tag) h_s es
    | Unop (ty, op, e) ->
      let h1 = Ty.hash ty in
      let h2 = combine h1 (Ty.Unop.hash op) in
      combine h2 e.tag
    | Binop (ty, op, e1, e2) ->
      let h = Ty.hash ty in
      let h = combine h (Ty.Binop.hash op) in
      let h = combine h e1.tag in
      combine h e2.tag
    | Triop (ty, op, e1, e2, e3) ->
      let h = Ty.hash ty in
      let h = combine h (Ty.Triop.hash op) in
      let h = combine h e1.tag in
      let h = combine h e2.tag in
      combine h e3.tag
    | Relop (ty, op, e1, e2) ->
      let h = Ty.hash ty in
      let h = combine h (Ty.Relop.hash op) in
      let h = combine h e1.tag in
      combine h e2.tag
    | Cvtop (ty, op, e) ->
      let h = Ty.hash ty in
      let h = combine h (Ty.Cvtop.hash op) in
      combine h e.tag
    | Naryop (ty, op, es) ->
      let h = Ty.hash ty in
      let h = combine h (Ty.Naryop.hash op) in
      List.fold_left (fun acc x -> combine acc x.Hc.tag) h es
    | Extract (e, hi, lo) ->
      let h = e.tag in
      let h = combine h hi in
      combine h lo
    | Concat (e1, e2) -> combine e1.tag e2.tag
    | Binder (b, vars, e) ->
      let h = Binder.hash b in
      let h_vars = List.fold_left (fun acc x -> combine acc x.Hc.tag) h vars in
      combine h_vars e.tag
end

module Hc = Hc.Make [@inlined hint] (Expr)

let equal (hte1 : t) (hte2 : t) = phys_equal hte1 hte2 [@@inline]

let hash (hte : t) = hte.tag [@@inline]

module Key = struct
  type nonrec t = t

  let to_int hte = hash hte

  let compare x y = compare (to_int x) (to_int y)
end

let[@inline] make e = Hc.hashcons e

let[@inline] view (hte : t) = hte.node

let[@inline] compare (hte1 : t) (hte2 : t) = compare hte1.tag hte2.tag

let symbol s = make (Symbol s)

(** The return type of an expression *)
let rec ty (hte : t) : Ty.t =
  match view hte with
  | Val x -> Value.type_of x
  | Ptr _ -> Ty_bitv 32
  | Symbol x -> Symbol.type_of x
  | List _ -> Ty_list
  | App (sym, _) -> begin match sym.ty with Ty_none -> Ty_app | ty -> ty end
  | Triop (_, Ite, _, hte1, hte2) ->
    let ty1 = ty hte1 in
    assert (
      let ty2 = ty hte2 in
      Ty.equal ty1 ty2 );
    ty1
  | Cvtop (_, (Zero_extend m | Sign_extend m), hte) -> (
    match ty hte with Ty_bitv n -> Ty_bitv (n + m) | _ -> assert false )
  | Unop (ty, _, _)
  | Binop (ty, _, _, _)
  | Triop (ty, _, _, _, _)
  | Relop (ty, _, _, _)
  | Cvtop (ty, _, _)
  | Naryop (ty, _, _) ->
    ty
  | Extract (_, h, l) -> Ty_bitv ((h - l) * 8)
  | Concat (e1, e2) -> (
    match (ty e1, ty e2) with
    | Ty_bitv n1, Ty_bitv n2 -> Ty_bitv (n1 + n2)
    | t1, t2 ->
      Fmt.failwith "Invalid concat of (%a) with (%a)" Ty.pp t1 Ty.pp t2 )
  | Binder (_, _, e) -> ty e

let rec is_symbolic (v : t) : bool =
  match view v with
  | Val _ -> false
  | Symbol _ -> true
  | Ptr { offset; _ } -> is_symbolic offset
  | Unop (_, _, v) | Cvtop (_, _, v) | Extract (v, _, _) | Binder (_, _, v) ->
    is_symbolic v
  | Binop (_, _, v1, v2) | Relop (_, _, v1, v2) | Concat (v1, v2) ->
    is_symbolic v1 || is_symbolic v2
  | Triop (_, _, v1, v2, v3) ->
    is_symbolic v1 || is_symbolic v2 || is_symbolic v3
  | List vs | App (_, vs) | Naryop (_, _, vs) -> List.exists is_symbolic vs

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
    | Binder (_, vars, e) ->
      List.iter symbols vars;
      symbols e
  in
  List.iter symbols hte;
  Hashtbl.fold (fun k () acc -> k :: acc) tbl []

let rec pp fmt (hte : t) =
  match view hte with
  | Val v -> Value.pp fmt v
  | Ptr { base; offset } -> Fmt.pf fmt "(Ptr %a %a)" Bitvector.pp base pp offset
  | Symbol s -> Fmt.pf fmt "@[<hov 1>%a@]" Symbol.pp s
  | List v -> Fmt.pf fmt "@[<hov 1>[%a]@]" (Fmt.list ~sep:Fmt.comma pp) v
  | App (s, v) ->
    Fmt.pf fmt "@[<hov 1>(%a@ %a)@]" Symbol.pp s (Fmt.list ~sep:Fmt.comma pp) v
  | Unop (ty, op, e) ->
    Fmt.pf fmt "@[<hov 1>(%a.%a@ %a)@]" Ty.pp ty Ty.Unop.pp op pp e
  | Binop (ty, op, e1, e2) ->
    Fmt.pf fmt "@[<hov 1>(%a.%a@ %a@ %a)@]" Ty.pp ty Ty.Binop.pp op pp e1 pp e2
  | Triop (ty, op, e1, e2, e3) ->
    Fmt.pf fmt "@[<hov 1>(%a.%a@ %a@ %a@ %a)@]" Ty.pp ty Ty.Triop.pp op pp e1 pp
      e2 pp e3
  | Relop (ty, op, e1, e2) ->
    Fmt.pf fmt "@[<hov 1>(%a.%a@ %a@ %a)@]" Ty.pp ty Ty.Relop.pp op pp e1 pp e2
  | Cvtop (ty, op, e) ->
    Fmt.pf fmt "@[<hov 1>(%a.%a@ %a)@]" Ty.pp ty Ty.Cvtop.pp op pp e
  | Naryop (ty, op, es) ->
    Fmt.pf fmt "@[<hov 1>(%a.%a@ (%a))@]" Ty.pp ty Ty.Naryop.pp op
      (Fmt.list ~sep:Fmt.comma pp)
      es
  | Extract (e, h, l) -> Fmt.pf fmt "@[<hov 1>(extract@ %a@ %d@ %d)@]" pp e l h
  | Concat (e1, e2) -> Fmt.pf fmt "@[<hov 1>(++@ %a@ %a)@]" pp e1 pp e2
  | Binder (b, vars, e) ->
    Fmt.pf fmt "@[<hov 1>(%a@ (%a)@ %a)@]" Binder.pp b (Fmt.list ~sep:Fmt.sp pp)
      vars pp e

let pp_list fmt (es : t list) = Fmt.hovbox (Fmt.list ~sep:Fmt.comma pp) fmt es

let pp_smtml fmt (es : t list) : unit =
  let def_num_printer = Num.get_default_printer () in
  Num.set_default_printer `Hexadecimal;
  Bitvector.set_default_printer `WithType;
  let pp_symbols fmt syms =
    Fmt.list ~sep:Fmt.cut
      (fun fmt sym ->
        let t = Symbol.type_of sym in
        Fmt.pf fmt "(let-const %a %a)" Symbol.pp sym Ty.pp t )
      fmt syms
  in
  let pp_asserts fmt es =
    Fmt.list ~sep:Fmt.cut
      (fun fmt e -> Fmt.pf fmt "(assert @[<h 2>%a@])" pp e)
      fmt es
  in
  let syms = get_symbols es in
  if List.length syms > 0 then Fmt.pf fmt "@[<v>%a@]@\n" pp_symbols syms;
  if List.length es > 0 then Fmt.pf fmt "@[<v>%a@]@\n" pp_asserts es;
  Fmt.string fmt "(check-sat)";
  Num.set_default_printer def_num_printer;
  Bitvector.set_default_printer `Pretty

let to_string e = Fmt.str "%a" pp e

let value (v : Value.t) : t = make (Val v) [@@inline]

let ptr base offset = make (Ptr { base = Bitvector.of_int32 base; offset })

let list l = make (List l)

let app symbol args = make (App (symbol, args))

let[@inline] binder bt vars expr = make (Binder (bt, vars, expr))

let let_in vars body = binder Let_in vars body

let forall vars body = binder Forall vars body

let exists vars body = binder Exists vars body

let raw_unop ty op hte = make (Unop (ty, op, hte)) [@@inline]

let negate_relop (hte : t) : t =
  let e =
    match view hte with
    | Relop (ty, Eq, e1, e2) -> Relop (ty, Ne, e1, e2)
    | Relop (ty, Ne, e1, e2) -> Relop (ty, Eq, e1, e2)
    | Relop (ty, Lt, e1, e2) -> Relop (ty, Le, e2, e1)
    | Relop (ty, LtU, e1, e2) -> Relop (ty, LeU, e2, e1)
    | Relop (ty, Le, e1, e2) -> Relop (ty, Lt, e2, e1)
    | Relop (ty, LeU, e1, e2) -> Relop (ty, LtU, e2, e1)
    | Relop (ty, Gt, e1, e2) -> Relop (ty, Le, e1, e2)
    | Relop (ty, GtU, e1, e2) -> Relop (ty, LeU, e1, e2)
    | Relop (ty, Ge, e1, e2) -> Relop (ty, Lt, e1, e2)
    | Relop (ty, GeU, e1, e2) -> Relop (ty, LtU, e1, e2)
    | _ -> Fmt.failwith "negate_relop: not a relop."
  in
  make e

let rec unop ty op hte =
  match (op, view hte) with
  | Ty.Unop.(Regexp_loop _ | Regexp_star), _ -> raw_unop ty op hte
  | _, Val v -> value (Eval.unop ty op v)
  | Not, Unop (_, Not, hte') -> hte'
  | Neg, Unop (_, Neg, hte') -> hte'
  | Not, Binop (inner_ty, Or, a, b) ->
    make (Binop (inner_ty, And, unop ty Not a, unop ty Not b))
  | Not, Relop (Ty_fp _, _, _, _) -> raw_unop ty op hte
  | Not, Relop (_, _, _, _) -> negate_relop hte
  | Trim, Cvtop (Ty_real, ToString, _) -> hte
  | Head, List (hd :: _) -> hd
  | Tail, List (_ :: tl) -> make (List tl)
  | Reverse, List es -> make (List (List.rev es))
  | Length, List es -> value (Int (List.length es))
  | _ -> raw_unop ty op hte

let raw_binop ty op hte1 hte2 = make (Binop (ty, op, hte1, hte2)) [@@inline]

let rec binop ty op hte1 hte2 =
  match (op, view hte1, view hte2) with
  | Ty.Binop.(String_in_re | Regexp_range), _, _ -> raw_binop ty op hte1 hte2
  | op, Val v1, Val v2 -> value (Eval.binop ty op v1 v2)
  | Sub, Ptr { base = b1; offset = os1 }, Ptr { base = b2; offset = os2 } ->
    if Bitvector.equal b1 b2 then binop ty Sub os1 os2
    else raw_binop ty op hte1 hte2
  | Add, Ptr { base; offset }, _ ->
    let m = Bitvector.numbits base in
    make (Ptr { base; offset = binop (Ty_bitv m) Add offset hte2 })
  | Sub, Ptr { base; offset }, _ ->
    let m = Bitvector.numbits base in
    make (Ptr { base; offset = binop (Ty_bitv m) Sub offset hte2 })
  | Rem, Ptr { base; offset }, _ ->
    let m = Bitvector.numbits base in
    let rhs = value (Bitv base) in
    let addr = binop (Ty_bitv m) Add rhs offset in
    binop ty Rem addr hte2
  | Add, _, Ptr { base; offset } ->
    let m = Bitvector.numbits base in
    make (Ptr { base; offset = binop (Ty_bitv m) Add offset hte1 })
  | Sub, _, Ptr { base; offset } ->
    let m = Bitvector.numbits base in
    let base = value (Bitv base) in
    binop ty Sub hte1 (binop (Ty_bitv m) Add base offset)
  | (Add | Or), Val (Bitv bv), _ when Bitvector.eqz bv -> hte2
  | (And | Div | DivU | Mul | Rem | RemU), Val (Bitv bv), _
    when Bitvector.eqz bv ->
    hte1
  | (Add | Or), _, Val (Bitv bv) when Bitvector.eqz bv -> hte1
  | Add, _, Val (Real -0.) -> hte1
  | Add, Val (Real -0.), _ -> hte2
  | Add, _, Val (Num (F32 -0l)) -> hte1
  | Add, Val (Num (F32 -0l)), _ -> hte2
  | Add, _, Val (Num (F64 -0L)) -> hte1
  | Add, Val (Num (F64 -0L)), _ -> hte2
  | (And | Mul), _, Val (Bitv bv) when Bitvector.eqz bv -> hte2
  | And, Val True, _ -> hte2
  | And, _, Val True -> hte1
  | And, Val False, _ -> hte1
  | And, _, Val False -> hte2
  | Or, Val True, _ -> hte1
  | Or, _, Val True -> hte2
  | Or, Val False, _ -> hte2
  | Or, _, Val False -> hte1
  | Add, Binop (ty, Add, x, { node = Val v1; _ }), Val v2 ->
    let v = value (Eval.binop ty Add v1 v2) in
    raw_binop ty Add x v
  | Sub, Binop (ty, Sub, x, { node = Val v1; _ }), Val v2 ->
    let v = value (Eval.binop ty Add v1 v2) in
    raw_binop ty Sub x v
  | Mul, Val (Bitv bv), _ when Bitvector.eq_one bv -> hte2
  | Mul, _, Val (Bitv bv) when Bitvector.eq_one bv -> hte1
  | Mul, Binop (ty, Mul, x, { node = Val v1; _ }), Val v2 ->
    let v = value (Eval.binop ty Mul v1 v2) in
    raw_binop ty Mul x v
  | Add, Val v1, Binop (ty, Add, x, { node = Val v2; _ }) ->
    let v = value (Eval.binop ty Add v1 v2) in
    raw_binop ty Add v x
  | Mul, Val v1, Binop (ty, Mul, x, { node = Val v2; _ }) ->
    let v = value (Eval.binop ty Mul v1 v2) in
    raw_binop ty Mul v x
  | At, List es, Val (Int n) ->
    (* TODO: use another datastructure? *)
    begin match List.nth_opt es n with None -> assert false | Some v -> v
    end
  | (String_contains | String_prefix | String_suffix), _, Val (Str "") ->
    value True
  | List_cons, _, List es -> make (List (hte1 :: es))
  | List_append, List _, (List [] | Val (List [])) -> hte1
  | List_append, (List [] | Val (List [])), List _ -> hte2
  | List_append, List l0, Val (List l1) -> make (List (l0 @ List.map value l1))
  | List_append, Val (List l0), List l1 -> make (List (List.map value l0 @ l1))
  | List_append, List l0, List l1 -> make (List (l0 @ l1))
  | _ -> raw_binop ty op hte1 hte2

let raw_triop ty op e1 e2 e3 = make (Triop (ty, op, e1, e2, e3)) [@@inline]

let triop ty op e1 e2 e3 =
  match (op, view e1, view e2, view e3) with
  | Ty.Triop.Ite, Val True, _, _ -> e2
  | Ite, Val False, _, _ -> e3
  | Ite, _, _, _ when equal e2 e3 -> e2
  | op, Val v1, Val v2, Val v3 -> value (Eval.triop ty op v1 v2 v3)
  | Ite, _, Triop (_, Ite, c2, r1, r2), Triop (_, Ite, _, _, _) ->
    let else_ = raw_triop ty Ite e1 r2 e3 in
    let cond = binop Ty_bool And e1 c2 in
    raw_triop ty Ite cond r1 else_
  | _ -> raw_triop ty op e1 e2 e3

let raw_relop ty op hte1 hte2 = make (Relop (ty, op, hte1, hte2)) [@@inline]

let rec relop ty (op : Ty.Relop.t) hte1 hte2 =
  let both_phys_eq = phys_equal hte1 hte2 in
  let can_be_shortcuted =
    match ty with
    | Ty.Ty_bool | Ty_bitv _ | Ty_int | Ty_unit -> both_phys_eq
    | Ty_fp _ | Ty_app | Ty_list | Ty_real | Ty_regexp | Ty_roundingMode
    | Ty_none | Ty_str ->
      false
  in
  match (op, view hte1, view hte2) with
  | (Eq | Le | Ge | LeU | GeU), _, _ when can_be_shortcuted -> value True
  | (Ne | Lt | Gt | LtU | GtU), _, _ when can_be_shortcuted -> value False
  | op, Val v1, Val v2 -> value (if Eval.relop ty op v1 v2 then True else False)
  | Ne, Val (Real v), _ | Ne, _, Val (Real v) ->
    if Float.is_nan v || Float.is_infinite v then value True
    else if both_phys_eq then value False
    else raw_relop ty op hte1 hte2
  | _, Val (Real v), _ | _, _, Val (Real v) ->
    if Float.is_nan v || Float.is_infinite v then value False
    else
      (* TODO: it is possible to add a shortcut when `both_phys_eq` *)
      raw_relop ty op hte1 hte2
  | Eq, _, Val Nothing | Eq, Val Nothing, _ -> value False
  | Ne, _, Val Nothing | Ne, Val Nothing, _ -> value True
  | Eq, _, Val (App (`Op "symbol", [ Str _ ]))
  | Eq, Val (App (`Op "symbol", [ Str _ ])), _ ->
    value False
  | Ne, _, Val (App (`Op "symbol", [ Str _ ]))
  | Ne, Val (App (`Op "symbol", [ Str _ ])), _ ->
    value True
  | ( Eq
    , Symbol ({ ty = Ty_fp prec1; _ } as s1)
    , Symbol ({ ty = Ty_fp prec2; _ } as s2) )
    when both_phys_eq || (prec1 = prec2 && Symbol.equal s1 s2) ->
    raw_unop Ty_bool Not (raw_unop (Ty_fp prec1) Is_nan hte1)
  | Eq, Ptr { base = b1; offset = os1 }, Ptr { base = b2; offset = os2 } ->
    if both_phys_eq then value True
    else if Bitvector.equal b1 b2 then relop Ty_bool Eq os1 os2
    else value False
  | Ne, Ptr { base = b1; offset = os1 }, Ptr { base = b2; offset = os2 } ->
    if both_phys_eq then value False
    else if Bitvector.equal b1 b2 then relop Ty_bool Ne os1 os2
    else value True
  | ( (LtU | LeU)
    , Ptr { base = b1; offset = os1 }
    , Ptr { base = b2; offset = os2 } ) ->
    if both_phys_eq then value True
    else if Bitvector.equal b1 b2 then relop ty op os1 os2
    else
      let b1 = Value.Bitv b1 in
      let b2 = Value.Bitv b2 in
      value (if Eval.relop ty op b1 b2 then True else False)
  | ( op
    , Val (Bitv _ as n)
    , Ptr { base; offset = { node = Val (Bitv _ as o); _ } } ) ->
    let base = Eval.binop (Ty_bitv 32) Add (Bitv base) o in
    value (if Eval.relop ty op n base then True else False)
  | op, Ptr { base; offset = { node = Val (Bitv _ as o); _ } }, Val (Bitv _ as n)
    ->
    let base = Eval.binop (Ty_bitv 32) Add (Bitv base) o in
    value (if Eval.relop ty op base n then True else False)
  | op, List l1, List l2 -> relop_list op l1 l2
  | Gt, _, _ -> relop ty Lt hte2 hte1
  | GtU, _, _ -> relop ty LtU hte2 hte1
  | Ge, _, _ -> relop ty Le hte2 hte1
  | GeU, _, _ -> relop ty LeU hte2 hte1
  | _, _, _ -> raw_relop ty op hte1 hte2

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

let raw_cvtop ty op hte = make (Cvtop (ty, op, hte)) [@@inline]

let rec cvtop theory op hte =
  match (op, view hte) with
  | Ty.Cvtop.String_to_re, _ -> raw_cvtop theory op hte
  | _, Val v -> value (Eval.cvtop theory op v)
  | ToBool, Cvtop (_, OfBool, hte) -> hte
  | OfBool, Cvtop (_, ToBool, hte) -> hte
  | String_to_float, Cvtop (Ty_real, ToString, hte) -> hte
  | Reinterpret_float, Cvtop (Ty_real, Reinterpret_int, e1) -> e1
  | Reinterpret_float, Cvtop (Ty_fp n, Reinterpret_int, e) ->
    assert (match theory with Ty_bitv m -> n = m | _ -> false);
    e
  | Reinterpret_int, Cvtop (Ty_int, Reinterpret_float, e1) -> e1
  | Reinterpret_int, Cvtop (Ty_bitv m, Reinterpret_float, e) ->
    assert (match theory with Ty_fp n -> n = m | _ -> false);
    e
  | Zero_extend n, Ptr { base; offset } ->
    let offset = cvtop theory op offset in
    make (Ptr { base = Bitvector.zero_extend n base; offset })
  | WrapI64, Ptr { base; offset } ->
    let offset = cvtop theory op offset in
    make (Ptr { base = Bitvector.extract base ~high:31 ~low:0; offset })
  | WrapI64, Cvtop (Ty_bitv 64, Zero_extend 32, hte) ->
    assert (Ty.equal theory (ty hte) && Ty.equal theory (Ty_bitv 32));
    hte
  | ToString, Cvtop (_, OfString, e1) -> e1
  | OfString, Cvtop (_, ToString, e1) -> e1
  | PromoteF32, Cvtop (_, DemoteF64, e1) -> e1
  | DemoteF64, Cvtop (_, PromoteF32, e1) -> e1
  | Zero_extend 0, _ -> hte
  | Sign_extend 0, _ -> hte
  | String_from_code, Cvtop (_, String_to_code, e1) -> e1
  | String_to_code, Cvtop (_, String_from_code, e1) -> e1
  | _ -> raw_cvtop theory op hte

let raw_naryop ty op es = make (Naryop (ty, op, es)) [@@inline]

let naryop ty op es =
  if List.for_all (fun e -> match view e with Val _ -> true | _ -> false) es
  then
    let vs =
      List.map (fun e -> match view e with Val v -> v | _ -> assert false) es
    in
    value (Eval.naryop ty op vs)
  else
    match (ty, op, List.map view es) with
    | ( Ty_str
      , Concat
      , [ Naryop (Ty_str, Concat, l1); Naryop (Ty_str, Concat, l2) ] ) ->
      raw_naryop Ty_str Concat (l1 @ l2)
    | Ty_str, Concat, [ Naryop (Ty_str, Concat, htes); hte ] ->
      raw_naryop Ty_str Concat (htes @ [ make hte ])
    | Ty_str, Concat, [ hte; Naryop (Ty_str, Concat, htes) ] ->
      raw_naryop Ty_str Concat (make hte :: htes)
    | _ -> raw_naryop ty op es

let[@inline] raw_extract (hte : t) ~(high : int) ~(low : int) : t =
  make (Extract (hte, high, low))

let extract (hte : t) ~(high : int) ~(low : int) : t =
  match (view hte, high, low) with
  | Val (Bitv bv), high, low ->
    let high = (high * 8) - 1 in
    let low = low * 8 in
    value (Bitv (Bitvector.extract bv ~high ~low))
  | ( Cvtop
        ( _
        , (Zero_extend 24 | Sign_extend 24)
        , ({ node = Symbol { ty = Ty_bitv 8; _ }; _ } as sym) )
    , 1
    , 0 ) ->
    sym
  | Concat (_, e), h, l when Ty.size (ty e) = h - l -> e
  | Concat (e, _), 8, 4 when Ty.size (ty e) = 4 -> e
  | _ ->
    if high - low = Ty.size (ty hte) then hte else raw_extract hte ~high ~low

let raw_concat (msb : t) (lsb : t) : t = make (Concat (msb, lsb)) [@@inline]

(* TODO: don't rebuild so many values it generates unecessary hc lookups *)
let rec concat (msb : t) (lsb : t) : t =
  match (view msb, view lsb) with
  | Val (Bitv a), Val (Bitv b) -> value (Bitv (Bitvector.concat a b))
  | Val (Bitv _), Concat (({ node = Val (Bitv _); _ } as b), se) ->
    raw_concat (concat msb b) se
  | Extract (s1, h, m1), Extract (s2, m2, l) when equal s1 s2 && m1 = m2 ->
    if h - l = Ty.size (ty s1) then s1 else raw_extract s1 ~high:h ~low:l
  | Extract (_, _, _), Concat (({ node = Extract (_, _, _); _ } as e2), e3) ->
    raw_concat (concat msb e2) e3
  | _ -> raw_concat msb lsb

let rec simplify_expr ?(in_relop = false) (hte : t) : t =
  match view hte with
  | Val _ | Symbol _ -> hte
  | Ptr { base; offset } ->
    let offset = simplify_expr ~in_relop offset in
    if not in_relop then make (Ptr { base; offset })
    else binop (Ty_bitv 32) Add (value (Bitv base)) offset
  | List es -> make @@ List (List.map (simplify_expr ~in_relop) es)
  | App (x, es) -> make @@ App (x, List.map (simplify_expr ~in_relop) es)
  | Unop (ty, op, e) ->
    let e = simplify_expr ~in_relop e in
    unop ty op e
  | Binop (ty, op, e1, e2) ->
    let e1 = simplify_expr ~in_relop e1 in
    let e2 = simplify_expr ~in_relop e2 in
    binop ty op e1 e2
  | Relop (ty, op, e1, e2) ->
    let e1 = simplify_expr ~in_relop:true e1 in
    let e2 = simplify_expr ~in_relop:true e2 in
    relop ty op e1 e2
  | Triop (ty, op, c, e1, e2) ->
    let c = simplify_expr ~in_relop c in
    let e1 = simplify_expr ~in_relop e1 in
    let e2 = simplify_expr ~in_relop e2 in
    triop ty op c e1 e2
  | Cvtop (ty, op, e) ->
    let e = simplify_expr ~in_relop e in
    cvtop ty op e
  | Naryop (ty, op, es) ->
    let es = List.map (simplify_expr ~in_relop) es in
    naryop ty op es
  | Extract (s, high, low) ->
    let s = simplify_expr ~in_relop s in
    extract s ~high ~low
  | Concat (e1, e2) ->
    let msb = simplify_expr ~in_relop e1 in
    let lsb = simplify_expr ~in_relop e2 in
    concat msb lsb
  | Binder _ ->
    (* Not simplifying anything atm *)
    hte

module Cache = Hashtbl.Make (struct
  type nonrec t = t

  let hash = hash

  let equal = equal
end)

let simplify =
  (* TODO: it may make sense to share the cache with simplify_expr ? *)
  let cache = Cache.create 512 in
  fun e ->
    match Cache.find_opt cache e with
    | Some simplified -> simplified
    | None ->
      let rec loop x =
        let x' = simplify_expr x in
        if equal x x' then begin
          Cache.add cache e x';
          x'
        end
        else loop x'
      in
      loop e

module Bool = struct
  open Ty

  let of_val = function
    | Val True -> Some true
    | Val False -> Some false
    | _ -> None

  let true_ = value True

  let false_ = value False

  let to_val b = if b then true_ else false_

  let v b = to_val b [@@inline]

  let not b =
    let bexpr = view b in
    match of_val bexpr with
    | Some b -> to_val (not b)
    | None -> (
      match bexpr with
      | Unop (Ty_bool, Not, cond) -> cond
      | _ -> unop Ty_bool Not b )

  let equal b1 b2 =
    match (view b1, view b2) with
    | Val True, Val True | Val False, Val False -> true_
    | _ -> relop Ty_bool Eq b1 b2

  let distinct b1 b2 =
    match (view b1, view b2) with
    | Val True, Val False | Val False, Val True -> true_
    | _ -> relop Ty_bool Ne b1 b2

  let and_ b1 b2 =
    match (of_val (view b1), of_val (view b2)) with
    | Some true, _ -> b2
    | _, Some true -> b1
    | Some false, _ | _, Some false -> false_
    | _ -> binop Ty_bool And b1 b2

  let or_ b1 b2 =
    match (of_val (view b1), of_val (view b2)) with
    | Some false, _ -> b2
    | _, Some false -> b1
    | Some true, _ | _, Some true -> true_
    | _ -> binop Ty_bool Or b1 b2

  let ite c r1 r2 = triop Ty_bool Ite c r1 r2
end

module Make (T : sig
  type elt

  val ty : Ty.t

  val value : elt -> Value.t
end) =
struct
  open Ty

  let v i = value (T.value i)

  let sym x = symbol Symbol.(x @: T.ty)

  let ( ~- ) e = unop T.ty Neg e

  let ( = ) e1 e2 = relop Ty_bool Eq e1 e2

  let ( != ) e1 e2 = relop Ty_bool Ne e1 e2

  let ( > ) e1 e2 = relop T.ty Gt e1 e2

  let ( >= ) e1 e2 = relop T.ty Ge e1 e2

  let ( < ) e1 e2 = relop T.ty Lt e1 e2

  let ( <= ) e1 e2 = relop T.ty Le e1 e2
end

module Bitv = struct
  open Ty

  module I8 = Make (struct
    type elt = int

    let ty = Ty_bitv 8

    let value i = Value.Bitv (Bitvector.of_int8 i)
  end)

  module I32 = Make (struct
    type elt = int32

    let ty = Ty_bitv 32

    let value i = Value.Bitv (Bitvector.of_int32 i)
  end)

  module I64 = Make (struct
    type elt = int64

    let ty = Ty_bitv 64

    let value i = Value.Bitv (Bitvector.of_int64 i)
  end)
end

module Fpa = struct
  open Ty

  module F32 = struct
    include Make (struct
      type elt = float

      let ty = Ty_fp 32

      let value f = Value.Num (F32 (Int32.bits_of_float f))
    end)

    (* Redeclare equality due to incorrect theory annotation *)
    let ( = ) e1 e2 = relop (Ty_fp 32) Eq e1 e2

    let ( != ) e1 e2 = relop (Ty_fp 32) Ne e1 e2
  end

  module F64 = struct
    include Make (struct
      type elt = float

      let ty = Ty_fp 64

      let value f = Value.Num (F64 (Int64.bits_of_float f))
    end)

    (* Redeclare equality due to incorrect theory annotation *)
    let ( = ) e1 e2 = relop (Ty_fp 64) Eq e1 e2

    let ( != ) e1 e2 = relop (Ty_fp 64) Ne e1 e2
  end
end

module Smtlib = struct
  let rec pp fmt (hte : t) =
    match view hte with
    | Val v -> Value.Smtlib.pp fmt v
    | Ptr _ -> assert false
    | Symbol s -> Fmt.pf fmt "@[<hov 1>%a@]" Symbol.pp s
    | List _ -> assert false
    | App _ -> assert false
    | Unop (ty, op, e) ->
      Fmt.pf fmt "@[<hov 1>(%a@ %a)@]" Ty.Smtlib.pp_unop (ty, op) pp e
    | Binop (ty, op, e1, e2) ->
      Fmt.pf fmt "@[<hov 1>(%a@ %a@ %a)@]" Ty.Smtlib.pp_binop (ty, op) pp e1 pp
        e2
    | Triop _ -> assert false
    | Relop (ty, op, e1, e2) ->
      Fmt.pf fmt "@[<hov 1>(%a@ %a@ %a)@]" Ty.Smtlib.pp_relop (ty, op) pp e1 pp
        e2
    | Cvtop _ -> assert false
    | Naryop _ -> assert false
    | Extract _ -> assert false
    | Concat _ -> assert false
    | Binder _ -> assert false
end

let inline_symbol_values map e =
  let rec aux e =
    match view e with
    | Val _ -> e
    | Symbol symbol -> Option.value ~default:e (Symbol.Map.find_opt symbol map)
    | Ptr e ->
      let offset = aux e.offset in
      make @@ Ptr { e with offset }
    | List vs ->
      let vs = List.map aux vs in
      list vs
    | App (x, vs) ->
      let vs = List.map aux vs in
      app x vs
    | Unop (ty, op, v) ->
      let v = aux v in
      unop ty op v
    | Binop (ty, op, v1, v2) ->
      let v1 = aux v1 in
      let v2 = aux v2 in
      binop ty op v1 v2
    | Triop (ty, op, v1, v2, v3) ->
      let v1 = aux v1 in
      let v2 = aux v2 in
      let v3 = aux v3 in
      triop ty op v1 v2 v3
    | Cvtop (ty, op, v) ->
      let v = aux v in
      cvtop ty op v
    | Relop (ty, op, v1, v2) ->
      let v1 = aux v1 in
      let v2 = aux v2 in
      relop ty op v1 v2
    | Naryop (ty, op, vs) ->
      let vs = List.map aux vs in
      naryop ty op vs
    | Extract (e, high, low) ->
      let e = aux e in
      extract e ~high ~low
    | Concat (e1, e2) ->
      let e1 = aux e1 in
      let e2 = aux e2 in
      concat e1 e2
    | Binder (b, vars, e) ->
      let e = aux e in
      binder b vars e
  in
  aux e

module Set = struct
  include Set.Make (Key)

  type key = Key.t

  let hash = Hashtbl.hash

  let to_list s = elements s

  let pp fmt v =
    Fmt.pf fmt "@[<hov 1>%a@]"
      (Fmt.iter iter ~sep:(fun fmt () -> Fmt.pf fmt "@;") pp)
      v

  let get_symbols (set : t) =
    let tbl = Hashtbl.create 64 in
    let rec symbols hte =
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
      | Binder (_, vars, e) ->
        List.iter symbols vars;
        symbols e
    in
    iter symbols set;
    Hashtbl.fold (fun k () acc -> k :: acc) tbl []

  let map f set =
    fold
      (fun elt set ->
        let elt = f elt in
        add elt set )
      set empty

  let inline_symbol_values symbol_map set =
    map (inline_symbol_values symbol_map) set
end

let rec split_conjunctions (e : t) : Set.t =
  match view e with
  | Binop (Ty_bool, And, e1, e2) ->
    let s1 = split_conjunctions e1 in
    let s2 = split_conjunctions e2 in
    Set.union s1 s2
  | _ -> Set.singleton e

let rec split_disjunctions (e : t) : Set.t =
  match view e with
  | Binop (Ty_bool, Or, e1, e2) ->
    let s1 = split_disjunctions e1 in
    let s2 = split_disjunctions e2 in
    Set.union s1 s2
  | _ -> Set.singleton e

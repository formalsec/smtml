(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2024 formalsec *)
(* Written by the Smtml programmers *)

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
      Int32.equal b1 b2 && phys_equal o1 o2
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

let symbol s = make (Symbol s)

(** The return type of an expression *)
let rec ty (hte : t) : Ty.t =
  match view hte with
  | Val x -> Value.type_of x
  | Ptr _ -> Ty_bitv 32
  | Symbol x -> Symbol.type_of x
  | List _ -> Ty_list
  | App _ -> Ty_app
  | Unop (ty, _, _) -> ty
  | Binop (ty, _, _, _) -> ty
  | Triop (_, Ite, _, hte1, hte2) ->
    let ty1 = ty hte1 in
    let ty2 = ty hte2 in
    assert (Ty.equal ty1 ty2);
    ty1
  | Triop (ty, _, _, _, _) -> ty
  | Relop (ty, _, _, _) -> ty
  | Cvtop (_, (Zero_extend m | Sign_extend m), hte) -> (
    match ty hte with Ty_bitv n -> Ty_bitv (n + m) | _ -> assert false )
  | Cvtop (ty, _, _) -> ty
  | Naryop (ty, _, _) -> ty
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
  | Binder (_, _, e) -> is_symbolic e

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

let negate_relop (hte : t) : (t, string) Result.t =
  let e =
    match view hte with
    | Relop (ty, Eq, e1, e2) -> Ok (Relop (ty, Ne, e1, e2))
    | Relop (ty, Ne, e1, e2) -> Ok (Relop (ty, Eq, e1, e2))
    | Relop (ty, Lt, e1, e2) -> Ok (Relop (ty, Le, e2, e1))
    | Relop (ty, LtU, e1, e2) -> Ok (Relop (ty, LeU, e2, e1))
    | Relop (ty, Le, e1, e2) -> Ok (Relop (ty, Lt, e2, e1))
    | Relop (ty, LeU, e1, e2) -> Ok (Relop (ty, LtU, e2, e1))
    | Relop (ty, Gt, e1, e2) -> Ok (Relop (ty, Le, e1, e2))
    | Relop (ty, GtU, e1, e2) -> Ok (Relop (ty, LeU, e1, e2))
    | Relop (ty, Ge, e1, e2) -> Ok (Relop (ty, Lt, e1, e2))
    | Relop (ty, GeU, e1, e2) -> Ok (Relop (ty, LtU, e1, e2))
    | _ -> Error "negate_relop: not a relop."
  in
  Result.map make e

module Set = struct
  include PatriciaTree.MakeHashconsedSet (Key) ()

  let hash = to_int

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
end

module Pp = struct
  let rec pp fmt (hte : t) =
    match view hte with
    | Val v -> Value.pp fmt v
    | Ptr { base; offset } -> Fmt.pf fmt "(Ptr (i32 %ld) %a)" base pp offset
    | Symbol s -> Fmt.pf fmt "@[<hov 1>%a@]" Symbol.pp s
    | List v -> Fmt.pf fmt "@[<hov 1>[%a]@]" (Fmt.list ~sep:Fmt.comma pp) v
    | App (s, v) ->
      Fmt.pf fmt "@[<hov 1>(%a@ %a)@]" Symbol.pp s
        (Fmt.list ~sep:Fmt.comma pp)
        v
    | Unop (ty, op, e) ->
      Fmt.pf fmt "@[<hov 1>(%a.%a@ %a)@]" Ty.pp ty Ty.Unop.pp op pp e
    | Binop (ty, op, e1, e2) ->
      Fmt.pf fmt "@[<hov 1>(%a.%a@ %a@ %a)@]" Ty.pp ty Ty.Binop.pp op pp e1 pp
        e2
    | Triop (ty, op, e1, e2, e3) ->
      Fmt.pf fmt "@[<hov 1>(%a.%a@ %a@ %a@ %a)@]" Ty.pp ty Ty.Triop.pp op pp e1
        pp e2 pp e3
    | Relop (ty, op, e1, e2) ->
      Fmt.pf fmt "@[<hov 1>(%a.%a@ %a@ %a)@]" Ty.pp ty Ty.Relop.pp op pp e1 pp
        e2
    | Cvtop (ty, op, e) ->
      Fmt.pf fmt "@[<hov 1>(%a.%a@ %a)@]" Ty.pp ty Ty.Cvtop.pp op pp e
    | Naryop (ty, op, es) ->
      Fmt.pf fmt "@[<hov 1>(%a.%a@ (%a))@]" Ty.pp ty Ty.Naryop.pp op
        (Fmt.list ~sep:Fmt.comma pp)
        es
    | Extract (e, h, l) ->
      Fmt.pf fmt "@[<hov 1>(extract@ %a@ %d@ %d)@]" pp e l h
    | Concat (e1, e2) -> Fmt.pf fmt "@[<hov 1>(++@ %a@ %a)@]" pp e1 pp e2
    | Binder (b, vars, e) ->
      Fmt.pf fmt "@[<hov 1>(%a@ (%a)@ %a)@]" Binder.pp b
        (Fmt.list ~sep:Fmt.sp pp) vars pp e

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

let list l = make (List l)

let app symbol args = make (App (symbol, args))

let[@inline] binder bt vars expr = make (Binder (bt, vars, expr))

let let_in vars body = binder Let_in vars body

let forall vars body = binder Forall vars body

let exists vars body = binder Exists vars body

let raw_unop ty op hte = make (Unop (ty, op, hte)) [@@inline]

let unop ty op hte =
  match (op, view hte) with
  | Ty.Unop.(Regexp_loop _ | Regexp_star), _ -> raw_unop ty op hte
  | _, Val v -> value (Eval.unop ty op v)
  | Not, Unop (_, Not, hte') -> hte'
  | Not, Relop (_, _, _, _) ->
     begin match negate_relop hte with Ok a -> a | Error _ -> assert false end
  | Neg, Unop (_, Neg, hte') -> hte'
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
    if Int32.equal b1 b2 then binop ty Sub os1 os2
    else raw_binop ty op hte1 hte2
  | Add, Ptr { base; offset }, _ ->
    ptr base (binop (Ty_bitv 32) Add offset hte2)
  | Sub, Ptr { base; offset }, _ ->
    ptr base (binop (Ty_bitv 32) Sub offset hte2)
  | Rem, Ptr { base; offset }, _ ->
    let rhs = value (Bitv (Bitvector.of_int32 base)) in
    let addr = binop (Ty_bitv 32) Add rhs offset in
    binop ty Rem addr hte2
  | Add, _, Ptr { base; offset } ->
    ptr base (binop (Ty_bitv 32) Add offset hte1)
  | Sub, _, Ptr { base; offset } ->
    let base = value (Bitv (Bitvector.of_int32 base)) in
    binop ty Sub hte1 (binop (Ty_bitv 32) Add base offset)
  | (Add | Or), Val (Bitv bv), _ when Bitvector.eqz bv -> hte2
  | (And | Div | DivU | Mul | Rem | RemU), Val (Bitv bv), _
    when Bitvector.eqz bv ->
    hte1
  | (Add | Or), _, Val (Bitv bv) when Bitvector.eqz bv -> hte1
  | (And | Mul), _, Val (Bitv bv) when Bitvector.eqz bv -> hte2
  | Add, Binop (ty, Add, x, { node = Val v1; _ }), Val v2 ->
    let v = value (Eval.binop ty Add v1 v2) in
    raw_binop ty Add x v
  | Sub, Binop (ty, Sub, x, { node = Val v1; _ }), Val v2 ->
    let v = value (Eval.binop ty Add v1 v2) in
    raw_binop ty Sub x v
  | Mul, Val (Num (I32 1l)), _ -> hte2
  | Mul, _, Val (Num (I32 1l)) -> hte1
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
    begin
      match List.nth_opt es n with None -> assert false | Some v -> v
    end
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
  | op, Val v1, Val v2, Val v3 -> value (Eval.triop ty op v1 v2 v3)
  | Ite, _, Triop (_, Ite, c2, r1, r2), Triop (_, Ite, _, _, _) ->
    let else_ = raw_triop ty Ite e1 r2 e3 in
    let cond = binop Ty_bool And e1 c2 in
    raw_triop ty Ite cond r1 else_
  | _ -> raw_triop ty op e1 e2 e3

let raw_relop ty op hte1 hte2 = make (Relop (ty, op, hte1, hte2)) [@@inline]

let rec relop ty op hte1 hte2 =
  match (op, view hte1, view hte2) with
  | op, Val v1, Val v2 -> value (if Eval.relop ty op v1 v2 then True else False)
  | Ty.Relop.Ne, Val (Real v), _ | Ne, _, Val (Real v) ->
    if Float.is_nan v || Float.is_infinite v then value True
    else raw_relop ty op hte1 hte2
  | _, Val (Real v), _ | _, _, Val (Real v) ->
    if Float.is_nan v || Float.is_infinite v then value False
    else raw_relop ty op hte1 hte2
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
  | ( (LtU | LeU)
    , Ptr { base = b1; offset = os1 }
    , Ptr { base = b2; offset = os2 } ) ->
    if Int32.equal b1 b2 then relop ty op os1 os2
    else
      let b1 = Value.Bitv (Bitvector.of_int32 b1) in
      let b2 = Value.Bitv (Bitvector.of_int32 b2) in
      value (if Eval.relop ty op b1 b2 then True else False)
  | ( op
    , Val (Bitv _ as n)
    , Ptr { base; offset = { node = Val (Bitv _ as o); _ } } ) ->
    let base = Eval.binop (Ty_bitv 32) Add (Bitv (Bitvector.of_int32 base)) o in
    value (if Eval.relop ty op n base then True else False)
  | op, Ptr { base; offset = { node = Val (Bitv _ as o); _ } }, Val (Bitv _ as n)
    ->
    let base = Eval.binop (Ty_bitv 32) Add (Bitv (Bitvector.of_int32 base)) o in
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

let cvtop ty op hte =
  match (op, view hte) with
  | Ty.Cvtop.String_to_re, _ -> raw_cvtop ty op hte
  | _, Val v -> value (Eval.cvtop ty op v)
  | String_to_float, Cvtop (Ty_real, ToString, real) -> real
  | _ -> raw_cvtop ty op hte

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
    if not in_relop then ptr base offset
    else binop (Ty_bitv 32) Add (value (Bitv (Bitvector.of_int32 base))) offset
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

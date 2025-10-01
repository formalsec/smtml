(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2024 formalsec *)
(* Written by the Smtml programmers *)

include Base_expr

let symbol s = make (Symbol s)

(** The return type of an expression *)
let rec ty (hte : t) : Ty.t =
  match view hte with
  | Val x -> Value.type_of x
  | Ptr _ -> Ty_bitv 32
  | Loc _ -> Ty_app
  | Symbol x -> Symbol.type_of x
  | List _ -> Ty_list
  | App (sym, _) -> begin match sym.ty with Ty_none -> Ty_app | ty -> ty end
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
  | Extract (_, h, l) -> Ty_bitv (h - l + 1)
  | Concat (e1, e2) -> (
    match (ty e1, ty e2) with
    | Ty_bitv n1, Ty_bitv n2 -> Ty_bitv (n1 + n2)
    | t1, t2 ->
      Fmt.failwith "Invalid concat of (%a) with (%a)" Ty.pp t1 Ty.pp t2 )
  | Binder (_, _, e) -> ty e

module TyTbl = Hashtbl.Make (Expr)

let memoize_ty (hte : t) : Ty.t =
  let ty_tbl = TyTbl.create 64 in
  let rec aux (hte : t) =
    match TyTbl.find_opt ty_tbl (view hte) with
    | Some ty -> ty
    | None ->
      let ty =
        match view hte with
        | Val x -> Value.type_of x
        | Ptr _ -> Ty_bitv 32
        | Loc _ -> Ty_app
        | Symbol x -> Symbol.type_of x
        | List _ -> Ty_list
        | App (sym, _) -> begin
          match sym.ty with Ty_none -> Ty_app | ty -> ty
        end
        | Unop (ty, _, _) -> ty
        | Binop (ty, _, _, _) -> ty
        | Triop (_, Ite, _, hte1, hte2) ->
          let ty1 = aux hte1 in
          let ty2 = aux hte2 in
          assert (Ty.equal ty1 ty2);
          ty1
        | Triop (ty, _, _, _, _) -> ty
        | Relop (ty, _, _, _) -> ty
        | Cvtop (_, (Zero_extend m | Sign_extend m), hte) -> (
          match ty hte with Ty_bitv n -> Ty_bitv (n + m) | _ -> assert false )
        | Cvtop (ty, _, _) -> ty
        | Naryop (ty, _, _) -> ty
        | Extract (_, h, l) -> Ty_bitv (h - l + 1)
        | Concat (e1, e2) -> (
          match (aux e1, aux e2) with
          | Ty_bitv n1, Ty_bitv n2 -> Ty_bitv (n1 + n2)
          | t1, t2 ->
            Fmt.failwith "Invalid concat of (%a) with (%a)" Ty.pp t1 Ty.pp t2 )
        | Binder (_, _, e) -> aux e
      in
      TyTbl.add ty_tbl (view hte) ty;
      ty
  in
  aux hte

let rec is_symbolic (v : t) : bool =
  match view v with
  | Val _ -> false
  | Symbol _ -> true
  | Loc _ -> false
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
    | Val _ | Loc _ -> ()
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
  | Loc l -> Fmt.pf fmt "(loc %a)" Loc.pp l
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

module Set = struct
  include PatriciaTree.MakeHashconsedSet (Key) ()

  let hash = to_int

  let pp fmt v =
    Fmt.pf fmt "@[<hov 1>%a@]"
      (pretty ~pp_sep:(fun fmt () -> Fmt.pf fmt "@;") pp)
      v

  let get_symbols (set : t) =
    let tbl = Hashtbl.create 64 in
    let rec symbols hte =
      match view hte with
      | Val _ | Loc _ -> ()
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


let ptr base offset = make (Ptr { base = Bitvector.of_int32 base; offset })

let loc l = make (Loc l)

let[@inline] binder bt vars expr = make (Binder (bt, vars, expr))

let let_in vars body = binder Let_in vars body

let forall vars body = binder Forall vars body

let exists vars body = binder Exists vars body

let unop ty op hte = Simplifier.simplify_unop ty op hte

let binop ty op hte1 hte2 = Simplifier.simplify_binop ty op hte1 hte2

let triop ty op hte1 hte2 hte3 = Simplifier.simplify_triop ty op hte1 hte2 hte3

let naryop ty op htes = Simplifier.simplify_naryop ty op htes

let normalize_eq_or_ne op (ty', e1, e2) =
  let make_relop lhs rhs = Relop (ty', op, lhs, rhs) in
  let ty1, ty2 = (memoize_ty e1, memoize_ty e2) in
  if not (Ty.equal ty1 ty2) then make_relop e1 e2
  else begin
    match ty1 with
    | Ty_bitv m ->
      let binop = make (Binop (ty1, Sub, e1, e2)) in
      let zero = make (Val (Bitv (Bitvector.make Z.zero m))) in
      make_relop binop zero
    | Ty_int ->
      let binop = make (Binop (ty1, Sub, e1, e2)) in
      let zero = make (Val (Int Int.zero)) in
      make_relop binop zero
    | Ty_real ->
      let binop = make (Binop (ty1, Sub, e1, e2)) in
      let zero = make (Val (Real 0.)) in
      make_relop binop zero
    | _ -> make_relop e1 e2
  end

let negate_relop (hte : t) : t =
  let e =
    match view hte with
    | Relop (ty, Eq, e1, e2) -> normalize_eq_or_ne Ne (ty, e1, e2)
    | Relop (ty, Ne, e1, e2) -> normalize_eq_or_ne Eq (ty, e1, e2)
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
  | ( Eq
    , Symbol ({ ty = Ty_fp prec1; _ } as s1)
    , Symbol ({ ty = Ty_fp prec2; _ } as s2) )
    when prec1 = prec2 && Symbol.equal s1 s2 ->
    raw_unop Ty_bool Not (raw_unop (Ty_fp prec1) Is_nan hte1)
  | Eq, Ptr { base = b1; offset = os1 }, Ptr { base = b2; offset = os2 } ->
    if Bitvector.equal b1 b2 then relop Ty_bool Eq os1 os2 else value False
  | Ne, Ptr { base = b1; offset = os1 }, Ptr { base = b2; offset = os2 } ->
    if Bitvector.equal b1 b2 then relop Ty_bool Ne os1 os2 else value True
  | ( (LtU | LeU)
    , Ptr { base = b1; offset = os1 }
    , Ptr { base = b2; offset = os2 } ) ->
    if Bitvector.equal b1 b2 then relop ty op os1 os2
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
  | String_to_float, Cvtop (Ty_real, ToString, hte) -> hte
  | ( Reinterpret_float
    , Cvtop (Ty_real, Reinterpret_int, { node = Symbol { ty = Ty_int; _ }; _ })
    ) ->
    hte
  | Zero_extend n, Ptr { base; offset } ->
    let offset = cvtop theory op offset in
    make (Ptr { base = Bitvector.zero_extend n base; offset })
  | WrapI64, Ptr { base; offset } ->
    let offset = cvtop theory op offset in
    make (Ptr { base = Bitvector.extract base ~high:31 ~low:0; offset })
  | WrapI64, Cvtop (Ty_bitv 64, Zero_extend 32, hte) ->
    assert (Ty.equal theory (ty hte) && Ty.equal theory (Ty_bitv 32));
    hte
  | _ -> raw_cvtop theory op hte


let[@inline] raw_extract (hte : t) ~(high : int) ~(low : int) : t =
  make (Extract (hte, high, low))

let extract (hte : t) ~(high : int) ~(low : int) : t =
  match (view hte, high, low) with
  | Val (Bitv bv), high, low -> value (Bitv (Bitvector.extract bv ~high ~low))
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
  | Val _ | Symbol _ | Loc _ -> hte
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

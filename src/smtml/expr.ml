(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2024 formalsec *)
(* Written by the Smtml programmers *)

module Hashcons = Hc

type t =
  | Imm of Value.t
  | Sym of expr Hc.hash_consed

and expr =
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

let equal_of_t a b =
  match (a, b) with
  | Imm a, Imm b -> Value.equal a b
  | Sym a, Sym b -> phys_equal a b
  | (Imm _ | Sym _), _ -> false

let[@inline] hash_of_t e = match e with Imm v -> Value.hash v | Sym e -> e.tag

module Expr = struct
  type t = expr

  let list_eq (l1 : 'a list) (l2 : 'a list) : bool =
    if List.compare_lengths l1 l2 = 0 then List.for_all2 equal_of_t l1 l2
    else false

  let equal (e1 : expr) (e2 : expr) : bool =
    match (e1, e2) with
    | Ptr { base = b1; offset = o1 }, Ptr { base = b2; offset = o2 } ->
      Bitvector.equal b1 b2 && equal_of_t o1 o2
    | Symbol s1, Symbol s2 -> Symbol.equal s1 s2
    | List l1, List l2 -> list_eq l1 l2
    | App (s1, l1), App (s2, l2) -> Symbol.equal s1 s2 && list_eq l1 l2
    | Unop (t1, op1, e1), Unop (t2, op2, e2) ->
      Ty.equal t1 t2 && Ty.Unop.equal op1 op2 && equal_of_t e1 e2
    | Binop (t1, op1, e1, e3), Binop (t2, op2, e2, e4) ->
      Ty.equal t1 t2 && Ty.Binop.equal op1 op2 && equal_of_t e1 e2
      && equal_of_t e3 e4
    | Relop (t1, op1, e1, e3), Relop (t2, op2, e2, e4) ->
      Ty.equal t1 t2 && Ty.Relop.equal op1 op2 && equal_of_t e1 e2
      && equal_of_t e3 e4
    | Triop (t1, op1, e1, e3, e5), Triop (t2, op2, e2, e4, e6) ->
      Ty.equal t1 t2 && Ty.Triop.equal op1 op2 && equal_of_t e1 e2
      && equal_of_t e3 e4 && equal_of_t e5 e6
    | Cvtop (t1, op1, e1), Cvtop (t2, op2, e2) ->
      Ty.equal t1 t2 && Ty.Cvtop.equal op1 op2 && equal_of_t e1 e2
    | Naryop (t1, op1, l1), Naryop (t2, op2, l2) ->
      Ty.equal t1 t2 && Ty.Naryop.equal op1 op2 && list_eq l1 l2
    | Extract (e1, h1, l1), Extract (e2, h2, l2) ->
      equal_of_t e1 e2 && h1 = h2 && l1 = l2
    | Concat (e1, e3), Concat (e2, e4) -> equal_of_t e1 e2 && equal_of_t e3 e4
    | Binder (binder1, vars1, e1), Binder (binder2, vars2, e2) ->
      Binder.equal binder1 binder2 && list_eq vars1 vars2 && equal_of_t e1 e2
    | ( ( Ptr _ | Symbol _ | List _ | App _ | Unop _ | Binop _ | Triop _
        | Relop _ | Cvtop _ | Naryop _ | Extract _ | Concat _ | Binder _ )
      , _ ) ->
      false

  (* Optimized mixer (DJB2 variant). Inlines to simple arithmetic. *)
  let[@inline] combine h v = (h * 33) + v

  let hash (e : expr) : int =
    match e with
    | Ptr { base; offset } -> combine (Bitvector.hash base) (hash_of_t offset)
    | Symbol s -> Symbol.hash s
    | List l -> List.fold_left (fun acc x -> combine acc (hash_of_t x)) 0 l
    | App (s, es) ->
      let h_s = Symbol.hash s in
      List.fold_left (fun acc x -> combine acc (hash_of_t x)) h_s es
    | Unop (ty, op, e) ->
      let h1 = Ty.hash ty in
      let h2 = combine h1 (Ty.Unop.hash op) in
      combine h2 (hash_of_t e)
    | Binop (ty, op, e1, e2) ->
      let h = Ty.hash ty in
      let h = combine h (Ty.Binop.hash op) in
      let h = combine h (hash_of_t e1) in
      combine h (hash_of_t e2)
    | Triop (ty, op, e1, e2, e3) ->
      let h = Ty.hash ty in
      let h = combine h (Ty.Triop.hash op) in
      let h = combine h (hash_of_t e1) in
      let h = combine h (hash_of_t e2) in
      combine h (hash_of_t e3)
    | Relop (ty, op, e1, e2) ->
      let h = Ty.hash ty in
      let h = combine h (Ty.Relop.hash op) in
      let h = combine h (hash_of_t e1) in
      combine h (hash_of_t e2)
    | Cvtop (ty, op, e) ->
      let h = Ty.hash ty in
      let h = combine h (Ty.Cvtop.hash op) in
      combine h (hash_of_t e)
    | Naryop (ty, op, es) ->
      let h = Ty.hash ty in
      let h = combine h (Ty.Naryop.hash op) in
      List.fold_left (fun acc x -> combine acc (hash_of_t x)) h es
    | Extract (e, hi, lo) ->
      let h = hash_of_t e in
      let h = combine h hi in
      combine h lo
    | Concat (e1, e2) -> combine (hash_of_t e1) (hash_of_t e2)
    | Binder (b, vars, e) ->
      let h = Hashtbl.hash b in
      let h_vars =
        List.fold_left (fun acc x -> combine acc (hash_of_t x)) h vars
      in
      combine h_vars (hash_of_t e)
end

module Hc = Hashcons.Make [@inlined hint] (Expr)

let[@inline] equal (a : t) (b : t) = equal_of_t a b

let[@inline] hash (hte : t) = hash_of_t hte

let[@inline] make e = Sym (Hc.hashcons e)

let[@inline] view hte = hte.Hashcons.node

let[@inline] compare (a : t) (b : t) =
  match (a, b) with
  | Imm a, Imm b -> Value.compare a b
  | Sym a, Sym b -> compare a.tag b.tag
  | Imm _, _ -> -1
  | Sym _, _ -> 1

(** The return type of an expression *)
let rec ty (hte : t) : Ty.t =
  match hte with
  | Imm x -> Value.type_of x
  | Sym x -> begin
    match view x with
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
  end

let rec is_symbolic (v : t) : bool =
  match v with
  | Imm _ -> false
  | Sym x -> begin
    match view x with
    | Symbol _ -> true
    | Ptr { offset; _ } -> is_symbolic offset
    | Unop (_, _, v) | Cvtop (_, _, v) | Extract (v, _, _) | Binder (_, _, v) ->
      is_symbolic v
    | Binop (_, _, v1, v2) | Relop (_, _, v1, v2) | Concat (v1, v2) ->
      is_symbolic v1 || is_symbolic v2
    | Triop (_, _, v1, v2, v3) ->
      is_symbolic v1 || is_symbolic v2 || is_symbolic v3
    | List vs | App (_, vs) | Naryop (_, _, vs) -> List.exists is_symbolic vs
  end

let get_symbols (hte : t list) =
  let tbl = Hashtbl.create 64 in
  let rec symbols (hte : t) =
    match hte with
    | Imm _ -> ()
    | Sym x -> begin
      match view x with
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
    end
  in
  List.iter symbols hte;
  Hashtbl.fold (fun k () acc -> k :: acc) tbl []

let rec pp fmt (hte : t) =
  match hte with
  | Imm v -> Value.pp fmt v
  | Sym hte -> begin
    match view hte with
    | Ptr { base; offset } ->
      Fmt.pf fmt "(Ptr %a %a)" Bitvector.pp base pp offset
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
  end

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

let[@inline] value (v : Value.t) : t = Imm v

let symbol s = make (Symbol s)

let ptr base offset = make (Ptr { base = Bitvector.of_int32 base; offset })

let list l = make (List l)

let app symbol args = make (App (symbol, args))

let[@inline] binder bt vars expr = make (Binder (bt, vars, expr))

let let_in vars body = binder Let_in vars body

let forall vars body = binder Forall vars body

let exists vars body = binder Exists vars body

let[@inline] raw_unop ty op hte = make (Unop (ty, op, hte))

let normalize_eq_or_ne op (ty', e1, e2) =
  let make_relop lhs rhs = Relop (ty', op, lhs, rhs) in
  let ty1, ty2 = (ty e1, ty e2) in
  if not (Ty.equal ty1 ty2) then make_relop e1 e2
  else begin
    match ty1 with
    | Ty_bitv m ->
      let binop = make (Binop (ty1, Sub, e1, e2)) in
      let zero = value (Bitv (Bitvector.make Z.zero m)) in
      make_relop binop zero
    | Ty_int ->
      let binop = make (Binop (ty1, Sub, e1, e2)) in
      let zero = value (Int Int.zero) in
      make_relop binop zero
    | Ty_real ->
      let binop = make (Binop (ty1, Sub, e1, e2)) in
      let zero = value (Real 0.) in
      make_relop binop zero
    | _ -> make_relop e1 e2
  end

let negate_relop (hte : t) : t =
  match hte with
  | Sym { node = Relop (ty, Eq, e1, e2); _ } ->
    make (normalize_eq_or_ne Ne (ty, e1, e2))
  | Sym { node = Relop (ty, Ne, e1, e2); _ } ->
    make (normalize_eq_or_ne Eq (ty, e1, e2))
  | Sym { node = Relop (ty, Lt, e1, e2); _ } -> make (Relop (ty, Le, e2, e1))
  | Sym { node = Relop (ty, LtU, e1, e2); _ } -> make (Relop (ty, LeU, e2, e1))
  | Sym { node = Relop (ty, Le, e1, e2); _ } -> make (Relop (ty, Lt, e2, e1))
  | Sym { node = Relop (ty, LeU, e1, e2); _ } -> make (Relop (ty, LtU, e2, e1))
  | Sym { node = Relop (ty, Gt, e1, e2); _ } -> make (Relop (ty, Le, e1, e2))
  | Sym { node = Relop (ty, GtU, e1, e2); _ } -> make (Relop (ty, LeU, e1, e2))
  | Sym { node = Relop (ty, Ge, e1, e2); _ } -> make (Relop (ty, Lt, e1, e2))
  | Sym { node = Relop (ty, GeU, e1, e2); _ } -> make (Relop (ty, LtU, e1, e2))
  | _ -> Fmt.failwith "negate_relop: not a relop."

let unop ty op e =
  match e with
  | Imm v -> Imm (Eval.unop ty op v)
  | Sym { node; _ } -> begin
    match (op, node) with
    | Not, Unop (_, Not, e') -> e'
    | Neg, Unop (_, Neg, e') -> e'
    | Not, Relop (Ty_fp _, _, _, _) -> raw_unop ty op e
    | Not, Relop _ -> negate_relop e
    | Trim, Cvtop (Ty_real, ToString, _) -> e
    | Head, List (hd :: _) -> hd
    | Tail, List (_ :: tl) -> make (List tl)
    | Reverse, List es -> make (List (List.rev es))
    | Length, List es -> value (Int (List.length es))
    | _ -> raw_unop ty op e
  end

let[@inline] raw_binop ty op hte1 hte2 = make (Binop (ty, op, hte1, hte2))

let rec binop ty op e1 e2 =
  match (e1, e2) with
  | Imm v1, Imm v2 -> value (Eval.binop ty op v1 v2)
  | _ -> begin
    match op with
    | Add -> begin
      match (e1, e2) with
      | Imm (Bitv bv), _ when Bitvector.eqz bv -> e2
      | _, Imm (Bitv bv) when Bitvector.eqz bv -> e1
      (* Ptr Logic: Ptr + x *)
      | Sym { node = Ptr { base; offset }; _ }, e
      | e, Sym { node = Ptr { base; offset }; _ } ->
        let m = Bitvector.numbits base in
        let offset = binop (Ty_bitv m) Add offset e in
        make (Ptr { base; offset })
      (* Normalization: c + x -> x + c *)
      | (Imm _ as value), (Sym { node = Symbol _; _ } as sym)
      | (Sym { node = Symbol _; _ } as sym), (Imm _ as value) ->
        raw_binop ty Add sym value
      (* Associativity: (x + c1) + c2 -> x + (c1 + c2) *)
      | Sym { node = Binop (_, Add, x, Imm v1); _ }, Imm v2
      | Imm v2, Sym { node = Binop (_, Add, x, Imm v1); _ } ->
        let v_sum = Eval.binop ty Add v1 v2 in
        binop ty Add x (Imm v_sum)
      | _ -> raw_binop ty Add e1 e2
    end
    | Sub -> begin
      match (e1, e2) with
      | _, Imm (Bitv bv) when Bitvector.eqz bv -> e1
      (* Ptr - Ptr *)
      | ( Sym { node = Ptr { base = b1; offset = o1 }; _ }
        , Sym { node = Ptr { base = b2; offset = o2 }; _ } ) ->
        if Bitvector.equal b1 b2 then binop ty Sub o1 o2
        else raw_binop ty Sub e1 e2
      (* Ptr - x *)
      | Sym { node = Ptr { base; offset }; _ }, x ->
        let m = Bitvector.numbits base in
        let offset = binop (Ty_bitv m) Sub offset x in
        make (Ptr { base; offset })
      (* x - Ptr *)
      | x, Sym { node = Ptr { base; offset }; _ } ->
        let m = Bitvector.numbits base in
        let base = value (Bitv base) in
        binop ty Sub x (binop (Ty_bitv m) Add base offset)
      (* Associativity: (x - c1) - c2 -> x - (c1 + c2) *)
      | Sym { node = Binop (_, Sub, x, Imm v1); _ }, Imm v2 ->
        let v_sum = Eval.binop ty Add v1 v2 in
        binop ty Sub x (Imm v_sum)
      | _ -> raw_binop ty Sub e1 e2
    end
    | Mul -> begin
      match (e1, e2) with
      | Imm (Bitv bv), _ when Bitvector.eq_one bv -> e2
      | _, Imm (Bitv bv) when Bitvector.eq_one bv -> e1
      | Imm (Bitv bv), _ when Bitvector.eqz bv -> e1
      | _, Imm (Bitv bv) when Bitvector.eqz bv -> e2
      (* Associativity *)
      | Sym { node = Binop (_, Mul, x, Imm v1); _ }, Imm v2
      | Imm v2, Sym { node = Binop (_, Mul, x, Imm v1); _ } ->
        let v_prod = Eval.binop ty Mul v1 v2 in
        binop ty Mul x (Imm v_prod)
      | _ -> raw_binop ty Mul e1 e2
    end
    | Rem -> begin
      match (e1, e2) with
      | Sym { node = Ptr { base; offset }; _ }, _ ->
        let m = Bitvector.numbits base in
        let rhs = value (Bitv base) in
        let addr = binop (Ty_bitv m) Add rhs offset in
        binop ty Rem addr e2
      | Imm (Bitv bv), _ when Bitvector.eqz bv -> e1 (* 0 % x = 0 *)
      | _ -> raw_binop ty Rem e1 e2
    end
    | Or -> begin
      match (e1, e2) with
      | Imm (Bitv bv), _ when Bitvector.eqz bv -> e2
      | _, Imm (Bitv bv) when Bitvector.eqz bv -> e1
      | _ -> raw_binop ty Or e1 e2
    end
    | And -> begin
      match (e1, e2) with
      | Imm (Bitv bv), _ | _, Imm (Bitv bv) ->
        if Bitvector.eqz bv then begin
          (* Soundness guarantee *)
          assert (
            match ty with Ty_bitv m -> m = Bitvector.numbits bv | _ -> false );
          Imm (Bitv bv)
        end
        else raw_binop ty op e1 e2
      | _ -> raw_binop ty op e1 e2
    end
    | Div | DivU | RemU -> begin
      match (e1, e2) with
      | Imm (Bitv bv), _ when Bitvector.eqz bv -> e1
      | _ -> raw_binop ty op e1 e2
    end
    (* Lists *)
    | List_append -> begin
      match (e1, e2) with
      (* Identity *)
      | ( (Sym { node = List []; _ } | Imm (List []))
        , (Sym { node = List _; _ } as sym_list) )
      | ( (Sym { node = List _; _ } as sym_list)
        , (Sym { node = List []; _ } | Imm (List [])) ) ->
        sym_list
      | Sym { node = List symbolic_list; _ }, Imm (List concrete_list) ->
        make (List (symbolic_list @ List.map value concrete_list))
      | Imm (List concrete_list), Sym { node = List symbolic_list; _ } ->
        make (List (List.map value concrete_list @ symbolic_list))
      | Sym { node = List l1; _ }, Sym { node = List l2; _ } ->
        make (List (l1 @ l2))
      | _ -> raw_binop ty op e1 e2
    end
    | At -> begin
      match (e1, e2) with
      | Sym { node = List es; _ }, Imm (Int n) -> (
        (* TODO: use another datastructure? *)
        match List.nth_opt es n with
        | Some v -> v
        | None -> assert false )
      | _ -> raw_binop ty At e1 e2
    end
    | List_cons -> begin
      match (e1, e2) with
      | _, Sym { node = List es; _ } -> make (List (e1 :: es))
      | _ -> raw_binop ty List_cons e1 e2
    end
    | _ -> raw_binop ty op e1 e2
  end

let raw_triop ty op e1 e2 e3 = make (Triop (ty, op, e1, e2, e3)) [@@inline]

let triop ty op e1 e2 e3 =
  match (op, e1, e2, e3) with
  | Ty.Triop.Ite, Imm True, _, _ -> e2
  | Ite, Imm False, _, _ -> e3
  | op, Imm v1, Imm v2, Imm v3 ->
    value (Eval.triop ty op v1 v2 v3)
    (* The Complex Rewrite: Lifting nested ITEs
             Original: if e1 then (if c2 then r1 else r2) else (if ... )
             Target:   if (e1 && c2) then r1 else (if e1 then r2 else e3)
          *)
  | ( Ite
    , _
    , Sym { node = Triop (_, Ite, c2, r1, r2); _ }
    , Sym { node = Triop (_, Ite, _, _, _); _ } ) ->
    let else_part = raw_triop ty Ite e1 r2 e3 in
    let cond = binop Ty_bool And e1 c2 in
    raw_triop ty Ite cond r1 else_part
  | _ -> raw_triop ty op e1 e2 e3

let raw_relop ty op hte1 hte2 = make (Relop (ty, op, hte1, hte2)) [@@inline]

let rec relop ty (op : Ty.Relop.t) e1 e2 =
  (* "zero cost" check upfront *)
  if equal_of_t e1 e2 then
    match ty with
    | Ty.Ty_bool | Ty_bitv _ | Ty_int | Ty_unit -> begin
      match op with
      | Eq | Le | Ge | LeU | GeU -> Imm True
      | Ne | Lt | Gt | LtU | GtU -> Imm False
    end
    | _ -> dispatch_relop ty op e1 e2
  else dispatch_relop ty op e1 e2

and dispatch_relop ty op e1 e2 =
  match (e1, e2) with
  | Imm v1, Imm v2 -> Imm (if Eval.relop ty op v1 v2 then True else False)
  | _ -> begin
    match (op, e1, e2) with
    | Gt, _, _ -> relop ty Lt e2 e1
    | GtU, _, _ -> relop ty LtU e2 e1
    | Ge, _, _ -> relop ty Le e2 e1
    | GeU, _, _ -> relop ty LeU e2 e1
    | Ne, Imm (Real v), _ | Ne, _, Imm (Real v) ->
      if Float.is_nan v || Float.is_infinite v then value True
      else raw_relop ty op e1 e2
    | _, Imm (Real v), _ | _, _, Imm (Real v) ->
      if Float.is_nan v || Float.is_infinite v then value False
      else raw_relop ty op e1 e2
    | Eq, _, Imm Nothing | Eq, Imm Nothing, _ -> value False
    | Ne, _, Imm Nothing | Ne, Imm Nothing, _ -> value True
    | Eq, _, Imm (App (`Op "symbol", [ Str _ ]))
    | Eq, Imm (App (`Op "symbol", [ Str _ ])), _ ->
      value False
    | Ne, _, Imm (App (`Op "symbol", [ Str _ ]))
    | Ne, Imm (App (`Op "symbol", [ Str _ ])), _ ->
      value True
    | ( Eq
      , Sym { node = Symbol ({ ty = Ty_fp prec1; _ } as s1); _ }
      , Sym { node = Symbol ({ ty = Ty_fp prec2; _ } as s2); _ } )
      when prec1 = prec2 && Symbol.equal s1 s2 ->
      raw_unop Ty_bool Not (raw_unop (Ty_fp prec1) Is_nan e1)
    | Eq, Sym { node = Ptr p1; _ }, Sym { node = Ptr p2; _ } ->
      if Bitvector.equal p1.base p2.base then
        relop Ty_bool Eq p1.offset p2.offset
      else value False
    | Ne, Sym { node = Ptr p1; _ }, Sym { node = Ptr p2; _ } ->
      if Bitvector.equal p1.base p2.base then
        relop Ty_bool Ne p1.offset p2.offset
      else value True
    | (LtU | LeU), Sym { node = Ptr p1; _ }, Sym { node = Ptr p2; _ } ->
      if Bitvector.equal p1.base p2.base then relop ty op p1.offset p2.offset
      else
        let b1 = Value.Bitv p1.base in
        let b2 = Value.Bitv p2.base in
        value (if Eval.relop ty op b1 b2 then True else False)
    | ( op
      , Imm (Bitv _ as n)
      , Sym { node = Ptr { base; offset = Imm (Bitv _ as o) }; _ } ) ->
      let base = Eval.binop (Ty_bitv 32) Add (Bitv base) o in
      value (if Eval.relop ty op n base then True else False)
    | ( op
      , Sym { node = Ptr { base; offset = Imm (Bitv _ as o) }; _ }
      , Imm (Bitv _ as n) ) ->
      let base = Eval.binop (Ty_bitv 32) Add (Bitv base) o in
      value (if Eval.relop ty op base n then True else False)
    | op, Sym { node = List l1; _ }, Sym { node = List l2; _ } ->
      relop_list op l1 l2
    | _, _, _ -> raw_relop ty op e1 e2
  end

and relop_list op l1 l2 =
  match op with
  | Ne ->
    (* Implement Ne as !(Eq) *)
    unop Ty_bool Not (relop_list Eq l1 l2)
  | Eq ->
    let rec loop l1 l2 acc =
      match (l1, l2) with
      | [], [] -> acc
      | [], _ | _, [] -> Imm False
      | x :: xs, y :: ys -> (
        let use_real =
          match (ty x, ty y) with Ty_real, Ty_real -> true | _ -> false
        in
        let res = relop (if use_real then Ty_real else Ty_bool) Eq x y in
        match res with
        | Imm False -> Imm False
        | Imm True -> loop xs ys acc
        (* If Symbolic, accumulate it and continue. *)
        | _ -> loop xs ys (binop Ty_bool And acc res) )
    in
    loop l1 l2 (Imm True)
  | _ -> assert false

let raw_cvtop ty op hte = make (Cvtop (ty, op, hte)) [@@inline]

let rec cvtop theory op e =
  match e with
  | Imm v -> value (Eval.cvtop theory op v)
  | Sym hte -> begin
    match (op, view hte) with
    | String_to_float, Cvtop (Ty_real, ToString, hte) -> hte
    | ( Reinterpret_float
      , Cvtop
          (Ty_real, Reinterpret_int, Sym { node = Symbol { ty = Ty_int; _ }; _ })
      ) ->
      e
    | Zero_extend n, Ptr ptr ->
      let offset = cvtop theory op ptr.offset in
      make (Ptr { base = Bitvector.zero_extend n ptr.base; offset })
    | WrapI64, Ptr ptr ->
      let offset = cvtop theory op ptr.offset in
      make (Ptr { base = Bitvector.extract ptr.base ~high:31 ~low:0; offset })
    | WrapI64, Cvtop (Ty_bitv 64, Zero_extend 32, hte) ->
      assert (Ty.equal theory (ty hte) && Ty.equal theory (Ty_bitv 32));
      hte
    | _ -> raw_cvtop theory op e
  end

let raw_naryop ty op es = make (Naryop (ty, op, es)) [@@inline]

let naryop ty op es =
  let rec get_values acc v =
    match v with
    | [] -> Some (List.rev acc)
    | Imm v :: tl -> get_values (v :: acc) tl
    | _ -> None
  in
  match get_values [] es with
  | Some vs -> value (Eval.naryop ty op vs)
  | None -> (
    match (ty, op, es) with
    | ( Ty_str
      , Concat
      , [ Sym { node = Naryop (Ty_str, Concat, l1); _ }
        ; Sym { node = Naryop (Ty_str, Concat, l2); _ }
        ] ) ->
      raw_naryop Ty_str Concat (l1 @ l2)
    | Ty_str, Concat, [ Sym { node = Naryop (Ty_str, Concat, htes); _ }; hte ]
      ->
      raw_naryop Ty_str Concat (htes @ [ hte ])
    | Ty_str, Concat, [ hte; Sym { node = Naryop (Ty_str, Concat, htes); _ } ]
      ->
      raw_naryop Ty_str Concat (hte :: htes)
    | _ -> raw_naryop ty op es )

let[@inline] raw_extract (hte : t) ~(high : int) ~(low : int) : t =
  make (Extract (hte, high, low))

let extract (e : t) ~(high : int) ~(low : int) : t =
  match e with
  | Imm (Bitv bv) ->
    let high = (high * 8) - 1 in
    let low = low * 8 in
    value (Bitv (Bitvector.extract bv ~high ~low))
  | Imm _ -> assert false
  | Sym hte -> begin
    match (view hte, high, low) with
    | ( Cvtop
          ( _
          , (Zero_extend 24 | Sign_extend 24)
          , (Sym { node = Symbol { ty = Ty_bitv 8; _ }; _ } as sym) )
      , 1
      , 0 ) ->
      sym
    | Concat (_, e), h, l when Ty.size (ty e) = h - l -> e
    | Concat (e, _), 8, 4 when Ty.size (ty e) = 4 -> e
    | _ -> if high - low = Ty.size (ty e) then e else raw_extract e ~high ~low
  end

let raw_concat (msb : t) (lsb : t) : t = make (Concat (msb, lsb)) [@@inline]

(* TODO: don't rebuild so many values it generates unecessary hc lookups *)
let rec concat (msb : t) (lsb : t) : t =
  match (msb, lsb) with
  | Imm (Bitv a), Imm (Bitv b) -> value (Bitv (Bitvector.concat a b))
  | Imm (Bitv _), Sym { node = Concat ((Imm (Bitv _) as b), se); _ } ->
    raw_concat (concat msb b) se
  | Sym { node = Extract (s1, h, m1); _ }, Sym { node = Extract (s2, m2, l); _ }
    when equal s1 s2 && m1 = m2 ->
    if h - l = Ty.size (ty s1) then s1 else raw_extract s1 ~high:h ~low:l
  | ( Sym { node = Extract (_, _, _); _ }
    , Sym { node = Concat ((Sym { node = Extract (_, _, _); _ } as e2), e3); _ }
    ) ->
    raw_concat (concat msb e2) e3
  | _ -> raw_concat msb lsb

let rec simplify_expr ?(in_relop = false) (e : t) : t =
  match e with
  | Imm _ -> e
  | Sym hte -> begin
    match view hte with
    | Symbol _ -> e
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
      e
  end

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
    | Imm True -> Some true
    | Imm False -> Some false
    | _ -> None

  let true_ = value True

  let false_ = value False

  let to_val b = if b then true_ else false_

  let v b = to_val b [@@inline]

  let not b =
    match of_val b with
    | Some b -> to_val (not b)
    | None -> (
      match b with
      | Sym { node = Unop (Ty_bool, Not, cond); _ } -> cond
      | _ -> unop Ty_bool Not b )

  let equal b1 b2 =
    match (b1, b2) with
    | Imm True, Imm True | Imm False, Imm False -> true_
    | _ -> relop Ty_bool Eq b1 b2

  let distinct b1 b2 =
    match (b1, b2) with
    | Imm True, Imm False | Imm False, Imm True -> true_
    | _ -> relop Ty_bool Ne b1 b2

  let and_ b1 b2 =
    match (of_val b1, of_val b2) with
    | Some true, _ -> b2
    | _, Some true -> b1
    | Some false, _ | _, Some false -> false_
    | _ -> binop Ty_bool And b1 b2

  let or_ b1 b2 =
    match (of_val b1, of_val b2) with
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
    match hte with
    | Imm v -> Value.Smtlib.pp fmt v
    | Sym hte -> begin
      match view hte with
      | Ptr _ -> assert false
      | Symbol s -> Fmt.pf fmt "@[<hov 1>%a@]" Symbol.pp s
      | List _ -> assert false
      | App _ -> assert false
      | Unop (ty, op, e) ->
        Fmt.pf fmt "@[<hov 1>(%a@ %a)@]" Ty.Smtlib.pp_unop (ty, op) pp e
      | Binop (ty, op, e1, e2) ->
        Fmt.pf fmt "@[<hov 1>(%a@ %a@ %a)@]" Ty.Smtlib.pp_binop (ty, op) pp e1
          pp e2
      | Triop _ -> assert false
      | Relop (ty, op, e1, e2) ->
        Fmt.pf fmt "@[<hov 1>(%a@ %a@ %a)@]" Ty.Smtlib.pp_relop (ty, op) pp e1
          pp e2
      | Cvtop _ -> assert false
      | Naryop _ -> assert false
      | Extract _ -> assert false
      | Concat _ -> assert false
      | Binder _ -> assert false
    end
end

let inline_symbol_values map e =
  let rec aux e =
    match e with
    | Imm _ -> e
    | Sym hte -> begin
      match view hte with
      | Symbol symbol ->
        Option.value ~default:e (Symbol.Map.find_opt symbol map)
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
    end
  in
  aux e

module Key = struct
  type nonrec t = t

  let to_int hte = hash hte

  let compare x y = Prelude.compare (to_int x) (to_int y)
end

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
      match hte with
      | Imm _ -> ()
      | Sym hte -> begin
        match view hte with
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
      end
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
  match e with
  | Sym { node = Binop (Ty_bool, And, e1, e2); _ } ->
    let s1 = split_conjunctions e1 in
    let s2 = split_conjunctions e2 in
    Set.union s1 s2
  | _ -> Set.singleton e

let rec split_disjunctions (e : t) : Set.t =
  match e with
  | Sym { node = Binop (Ty_bool, Or, e1, e2); _ } ->
    let s1 = split_disjunctions e1 in
    let s2 = split_disjunctions e2 in
    Set.union s1 s2
  | _ -> Set.singleton e

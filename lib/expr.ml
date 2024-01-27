open Ty

type t = expr Hc.hash_consed

and expr =
  | Val of Value.t
  | Ptr of int32 * t
  | Unop of Ty.t * unop * t
  | Binop of Ty.t * binop * t * t
  | Triop of Ty.t * triop * t * t * t
  | Relop of Ty.t * relop * t * t
  | Cvtop of Ty.t * cvtop * t
  | Symbol of Symbol.t
  | Extract of t * int * int
  | Concat of t * t

module HashedType : Hashtbl.HashedType with type t = expr = struct
  type t = expr

  let equal (e1 : expr) (e2 : expr) : bool =
    match (e1, e2) with
    | Val v1, Val v2 -> Value.equal v1 v2
    | Ptr (b1, o1), Ptr (b2, o2) -> b1 = b2 && o1 == o2
    | Unop (t1, op1, e1), Unop (t2, op2, e2) ->
      Ty.equal t1 t2 && op1 = op2 && e1 == e2
    | Cvtop (t1, op1, e1), Cvtop (t2, op2, e2) ->
      Ty.equal t1 t2 && op1 = op2 && e1 == e2
    | Binop (t1, op1, e1, e3), Binop (t2, op2, e2, e4) ->
      Ty.equal t1 t2 && op1 = op2 && e1 == e2 && e3 == e4
    | Relop (t1, op1, e1, e3), Relop (t2, op2, e2, e4) ->
      Ty.equal t1 t2 && op1 = op2 && e1 == e2 && e3 == e4
    | Triop (t1, op1, e1, e3, e5), Triop (t2, op2, e2, e4, e6) ->
      Ty.equal t1 t2 && op1 = op2 && e1 == e2 && e3 == e4 && e5 == e6
    | Symbol s1, Symbol s2 -> Symbol.equal s1 s2
    | Extract (e1, h1, l1), Extract (e2, h2, l2) ->
      e1 == e2 && h1 = h2 && l1 = l2
    | Concat (e1, e3), Concat (e2, e4) -> e1 == e2 && e3 == e4
    | _ -> false

  let hash (e : expr) : int =
    let h x = Hashtbl.hash x in
    match e with
    | Val v -> h v
    | Ptr (b, o) -> h (b, o.tag)
    | Unop (ty, op, e) -> h (ty, op, e.tag)
    | Cvtop (ty, op, e) -> h (ty, op, e.tag)
    | Binop (ty, op, e1, e2) -> h (ty, op, e1.tag, e2.tag)
    | Relop (ty, op, e1, e2) -> h (ty, op, e1.tag, e2.tag)
    | Triop (ty, op, e1, e2, e3) -> h (ty, op, e1.tag, e2.tag, e3.tag)
    | Symbol s -> h s
    | Extract (e, hi, lo) -> h (e.tag, hi, lo)
    | Concat (e1, e2) -> h (e1.tag, e2.tag)
end

module Hc = Hc.Make (HashedType)

let hash (e : t) = e.tag

let equal (e1 : t) (e2 : t) = e1.tag == e2.tag

let mk e = Hc.hashcons e

let mk_symbol s = mk (Symbol s)

let ( @: ) e _ = mk e

let is_num (e : t) = match e.node with Val (Num _) -> true | _ -> false

let ty (e : t) : Ty.t =
  match e.node with
  | Val v -> Value.ty v
  | Ptr _ -> Ty_bitv S32
  | Unop (ty, _, _)
  | Cvtop (ty, _, _)
  | Binop (ty, _, _, _)
  | Triop (ty, _, _, _, _)
  | Relop (ty, _, _, _) ->
    ty
  | Symbol s -> Symbol.ty s
  | Extract _ -> Log.err "Expr.ty: Extract: TODO"
  | Concat _ -> Log.err "Expr.ty: Concat: TODO"

let get_symbols (hte : t list) =
  let tbl = Hashtbl.create 64 in
  let rec symbols (hte : t) =
    match hte.node with
    | Val _ -> ()
    | Ptr (_, offset) -> symbols offset
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
    | Symbol s -> Hashtbl.replace tbl s ()
    | Extract (e, _, _) -> symbols e
    | Concat (e1, e2) ->
      symbols e1;
      symbols e2
  in
  List.iter symbols hte;
  Hashtbl.fold (fun k () acc -> k :: acc) tbl []

let negate_relop (hte : t) : (t, string) Result.t =
  let e =
    match hte.node with
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
  Result.map mk e

module Pp = struct
  open Format

  let rec pp fmt (hte : t) =
    match hte.node with
    | Val v -> Value.pp fmt v
    | Ptr (base, offset) -> fprintf fmt "(Ptr (i32 %ld) %a)" base pp offset
    | Unop (ty, op, e) -> fprintf fmt "(%a.%a %a)" Ty.pp ty pp_unop op pp e
    | Binop (ty, op, e1, e2) ->
      fprintf fmt "(%a.%a %a %a)" Ty.pp ty pp_binop op pp e1 pp e2
    | Triop (ty, op, e1, e2, e3) ->
      fprintf fmt "(%a.%a %a %a %a)" Ty.pp ty pp_triop op pp e1 pp e2 pp e3
    | Relop (ty, op, e1, e2) ->
      fprintf fmt "(%a.%a %a %a)" Ty.pp ty pp_relop op pp e1 pp e2
    | Cvtop (ty, op, e) -> fprintf fmt "(%a.%a %a)" Ty.pp ty pp_cvtop op pp e
    | Symbol s -> Symbol.pp fmt s
    | Extract (e, h, l) -> fprintf fmt "(extract %a %d %d)" pp e l h
    | Concat (e1, e2) -> fprintf fmt "(++ %a %a)" pp e1 pp e2

  let pp_list fmt (es : t list) = pp_print_list ~pp_sep:pp_print_space pp fmt es

  let pp_smt fmt (es : t list) : unit =
    let pp_symbols fmt syms =
      pp_print_list ~pp_sep:pp_print_newline
        (fun fmt sym ->
          let ty = Symbol.ty sym in
          fprintf fmt "(declare-fun %a %a)" Symbol.pp sym Ty.pp ty )
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

let rec simplify_binop ty (op : binop) (e1 : t) (e2 : t) =
  match (e1.node, e2.node) with
  | Val (Num n1), Val (Num n2) -> Val (Num (Eval_numeric.eval_binop ty op n1 n2))
  | Ptr (lbase, loff), Ptr (rbase, roff) -> (
    match op with
    | Sub when lbase = rbase -> simplify_binop ty Sub loff roff
    | _ ->
      (* TODO: simplify to i32 here *)
      Binop (ty, op, e1, e2) )
  | Ptr (base, offset), _ -> (
    match op with
    | Add ->
      let new_offset = simplify_binop (Ty_bitv S32) Add offset e2 in
      Ptr (base, mk new_offset)
    | Sub ->
      let new_offset = simplify_binop (Ty_bitv S32) Sub offset e2 in
      Ptr (base, mk new_offset)
    | Rem ->
      let rhs = mk (Val (Num (I32 base))) in
      let addr = mk @@ simplify_binop ty Add rhs offset in
      simplify_binop (Ty_bitv S32) Rem addr e2
    | _ -> Binop (ty, op, e1, e2) )
  | _, Ptr (base, offset) -> (
    match op with
    | Add ->
      let new_offset = simplify_binop (Ty_bitv S32) Add offset e1 in
      Ptr (base, mk new_offset)
    | _ ->
      (* TODO: simplify here *)
      Binop (ty, op, e1, e2) )
  | Val (Num (I32 0l)), _ -> (
    match op with
    | Add | Or -> e2.node
    | And | Div | DivU | Mul | Rem | RemU -> Val (Num (I32 0l))
    | _ -> Binop (ty, op, e1, e2) )
  | _, Val (Num (I32 0l)) -> (
    match op with
    | Add | Or | Sub -> e1.node
    | And | Mul -> Val (Num (I32 0l))
    | _ -> Binop (ty, op, e1, e2) )
  | Binop (ty2, op2, x, { node = Val (Num v1); _ }), Val (Num v2)
    when not (is_num x) -> (
    assert (Ty.equal ty ty2);
    match (op, op2) with
    (* (+ (+ x 1) 2) = (+ x (+ 1 2)) *)
    (* (- (- x 1) 2) = (- x (+ 1 2)) *)
    | Add, Add | Sub, Sub ->
      let v = Eval_numeric.eval_binop ty Add v1 v2 in
      Binop (ty, op, x, mk (Val (Num v)))
    (* (+ (- x 1) 2) = (+ x (- 2 1)) *)
    | Add, Sub ->
      let v = Eval_numeric.eval_binop ty Sub v2 v1 in
      Binop (ty, Add, x, mk (Val (Num v)))
    (* (- (+ x 1) 2) = (+ x (- 1 2)) *)
    | Sub, Add ->
      let v = Eval_numeric.eval_binop ty Sub v1 v2 in
      Binop (ty, Add, x, mk (Val (Num v)))
    | _, _ -> Binop (ty, op, e1, e2) )
  (* FIXME: commenting because this seems wrong? *)
  (* | Binop (_, And, _, _), Val (Num (I32 1l)) -> e1.node *)
  (* | Val (Num (I32 1l)), Binop (_, And, _, _) -> e2.node *)
  | _ -> Binop (ty, op, e1, e2)

let simplify_relop ty (op : relop) (e1 : t) (e2 : t) =
  match (e1.node, e2.node) with
  | Val (Num v1), Val (Num v2) ->
    Val (if Eval_numeric.eval_relop ty op v1 v2 then True else False)
  | Ptr (_, _), Val (Num (I32 0l)) | Val (Num (I32 0l)), Ptr (_, _) -> (
    match op with
    | Eq -> Val False
    | Ne -> Val True
    | _ -> Relop (ty, op, e1, e2) )
  | Ptr (b1, os1), Ptr (b2, os2) -> (
    let v i = mk (Val (Num (I32 i))) in
    match op with
    | Eq -> if b1 = b2 then Relop (ty, Eq, os1, os2) else Val False
    | Ne -> if b1 = b2 then Relop (ty, Ne, os1, os2) else Val True
    | (LtU | LeU | GtU | GeU) as op ->
      if b1 = b2 then Relop (ty, op, os1, os2) else Relop (ty, op, v b1, v b2)
    | _ -> Relop (ty, op, e1, e2) )
  | _ -> Relop (ty, op, e1, e2)

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

let simplify_extract (s : t) h l =
  match s.node with
  | Val (Num (I64 x)) ->
    let x' = nland64 (Int64.shift_right x (l * 8)) (h - l) in
    Val (Num (I64 x'))
  | _ ->
    (* FIXME: *)
    (* if h - l = size s.node then s.node else *)
    Extract (s, h, l)

let simplify_concat (msb : t) (lsb : t) =
  match (msb.node, lsb.node) with
  (* TODO: Add concat of I8s *)
  | ( Extract ({ node = Val (Num (I64 x2)); _ }, h2, l2)
    , Extract ({ node = Val (Num (I64 x1)); _ }, h1, l1) ) ->
    let d1 = h1 - l1 in
    let d2 = h2 - l2 in
    let x1' = nland64 (Int64.shift_right x1 (l1 * 8)) d1 in
    let x2' = nland64 (Int64.shift_right x2 (l2 * 8)) d2 in
    let x = Int64.(logor (shift_left x2' (d1 * 8)) x1') in
    Extract (mk (Val (Num (I64 x))), d1 + d2, 0)
  | ( Extract ({ node = Val (Num (I32 x2)); _ }, h2, l2)
    , Extract ({ node = Val (Num (I32 x1)); _ }, h1, l1) ) ->
    let d1 = h1 - l1 in
    let d2 = h2 - l2 in
    let x1' = nland32 (Int32.shift_right x1 (l1 * 8)) d1 in
    let x2' = nland32 (Int32.shift_right x2 (l2 * 8)) d2 in
    let x = Int32.(logor (shift_left x2' (d1 * 8)) x1') in
    Extract (mk (Val (Num (I32 x))), d1 + d2, 0)
  | Extract (s1, h, m1), Extract (s2, m2, l) when equal s1 s2 && m1 = m2 ->
    Extract (s1, h, l)
  | ( Extract ({ node = Val (Num (I64 x2)); _ }, h2, l2)
    , Concat
        ({ node = Extract ({ node = Val (Num (I64 x1)); _ }, h1, l1); _ }, se) )
    when not (is_num se) ->
    let d1 = h1 - l1 in
    let d2 = h2 - l2 in
    let x1' = nland64 (Int64.shift_right x1 (l1 * 8)) d1 in
    let x2' = nland64 (Int64.shift_right x2 (l2 * 8)) d2 in
    let x = Int64.(logor (shift_left x2' (d1 * 8)) x1') in
    Concat (mk @@ Extract (mk @@ Val (Num (I64 x)), d1 + d2, 0), se)
  | _ -> Concat (msb, lsb)

let rec simplify ?(extract = true) (hte : t) : t =
  match hte.node with
  | Val _ -> hte
  | Ptr (base, offset) -> mk @@ Ptr (base, simplify offset)
  | Binop (ty, op, e1, e2) ->
    let e1 = simplify e1 in
    let e2 = simplify e2 in
    mk @@ simplify_binop ty op e1 e2
  | Relop (ty, op, e1, e2) ->
    let e1 = simplify e1 in
    let e2 = simplify e2 in
    mk @@ simplify_relop ty op e1 e2
  | Extract (_, _, _) when not extract -> hte
  | Extract (s, h, l) when extract -> mk @@ simplify_extract s h l
  | Concat (e1, e2) ->
    let msb = simplify ~extract:false e1 in
    let lsb = simplify ~extract:false e2 in
    mk @@ simplify_concat msb lsb
  | _ -> hte

module Bool = struct
  let v b = mk @@ match b with true -> Val True | false -> Val False

  let not (v : t) =
    mk
    @@
    match v.node with
    | Val True -> Val False
    | Val False -> Val True
    | _ -> Unop (Ty_bool, Not, v)

  let ( = ) (b1 : t) (b2 : t) =
    mk
    @@
    match (b1.node, b2.node) with
    | Val True, Val True | Val False, Val False -> Val True
    | _ -> Relop (Ty_bool, Eq, b1, b2)

  let ( != ) (b1 : t) (b2 : t) =
    mk
    @@
    match (b1.node, b2.node) with
    | Val True, Val False | Val False, Val True -> Val True
    | _ -> Relop (Ty_bool, Ne, b1, b2)

  let ( && ) (b1 : t) (b2 : t) =
    mk
    @@
    match (b1.node, b2.node) with
    | Val True, Val True -> Val True
    | Val False, Val True | Val True, Val False | Val False, Val False ->
      Val False
    | _ -> Binop (Ty_bool, And, b1, b2)

  let ( || ) (b1 : t) (b2 : t) =
    mk
    @@
    match (b1.node, b2.node) with
    | Val False, Val False -> Val False
    | Val True, Val True | Val False, Val True | Val True, Val False -> Val True
    | _ -> Binop (Ty_bool, Or, b1, b2)
end

module Make (T : sig
  type elt

  val ty : Ty.t

  val num : elt -> Num.t
end) =
struct
  let v i = mk @@ Val (Num (T.num i))

  let sym x = mk_symbol Symbol.(x @: T.ty)

  let ( ~- ) e = mk @@ Unop (T.ty, Neg, e)

  let ( = ) e1 e2 = mk @@ Relop (T.ty, Eq, e1, e2)

  let ( != ) e1 e2 = mk @@ Relop (T.ty, Ne, e1, e2)

  let ( > ) e1 e2 = mk @@ Relop (T.ty, Gt, e1, e2)

  let ( >= ) e1 e2 = mk @@ Relop (T.ty, Ge, e1, e2)

  let ( < ) e1 e2 = mk @@ Relop (T.ty, Lt, e1, e2)

  let ( <= ) e1 e2 = mk @@ Relop (T.ty, Le, e1, e2)
end

module Bitv = struct
  module I8 = Make (struct
    type elt = int

    let ty = Ty_bitv S8

    let num i = Num.I8 i
  end)

  module I32 = Make (struct
    type elt = int32

    let ty = Ty_bitv S32

    let num i = Num.I32 i
  end)

  module I64 = Make (struct
    type elt = int64

    let ty = Ty_bitv S64

    let num i = Num.I64 i
  end)
end

module Fpa = struct
  module F32 = Make (struct
    type elt = float

    let ty = Ty_fp S32

    let num f = Num.F32 (Int32.bits_of_float f)
  end)

  module F64 = Make (struct
    type elt = float

    let ty = Ty_fp S64

    let num f = Num.F64 (Int64.bits_of_float f)
  end)
end

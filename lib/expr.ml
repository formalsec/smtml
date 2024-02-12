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
  | Concat of t list

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
    | Concat es1, Concat es2 ->
      if List.length es1 <> List.length es2 then false
      else List.for_all2 (fun ei ej -> ei == ej) es1 es2
    | _ -> false

  let hash (e : expr) : int =
    let open Hc in
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
    | Concat es -> h (List.map (fun e -> e.tag) es)
end

module Hc = Hc.Make (HashedType)

let hash (e : t) = e.tag

let equal (e1 : t) (e2 : t) = e1.tag == e2.tag

let mk e = Hc.hashcons e

let mk_symbol s = mk (Symbol s)

let ( @: ) e _ = mk e

let is_num (e : t) = match e.node with Val (Num _) -> true | _ -> false

let rec ty (e : t) : Ty.t =
  match e.node with
  | Val v -> Value.ty v
  | Ptr _ -> Ty_bitv 32
  | Unop (ty, _, _)
  | Cvtop (ty, _, _)
  | Binop (ty, _, _, _)
  | Triop (ty, _, _, _, _)
  | Relop (ty, _, _, _) ->
    ty
  | Symbol s -> Symbol.ty s
  | Extract (_, h, l) -> Ty_bitv ((h - l) * 8)
  | Concat es ->
    Ty_bitv
      (List.fold_left
         (fun n1 e ->
           match ty e with
           | Ty_bitv n2 -> n1 + n2
           | t -> Log.err "Invalid concat with expr of type '%a'" Ty.pp t )
         0 es )

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
    | Concat es -> List.iter symbols es
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
    | Unop (ty, op, e) -> fprintf fmt "(%a.%a@ %a)" Ty.pp ty pp_unop op pp e
    | Binop (ty, op, e1, e2) ->
      fprintf fmt "@[<hov>(%a.%a@ %a@ %a)@]" Ty.pp ty pp_binop op pp e1 pp e2
    | Triop (ty, op, e1, e2, e3) ->
      fprintf fmt "@[<hov>(%a.%a@ %a@ %a@ %a)@]" Ty.pp ty pp_triop op pp e1 pp
        e2 pp e3
    | Relop (ty, op, e1, e2) ->
      fprintf fmt "@[<hov>(%a.%a@ %a@ %a)@]" Ty.pp ty pp_relop op pp e1 pp e2
    | Cvtop (ty, op, e) ->
      fprintf fmt "@[<hov>(%a.%a %a)@]" Ty.pp ty pp_cvtop op pp e
    | Symbol s -> Symbol.pp fmt s
    | Extract (e, h, l) -> fprintf fmt "(extract %a %d %d)" pp e l h
    | Concat es ->
      fprintf fmt "(++ @[<v>%a@])" (pp_print_list ~pp_sep:pp_print_space pp) es

  let pp_list fmt (es : t list) = pp_print_list ~pp_sep:pp_print_space pp fmt es

  let pp_smt fmt (es : t list) : unit =
    let pp_symbols fmt syms =
      pp_print_list ~pp_sep:pp_print_newline
        (fun fmt sym ->
          let ty = Symbol.ty sym in
          fprintf fmt "@[<hov>(declare-fun@ %a@ %a)@]" Symbol.pp sym Ty.pp ty )
        fmt syms
    in
    let pp_asserts fmt es =
      pp_print_list ~pp_sep:pp_print_newline
        (fun fmt e -> fprintf fmt "(assert @[<v 2>%a@])" pp e)
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

let simplify_unop ty (op : unop) (e : t) : t =
  match e.node with
  | Val (Num n) -> mk @@ Val (Num (Eval_numeric.eval_unop ty op n))
  | _ -> mk @@ Unop (ty, op, e)

let rec simplify_binop ty (op : binop) (e1 : t) (e2 : t) : t =
  match (e1.node, e2.node) with
  | Val (Num n1), Val (Num n2) ->
    mk @@ Val (Num (Eval_numeric.eval_binop ty op n1 n2))
  | Ptr (lbase, loff), Ptr (rbase, roff) -> (
    match op with
    | Sub when lbase = rbase -> simplify_binop ty Sub loff roff
    | _ ->
      (* TODO: simplify to i32 here *)
      mk @@ Binop (ty, op, e1, e2) )
  | Ptr (base, offset), _ -> (
    match op with
    | Add ->
      let new_offset = simplify_binop (Ty_bitv 32) Add offset e2 in
      mk @@ Ptr (base, new_offset)
    | Sub ->
      let new_offset = simplify_binop (Ty_bitv 32) Sub offset e2 in
      mk @@ Ptr (base, new_offset)
    | Rem ->
      let rhs = mk (Val (Num (I32 base))) in
      let addr = simplify_binop ty Add rhs offset in
      simplify_binop (Ty_bitv 32) Rem addr e2
    | _ -> mk @@ Binop (ty, op, e1, e2) )
  | _, Ptr (base, offset) -> (
    match op with
    | Add ->
      let new_offset = simplify_binop (Ty_bitv 32) Add offset e1 in
      mk @@ Ptr (base, new_offset)
    | _ ->
      (* TODO: simplify here *)
      mk @@ Binop (ty, op, e1, e2) )
  | Val (Num (I32 0l)), _ -> (
    match op with
    | Add | Or -> e2
    | And | Div | DivU | Mul | Rem | RemU -> mk @@ Val (Num (I32 0l))
    | _ -> mk @@ Binop (ty, op, e1, e2) )
  | _, Val (Num (I32 0l)) -> (
    match op with
    | Add | Or | Sub -> e1
    | And | Mul -> mk @@ Val (Num (I32 0l))
    | _ -> mk @@ Binop (ty, op, e1, e2) )
  | Binop (ty2, op2, x, { node = Val (Num v1); _ }), Val (Num v2)
    when not (is_num x) -> (
    assert (Ty.equal ty ty2);
    match (op, op2) with
    (* (+ (+ x 1) 2) = (+ x (+ 1 2)) *)
    (* (- (- x 1) 2) = (- x (+ 1 2)) *)
    | Add, Add | Sub, Sub ->
      let v = Eval_numeric.eval_binop ty Add v1 v2 in
      mk @@ Binop (ty, op, x, mk (Val (Num v)))
    (* (+ (- x 1) 2) = (+ x (- 2 1)) *)
    | Add, Sub ->
      let v = Eval_numeric.eval_binop ty Sub v2 v1 in
      mk @@ Binop (ty, Add, x, mk (Val (Num v)))
    (* (- (+ x 1) 2) = (+ x (- 1 2)) *)
    | Sub, Add ->
      let v = Eval_numeric.eval_binop ty Sub v1 v2 in
      mk @@ Binop (ty, Add, x, mk (Val (Num v)))
    | _, _ -> mk @@ Binop (ty, op, e1, e2) )
  (* FIXME: this seems wrong? *)
  (* | Binop (_, And, _, _), Val (Num (I32 1l)) -> e1.node *)
  (* | Val (Num (I32 1l)), Binop (_, And, _, _) -> e2.node *)
  | _ -> mk @@ Binop (ty, op, e1, e2)

let simplify_triop ty (op : triop) (e1 : t) (e2 : t) (e3 : t) : t =
  match op with
  | Ite -> (
    match e1.node with
    | Val True -> e2
    | Val False -> e3
    | _ -> mk @@ Triop (ty, op, e1, e2, e3) )
  | Substr -> (
    match (e1.node, e2.node, e3.node) with
    | Val (Str s), Val (Int i), Val (Int len) ->
      mk @@ Val (Str (String.sub s i len))
    | _ -> mk @@ Triop (ty, op, e1, e2, e3) )

let simplify_relop ty (op : relop) (e1 : t) (e2 : t) : t =
  match (e1.node, e2.node) with
  | Val (Num v1), Val (Num v2) ->
    mk @@ Val (if Eval_numeric.eval_relop ty op v1 v2 then True else False)
  | Ptr (_, _), Val (Num (I32 0l)) | Val (Num (I32 0l)), Ptr (_, _) -> (
    match op with
    | Eq -> mk @@ Val False
    | Ne -> mk @@ Val True
    | _ -> mk @@ Relop (ty, op, e1, e2) )
  | Ptr (b1, os1), Ptr (b2, os2) -> (
    let v i = mk (Val (Num (I32 i))) in
    match op with
    | Eq -> if b1 = b2 then mk @@ Relop (ty, Eq, os1, os2) else mk @@ Val False
    | Ne -> if b1 = b2 then mk @@ Relop (ty, Ne, os1, os2) else mk @@ Val True
    | (LtU | LeU | GtU | GeU) as op ->
      if b1 = b2 then mk @@ Relop (ty, op, os1, os2)
      else mk @@ Relop (ty, op, v b1, v b2)
    | _ -> mk @@ Relop (ty, op, e1, e2) )
  | _ -> mk @@ Relop (ty, op, e1, e2)

let simplify_cvtop ty (op : cvtop) (e : t) =
  match e.node with
  | Val (Num n) -> mk @@ Val (Num (Eval_numeric.eval_cvtop ty op n))
  | _ -> mk @@ Cvtop (ty, op, e)

let nland64 (x : int64) (n : int) =
  let rec loop x' n' acc =
    if n' = 0 then Int64.logand x' acc
    else loop x' (n' - 1) Int64.(logor (shift_left acc 8) 0xffL)
  in
  loop x n 0L

let simplify_extract (s : t) h l =
  match s.node with
  | Val (Num (I64 x)) ->
    let x' = nland64 (Int64.shift_right x (l * 8)) (h - l) in
    mk @@ Val (Num (I64 x'))
  | _ -> if h - l = Ty.size (ty s) then s else mk @@ Extract (s, h, l)

let simplify_concat (es : t list) : t =
  match es with
  | [ x ] -> x
  (* TODO: Find a cleaner way to do this? *)
  | [ { node = Val (Num (I8 i3)); _ }
    ; { node = Val (Num (I8 i2)); _ }
    ; { node = Val (Num (I8 i1)); _ }
    ; { node = Val (Num (I8 i0)); _ }
    ] ->
    let v =
      Int32.(
        logor (shift_left (of_int i1) 8) (of_int i0)
        |> logor (shift_left (of_int i2) 16)
        |> logor (shift_left (of_int i3) 24) )
    in
    mk @@ Val (Num (I32 v))
  | [ { node = Val (Num (I8 i7)); _ }
    ; { node = Val (Num (I8 i6)); _ }
    ; { node = Val (Num (I8 i5)); _ }
    ; { node = Val (Num (I8 i4)); _ }
    ; { node = Val (Num (I8 i3)); _ }
    ; { node = Val (Num (I8 i2)); _ }
    ; { node = Val (Num (I8 i1)); _ }
    ; { node = Val (Num (I8 i0)); _ }
    ] ->
    let v =
      Int64.(
        logor (shift_left (of_int i1) 8) (of_int i0)
        |> logor (shift_left (of_int i2) 16)
        |> logor (shift_left (of_int i3) 24)
        |> logor (shift_left (of_int i4) 32)
        |> logor (shift_left (of_int i5) 40)
        |> logor (shift_left (of_int i6) 48)
        |> logor (shift_left (of_int i7) 56) )
    in
    mk @@ Val (Num (I64 v))
  | { node = Extract (x0, h, _); _ } :: { node = Extract (x1, _, l); _ } :: tl
    when equal x0 x1 ->
    mk @@ Concat ((mk @@ Extract (x0, h, l)) :: tl)
  | _ -> mk @@ Concat es

let rec simplify_expr ?(extract = true) (hte : t) : t =
  match hte.node with
  | Val _ | Symbol _ -> hte
  | Ptr (base, offset) -> mk @@ Ptr (base, simplify_expr offset)
  | Unop (ty, op, e) ->
    let e = simplify_expr e in
    simplify_unop ty op e
  | Binop (ty, op, e1, e2) ->
    let e1 = simplify_expr e1 in
    let e2 = simplify_expr e2 in
    simplify_binop ty op e1 e2
  | Triop (ty, op, e1, e2, e3) ->
    let e1 = simplify_expr e1 in
    let e2 = simplify_expr e2 in
    let e3 = simplify_expr e3 in
    simplify_triop ty op e1 e2 e3
  | Relop (ty, op, e1, e2) ->
    let e1 = simplify_expr e1 in
    let e2 = simplify_expr e2 in
    simplify_relop ty op e1 e2
  | Cvtop (ty, op, e) ->
    let e = simplify_expr e in
    simplify_cvtop ty op e
  | Extract (_, _, _) when not extract -> hte
  | Extract (s, h, l) when extract -> simplify_extract s h l
  | Concat es -> simplify_concat (List.map simplify_expr es)
  | Extract _ -> hte

let simplify (hte : t) : t =
  let rec loop x =
    let simpl_x = simplify_expr x in
    if equal x simpl_x then simpl_x else loop simpl_x
  in
  loop hte

let unop ty (op : unop) (e : t) : t =
  mk
  @@
  match e.node with
  | Val (Num n) -> Val (Num (Eval_numeric.eval_unop ty op n))
  | _ -> Unop (ty, op, e)

let binop ty (op : binop) (e1 : t) (e2 : t) =
  match (e1.node, e2.node) with
  | Val (Num n1), Val (Num n2) ->
    mk @@ Val (Num (Eval_numeric.eval_binop ty op n1 n2))
  | Ptr _, _ | _, Ptr _ ->
    (* Does pointer arithmetic *)
    simplify @@ mk (Binop (Ty_bitv 32, op, e1, e2))
  | _ -> mk @@ Binop (ty, op, e1, e2)

let relop ty (op : relop) (e1 : t) (e2 : t) =
  mk
  @@
  match (e1.node, e2.node) with
  | Val (Num n1), Val (Num n2) ->
    let res = Eval_numeric.eval_relop ty op n1 n2 in
    Val (if res then True else False)
  | Val (Num n), Ptr (b, { node = Val (Num o); _ }) ->
    let base = Eval_numeric.eval_binop (Ty_bitv 32) Add (I32 b) o in
    let res = Eval_numeric.eval_relop ty op n base in
    Val (if res then True else False)
  | Ptr (b, { node = Val (Num o); _ }), Val (Num n) ->
    let base = Eval_numeric.eval_binop (Ty_bitv 32) Add (I32 b) o in
    let res = Eval_numeric.eval_relop ty op base n in
    Val (if res then True else False)
  | _ -> Relop (ty, op, e1, e2)

let cvtop ty (op : cvtop) (e : t) =
  mk
  @@
  match e.node with
  | Val (Num n) -> Val (Num (Eval_numeric.eval_cvtop ty op n))
  | _ -> Cvtop (ty, op, e)

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

module Make_bitv (T : sig
  type elt

  val ty : Ty.t

  val num : elt -> Num.t
end) =
struct
  let v i = mk @@ Val (Num (T.num i))

  let sym x = mk_symbol Symbol.(x @: T.ty)

  let ( ~- ) e = mk @@ Unop (T.ty, Neg, e)

  let clz e = mk @@ Unop (T.ty, Clz, e)

  let add e1 e2 = mk @@ Binop (T.ty, Add, e1, e2)

  let sub e1 e2 = mk @@ Binop (T.ty, Sub, e1, e2)

  let mul e1 e2 = mk @@ Binop (T.ty, Mul, e1, e2)

  let div e1 e2 = mk @@ Binop (T.ty, Div, e1, e2)

  let div_u e1 e2 = mk @@ Binop (T.ty, DivU, e1, e2)

  let rem e1 e2 = mk @@ Binop (T.ty, Rem, e1, e2)

  let rem_u e1 e2 = mk @@ Binop (T.ty, RemU, e1, e2)

  let logand e1 e2 = mk @@ Binop (T.ty, And, e1, e2)

  let logor e1 e2 = mk @@ Binop (T.ty, Or, e1, e2)

  let logxor e1 e2 = mk @@ Binop (T.ty, Xor, e1, e2)

  let shl e1 e2 = mk @@ Binop (T.ty, Shl, e1, e2)

  let shr_s e1 e2 = mk @@ Binop (T.ty, ShrA, e1, e2)

  let shr_u e1 e2 = mk @@ Binop (T.ty, ShrL, e1, e2)

  let rotl e1 e2 = mk @@ Binop (T.ty, Rotl, e1, e2)

  let rotr e1 e2 = mk @@ Binop (T.ty, Rotr, e1, e2)

  let ( = ) e1 e2 = mk @@ Relop (T.ty, Eq, e1, e2)

  let ( != ) e1 e2 = mk @@ Relop (T.ty, Ne, e1, e2)

  let ( > ) e1 e2 = mk @@ Relop (T.ty, Gt, e1, e2)

  let gt_u e1 e2 = mk @@ Relop (T.ty, GtU, e1, e2)

  let ( >= ) e1 e2 = mk @@ Relop (T.ty, Ge, e1, e2)

  let ge_u e1 e2 = mk @@ Relop (T.ty, GeU, e1, e2)

  let ( < ) e1 e2 = mk @@ Relop (T.ty, Lt, e1, e2)

  let lt_u e1 e2 = mk @@ Relop (T.ty, LtU, e1, e2)

  let ( <= ) e1 e2 = mk @@ Relop (T.ty, Le, e1, e2)

  let le_u e1 e2 = mk @@ Relop (T.ty, LeU, e1, e2)

  let bits_of_float e = mk @@ Cvtop (T.ty, Reinterpret_float, e)
end

module Bitv = struct
  module I8 = Make_bitv (struct
    type elt = int

    let ty = Ty_bitv 8

    let num i = Num.I8 i
  end)

  module I32 = Make_bitv (struct
    type elt = int32

    let ty = Ty_bitv 32

    let num i = Num.I32 i
  end)

  module I64 = Make_bitv (struct
    type elt = int64

    let ty = Ty_bitv 64

    let num i = Num.I64 i
  end)
end

module Make_fp (T : sig
  type elt

  val ty : Ty.t

  val num : elt -> Num.t
end) =
struct
  let v i = mk @@ Val (Num (T.num i))

  let sym x = mk_symbol Symbol.(x @: T.ty)

  let abs e = mk @@ Unop (T.ty, Abs, e)

  let ( ~- ) e = mk @@ Unop (T.ty, Neg, e)

  let sqrt e = mk @@ Unop (T.ty, Sqrt, e)

  let ceil e = mk @@ Unop (T.ty, Ceil, e)

  let floor e = mk @@ Unop (T.ty, Floor, e)

  let trunc e = mk @@ Unop (T.ty, Trunc, e)

  let nearest e = mk @@ Unop (T.ty, Nearest, e)

  let add e1 e2 = mk @@ Binop (T.ty, Add, e1, e2)

  let sub e1 e2 = mk @@ Binop (T.ty, Sub, e1, e2)

  let mul e1 e2 = mk @@ Binop (T.ty, Mul, e1, e2)

  let div e1 e2 = mk @@ Binop (T.ty, Div, e1, e2)

  let min e1 e2 = mk @@ Binop (T.ty, Min, e1, e2)

  let max e1 e2 = mk @@ Binop (T.ty, Max, e1, e2)

  let ( = ) e1 e2 = mk @@ Relop (T.ty, Eq, e1, e2)

  let ( != ) e1 e2 = mk @@ Relop (T.ty, Ne, e1, e2)

  let ( > ) e1 e2 = mk @@ Relop (T.ty, Gt, e1, e2)

  let ( >= ) e1 e2 = mk @@ Relop (T.ty, Ge, e1, e2)

  let ( < ) e1 e2 = mk @@ Relop (T.ty, Lt, e1, e2)

  let ( <= ) e1 e2 = mk @@ Relop (T.ty, Le, e1, e2)

  let float_of_bits e = mk @@ Cvtop (T.ty, Reinterpret_int, e)
end

module Fpa = struct
  module F32 = Make_fp (struct
    type elt = float

    let ty = Ty_fp 32

    let num f = Num.F32 (Int32.bits_of_float f)
  end)

  module F64 = Make_fp (struct
    type elt = float

    let ty = Ty_fp 64

    let num f = Num.F64 (Int64.bits_of_float f)
  end)
end

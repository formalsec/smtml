open Ty

module M = struct
  type t =
    { e : expr
    ; ty : Ty.t
    }

  and expr =
    | Val of Value.t
    | Ptr of int32 * t
    | Unop of unop * t
    | Binop of binop * t * t
    | Triop of triop * t * t * t
    | Relop of relop * t * t
    | Cvtop of cvtop * t
    | Symbol of Symbol.t
    | Extract of t * int * int
    | Concat of t * t

  let equal (e1 : t) (e2 : t) : bool =
    if not (e1.ty = e2.ty) then false
    else
      match (e1.e, e2.e) with
      | Val v1, Val v2 -> Value.equal v1 v2
      | Ptr (b1, o1), Ptr (b2, o2) -> b1 = b2 && o1 == o2
      | Unop (op1, e1), Unop (op2, e2) -> op1 = op2 && e1 == e2
      | Cvtop (op1, e1), Cvtop (op2, e2) -> op1 = op2 && e1 == e2
      | Binop (op1, e1, e3), Binop (op2, e2, e4) ->
        op1 = op2 && e1 == e2 && e3 == e4
      | Relop (op1, e1, e3), Relop (op2, e2, e4) ->
        op1 = op2 && e1 == e2 && e3 == e4
      | Triop (op1, e1, e3, e5), Triop (op2, e2, e4, e6) ->
        op1 = op2 && e1 == e2 && e3 == e4 && e5 == e6
      | Symbol s1, Symbol s2 -> Symbol.equal s1 s2
      | Extract (e1, h1, l1), Extract (e2, h2, l2) ->
        e1 == e2 && h1 = h2 && l1 = l2
      | Concat (e1, e3), Concat (e2, e4) -> e1 == e2 && e3 == e4
      | _ -> false

  let hash = Hashtbl.hash
end

include M
module H = Ephemeron.K1.Make (M)

let table = H.create 251

let hashcons x =
  match H.find table x with
  | exception Not_found ->
    H.add table x x;
    x
  | v -> v

let v e ty = hashcons { e; ty }
let ( @: ) e ty = v e ty
let mk_symbol s = Symbol s @: Symbol.type_of s
let is_num e = match e.e with Val (Num _) -> true | _ -> false

let get_symbols e =
  let tbl = Hashtbl.create 64 in
  let rec symbols e =
    match e.e with
    | Val _ -> ()
    | Ptr (_, offset) -> symbols offset
    | Unop (_, e1) -> symbols e1
    | Binop (_, e1, e2) ->
      symbols e1;
      symbols e2
    | Triop (_, e1, e2, e3) ->
      symbols e1;
      symbols e2;
      symbols e3
    | Relop (_, e1, e2) ->
      symbols e1;
      symbols e2
    | Cvtop (_, e) -> symbols e
    | Symbol s -> Hashtbl.replace tbl s ()
    | Extract (e, _, _) -> symbols e
    | Concat (e1, e2) ->
      symbols e1;
      symbols e2
  in
  List.iter symbols e;
  Hashtbl.fold (fun k () acc -> k :: acc) tbl []

let negate_relop ({ e; ty } : t) : (t, string) Result.t =
  let e =
    match e with
    | Relop (Eq, e1, e2) -> Ok (Relop (Ne, e1, e2))
    | Relop (Ne, e1, e2) -> Ok (Relop (Eq, e1, e2))
    | Relop (Lt, e1, e2) -> Ok (Relop (Ge, e1, e2))
    | Relop (LtU, e1, e2) -> Ok (Relop (GeU, e1, e2))
    | Relop (Le, e1, e2) -> Ok (Relop (Gt, e1, e2))
    | Relop (LeU, e1, e2) -> Ok (Relop (GtU, e1, e2))
    | Relop (Gt, e1, e2) -> Ok (Relop (Le, e1, e2))
    | Relop (GtU, e1, e2) -> Ok (Relop (LeU, e1, e2))
    | Relop (Ge, e1, e2) -> Ok (Relop (Lt, e1, e2))
    | Relop (GeU, e1, e2) -> Ok (Relop (LtU, e1, e2))
    | _ -> Error "negate_relop: not a relop."
  in
  Result.map (fun relop -> relop @: ty) e

module Pp = struct
  open Format

  let rec pp fmt ({ e; ty } : t) =
    match e with
    | Val v -> Value.pp fmt v
    | Ptr (base, offset) -> fprintf fmt "(Ptr (i32 %ld) %a)" base pp offset
    | Unop (op, e) -> fprintf fmt "(%a.%a %a)" Ty.pp ty pp_unop op pp e
    | Binop (op, e1, e2) ->
      fprintf fmt "(%a.%a %a %a)" Ty.pp ty pp_binop op pp e1 pp e2
    | Triop (op, e1, e2, e3) ->
      fprintf fmt "(%a.%a %a %a %a)" Ty.pp ty pp_triop op pp e1 pp e2 pp e3
    | Relop (op, e1, e2) ->
      fprintf fmt "(%a.%a %a %a)" Ty.pp ty pp_relop op pp e1 pp e2
    | Cvtop (op, e) -> fprintf fmt "(%a.%a %a)" Ty.pp ty pp_cvtop op pp e
    | Symbol s -> Symbol.pp fmt s
    | Extract (e, h, l) -> fprintf fmt "(extract %a %d %d)" pp e l h
    | Concat (e1, e2) -> fprintf fmt "(++ %a %a)" pp e1 pp e2

  let pp_list fmt (es : t list) = pp_print_list ~pp_sep:pp_print_space pp fmt es

  let pp_smt fmt (es : t list) : unit =
    let pp_symbols fmt syms =
      pp_print_list ~pp_sep:pp_print_newline
        (fun fmt sym ->
          let t = Symbol.type_of sym in
          fprintf fmt "(declare-fun %a %a)" Symbol.pp sym Ty.pp t )
        fmt syms
    in
    let pp_asserts fmt es =
      pp_print_list ~pp_sep:pp_print_newline
        (fun fmt e -> fprintf fmt "(assert @[<h 2>%a@])" pp e)
        fmt es
    in
    let syms = get_symbols es in
    fprintf fmt "%a@\n%a@\n(check-sat)" pp_symbols syms pp_asserts es
end

let pp = Pp.pp
let pp_list = Pp.pp_list
let pp_smt = Pp.pp_smt
let to_string e = Format.asprintf "%a" pp e

let rec simplify_binop ty (op : binop) e1 e2 =
  match (e1.e, e2.e) with
  | Val (Num n1), Val (Num n2) -> Val (Num (Eval_numeric.eval_binop ty op n1 n2))
  | Ptr (b1, os1), Ptr (b2, os2) -> (
    match op with
    | Sub when b1 = b2 -> simplify_binop ty Sub os1 os2
    | _ -> Binop (op, e1, e2) )
  | Ptr (base, offset), _ -> (
    match op with
    | Add ->
      let new_offset = simplify_binop offset.ty Add offset e2 in
      Ptr (base, new_offset @: offset.ty)
    | Sub ->
      let new_offset = simplify_binop offset.ty Sub offset e2 in
      Ptr (base, new_offset @: offset.ty)
    | _ -> Binop (op, e1, e2) )
  | _, Ptr (base, offset) -> (
    match op with
    | Add ->
      let new_offset = simplify_binop offset.ty Add offset e1 in
      Ptr (base, new_offset @: offset.ty)
    | _ -> Binop (op, e1, e2) )
  | Val (Num (I32 0l)), _ -> (
    match op with
    | Add | Or -> e2.e
    | And | Div | DivU | Mul | Rem | RemU -> Val (Num (I32 0l))
    | _ -> Binop (op, e1, e2) )
  | _, Val (Num (I32 0l)) -> (
    match op with
    | Add | Or | Sub -> e1.e
    | And | Mul -> Val (Num (I32 0l))
    | _ -> Binop (op, e1, e2) )
  | Binop (op2, x, { e = Val (Num v1); ty }), Val (Num v2) when not (is_num x)
    -> (
    match (op, op2) with
    | Add, Add ->
      let v = Eval_numeric.eval_binop ty Add v1 v2 in
      Binop (Add, x, Val (Num v) @: ty)
    (* | Add, Sub | Sub, Add -> *)
    (*   let v = Eval_numeric.eval_binop (I32 Sub) v1 v2 in *)
    (*   Binop (I32 Add, x, Val (Num v)) *)
    | Sub, Sub ->
      let v = Eval_numeric.eval_binop ty Add v1 v2 in
      Binop (Sub, x, Val (Num v) @: ty)
    | _, _ -> Binop (op, e1, e2) )
  | Binop (And, _, _), Val (Num (I32 1l)) -> e1.e
  | Val (Num (I32 1l)), Binop (And, _, _) -> e2.e
  | _ -> Binop (op, e1, e2)

let simplify_relop ty (op : relop) e1 e2 =
  match (e1.e, e2.e) with
  | Val (Num v1), Val (Num v2) ->
    Val (if Eval_numeric.eval_relop ty op v1 v2 then True else False)
  | Ptr (_, _), Val (Num (I32 0l)) | Val (Num (I32 0l)), Ptr (_, _) -> (
    match op with Eq -> Val False | Ne -> Val True | _ -> Relop (op, e1, e2) )
  | Ptr (b1, os1), Ptr (b2, os2) -> (
    let v i = Val (Num (I32 i)) @: Ty_bitv S32 in
    match op with
    | Eq -> if b1 = b2 then Relop (Eq, os1, os2) else Val False
    | Ne -> if b1 = b2 then Relop (Ne, os1, os2) else Val True
    | (LtU | LeU | GtU | GeU) as op ->
      if b1 = b2 then Relop (op, os1, os2) else Relop (op, v b1, v b2)
    | _ -> Relop (op, e1, e2) )
  | _ -> Relop (op, e1, e2)

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

let simplify_extract s h l =
  match s.e with
  | Val (Num (I64 x)) ->
    let x' = nland64 (Int64.shift_right x (l * 8)) (h - l) in
    Val (Num (I64 x'))
  | _ -> if h - l = size s.ty then s.e else Extract (s, h, l)

let simplify_concat (msb : t) (lsb : t) =
  match (msb.e, lsb.e) with
  | ( Extract ({ e = Val (Num (I64 x2)); _ }, h2, l2)
    , Extract ({ e = Val (Num (I64 x1)); _ }, h1, l1) ) ->
    let d1 = h1 - l1 in
    let d2 = h2 - l2 in
    let x1' = nland64 (Int64.shift_right x1 (l1 * 8)) d1 in
    let x2' = nland64 (Int64.shift_right x2 (l2 * 8)) d2 in
    let x = Int64.(logor (shift_left x2' (d1 * 8)) x1') in
    Extract (Val (Num (I64 x)) @: msb.ty, d1 + d2, 0)
  | ( Extract ({ e = Val (Num (I32 x2)); _ }, h2, l2)
    , Extract ({ e = Val (Num (I32 x1)); _ }, h1, l1) ) ->
    let d1 = h1 - l1 in
    let d2 = h2 - l2 in
    let x1' = nland32 (Int32.shift_right x1 (l1 * 8)) d1 in
    let x2' = nland32 (Int32.shift_right x2 (l2 * 8)) d2 in
    let x = Int32.(logor (shift_left x2' (d1 * 8)) x1') in
    Extract (Val (Num (I32 x)) @: msb.ty, d1 + d2, 0)
  | Extract (s1, h, m1), Extract (s2, m2, l) when equal s1 s2 && m1 = m2 ->
    Extract (s1, h, l)
  | ( Extract ({ e = Val (Num (I64 x2)); ty }, h2, l2)
    , Concat ({ e = Extract ({ e = Val (Num (I64 x1)); _ }, h1, l1); _ }, se) )
    when not (is_num se) ->
    let d1 = h1 - l1 in
    let d2 = h2 - l2 in
    let x1' = nland64 (Int64.shift_right x1 (l1 * 8)) d1 in
    let x2' = nland64 (Int64.shift_right x2 (l2 * 8)) d2 in
    let x = Int64.(logor (shift_left x2' (d1 * 8)) x1') in
    Concat (Extract (Val (Num (I64 x)) @: ty, d1 + d2, 0) @: ty, se)
  | _ -> Concat (msb, lsb)

let rec simplify ?(extract = true) ({ ty; e } as expr : t) : t =
  match e with
  | Val _ -> expr
  | Ptr (base, offset) -> Ptr (base, simplify offset) @: ty
  | Binop (op, e1, e2) ->
    let e1 = simplify e1 in
    let e2 = simplify e2 in
    simplify_binop ty op e1 e2 @: ty
  | Relop (op, e1, e2) ->
    let e1 = simplify e1 in
    let e2 = simplify e2 in
    simplify_relop ty op e1 e2 @: ty
  | Extract (_, _, _) when not extract -> expr
  | Extract (s, h, l) when extract -> simplify_extract s h l @: ty
  | Concat (e1, e2) ->
    let msb = simplify ~extract:false e1 in
    let lsb = simplify ~extract:false e2 in
    simplify_concat msb lsb @: ty
  | _ -> expr

module Bool = struct
  let v b = (match b with true -> Val True | false -> Val False) @: Ty_bool

  let not v =
    ( match v.e with
    | Val True -> Val False
    | Val False -> Val True
    | _ -> Unop (Not, v) )
    @: Ty_bool

  let ( = ) b1 b2 =
    ( match (b1.e, b2.e) with
    | Val True, Val True | Val False, Val False -> Val True
    | _ -> Relop (Eq, b1, b2) )
    @: Ty_bool

  let ( && ) b1 b2 =
    ( match (b1.e, b2.e) with
    | Val True, Val True -> Val True
    | Val False, Val True | Val True, Val False | Val False, Val False ->
      Val False
    | _ -> Binop (And, b1, b2) )
    @: Ty_bool

  let ( || ) b1 b2 =
    ( match (b1.e, b2.e) with
    | Val False, Val False -> Val False
    | Val True, Val True | Val False, Val True | Val True, Val False -> Val True
    | _ -> Binop (Or, b1, b2) )
    @: Ty_bool
end

module Bitv = struct
  module Make (T : sig
    type elt

    val ty : Ty.t
    val num : elt -> Num.t
  end) =
  struct
    let v i = Val (Num (T.num i)) @: T.ty
    let sym x = mk_symbol Symbol.(x @: T.ty)
    let not e = Unop (Not, e) @: T.ty
    let ( = ) e1 e2 = Relop (Eq, e1, e2) @: T.ty
    let ( > ) e1 e2 = Relop (Gt, e1, e2) @: T.ty
    let ( >= ) e1 e2 = Relop (Ge, e1, e2) @: T.ty
    let ( < ) e1 e2 = Relop (Lt, e1, e2) @: T.ty
    let ( <= ) e1 e2 = Relop (Le, e1, e2) @: T.ty
  end

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

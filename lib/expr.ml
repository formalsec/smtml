open Ty

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

let ( @: ) e ty = { e; ty }
let mk_symbol s = Symbol s @: Symbol.type_of s
let is_num e = match e.e with Val (Num _) -> true | _ -> false

let rec equal (e1 : t) (e2 : t) : bool =
  if not (e1.ty = e2.ty) then false
  else
    match (e1.e, e2.e) with
    | Val v1, Val v2 -> Value.equal v1 v2
    | Ptr (b1, o1), Ptr (b2, o2) -> b1 = b2 && equal o1 o2
    | Unop (op1, e1), Unop (op2, e2) -> op1 = op2 && equal e1 e2
    | Cvtop (op1, e1), Cvtop (op2, e2) -> op1 = op2 && equal e1 e2
    | Binop (op1, e1, e3), Binop (op2, e2, e4) ->
      op1 = op2 && equal e1 e2 && equal e3 e4
    | Relop (op1, e1, e3), Relop (op2, e2, e4) ->
      op1 = op2 && equal e1 e2 && equal e3 e4
    | Triop (op1, e1, e3, e5), Triop (op2, e2, e4, e6) ->
      op1 = op2 && equal e1 e2 && equal e3 e4 && equal e5 e6
    | Symbol s1, Symbol s2 -> Symbol.equal s1 s2
    | Extract (e1, h1, l1), Extract (e2, h2, l2) ->
      equal e1 e2 && h1 = h2 && l1 = l2
    | Concat (e1, e3), Concat (e2, e4) -> equal e1 e2 && equal e3 e4
    | _ -> false

let get_symbols e =
  let tbl = Hashtbl.create 64 in
  let rec symbols e =
    match e.e with
    | Val _ -> ()
    | Ptr (_, offset) -> symbols offset
    | Unop (_, e1) -> symbols e1
    | Binop (_, e1, e2) -> symbols e1; symbols e2
    | Triop (_, e1, e2, e3) -> symbols e1; symbols e2; symbols e3
    | Relop (_, e1, e2) -> symbols e1; symbols e2
    | Cvtop (_, e) -> symbols e
    | Symbol s -> Hashtbl.replace tbl s ()
    | Extract (e, _, _) -> symbols e
    | Concat (e1, e2) -> symbols e1; symbols e2
  in
  List.iter symbols e;
  Hashtbl.fold (fun k () acc -> k::acc) tbl []

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
  Result.map (fun relop -> { ty; e = relop }) e

module Pp = struct
  let rec pp fmt (e : t) =
    let fprintf = Format.fprintf in
    match e.e with
    | Val v -> fprintf fmt "%a" Value.pp v
    | Ptr (base, offset) -> fprintf fmt "(Ptr (i32 %ld) %a)" base pp offset
    | Unop (op, e) -> fprintf fmt "(%a.%a %a)" Ty.pp e.ty pp_unop op pp e
    | Binop (op, e1, e2) ->
      fprintf fmt "(%a.%a %a %a)" Ty.pp e.ty pp_binop op pp e1 pp e2
    | Triop (op, e1, e2, e3) ->
      fprintf fmt "(%a.%a %a %a %a)" Ty.pp e.ty pp_triop op pp e1 pp e2 pp e3
    | Relop (op, e1, e2) ->
      fprintf fmt "(%a.%a %a %a)" Ty.pp e.ty pp_relop op pp e1 pp e2
    | Cvtop (op, e) -> fprintf fmt "(%a.%a %a)" Ty.pp e.ty pp_cvtop op pp e
    | Symbol s -> fprintf fmt "%a" Symbol.pp s
    | Extract (e, h, l) -> fprintf fmt "(extract %a %d %d)" pp e l h
    | Concat (e1, e2) -> fprintf fmt "(++ %a %a)" pp e1 pp e2

  let pp_list fmt (es : t list) =
    Format.pp_print_list ~pp_sep:Format.pp_print_space pp fmt es

  let pp_smt fmt (es : t list) : unit =
    let pp_symbols fmt syms =
      Format.pp_print_list ~pp_sep:Format.pp_print_newline
        (fun fmt sym ->
          let x = Symbol.to_string sym in
          let t = Symbol.type_of sym in
          Format.fprintf fmt "(declare-fun %s %a)" x Ty.pp t )
        fmt syms
    in
    let pp_asserts fmt es =
      Format.pp_print_list ~pp_sep:Format.pp_print_newline
        (fun fmt e -> Format.fprintf fmt "(assert @[<h 2>%a@])" pp e)
        fmt es
    in
    let syms = get_symbols es in
    Format.fprintf fmt "%a@\n%a@\n(check-sat)" pp_symbols syms pp_asserts es
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
      Ptr (base, { e = new_offset; ty = offset.ty })
    | Sub ->
      let new_offset = simplify_binop offset.ty Sub offset e2 in
      Ptr (base, { e = new_offset; ty = offset.ty })
    | _ -> Binop (op, e1, e2) )
  | _, Ptr (base, offset) -> (
    match op with
    | Add ->
      let new_offset = simplify_binop offset.ty Add offset e1 in
      Ptr (base, { e = new_offset; ty = offset.ty })
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
      Binop (Add, x, { e = Val (Num v); ty })
    (* | Add, Sub | Sub, Add -> *)
    (*   let v = Eval_numeric.eval_binop (I32 Sub) v1 v2 in *)
    (*   Binop (I32 Add, x, Val (Num v)) *)
    | Sub, Sub ->
      let v = Eval_numeric.eval_binop ty Add v1 v2 in
      Binop (Sub, x, { e = Val (Num v); ty })
    | _, _ -> Binop (op, e1, e2) )
  | Binop (And, _, _), Val (Num (I32 1l)) -> e1.e
  | Val (Num (I32 1l)), Binop (And, _, _) -> e2.e
  | _ -> Binop (op, e1, e2)

let simplify_relop ty (op : relop) e1 e2 =
  match (e1.e, e2.e) with
  | Val (Num v1), Val (Num v2) ->
    Val (Bool (Eval_numeric.eval_relop ty op v1 v2))
  | Ptr (_, _), Val (Num (I32 0l)) | Val (Num (I32 0l)), Ptr (_, _) -> (
    match op with
    | Eq -> Val (Bool false)
    | Ne -> Val (Bool true)
    | _ -> Relop (op, e1, e2) )
  | Ptr (b1, os1), Ptr (b2, os2) -> (
    let v i = { ty = Ty_bitv S32; e = Val (Num (I32 i)) } in
    match op with
    | Eq -> if b1 = b2 then Relop (Eq, os1, os2) else Val (Bool false)
    | Ne -> if b1 = b2 then Relop (Ne, os1, os2) else Val (Bool false)
    | LtU -> if b1 = b2 then Relop (LtU, os1, os2) else Relop (LtU, v b1, v b2)
    | LeU -> if b1 = b2 then Relop (LeU, os1, os2) else Relop (LeU, v b1, v b2)
    | GtU -> if b1 = b2 then Relop (GtU, os1, os2) else Relop (GtU, v b1, v b2)
    | GeU -> if b1 = b2 then Relop (GeU, os1, os2) else Relop (GeU, v b1, v b2)
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
    Extract ({ e = Val (Num (I64 x)); ty = msb.ty }, d1 + d2, 0)
  | ( Extract ({ e = Val (Num (I32 x2)); _ }, h2, l2)
    , Extract ({ e = Val (Num (I32 x1)); _ }, h1, l1) ) ->
    let d1 = h1 - l1 in
    let d2 = h2 - l2 in
    let x1' = nland32 (Int32.shift_right x1 (l1 * 8)) d1 in
    let x2' = nland32 (Int32.shift_right x2 (l2 * 8)) d2 in
    let x = Int32.(logor (shift_left x2' (d1 * 8)) x1') in
    Extract ({ e = Val (Num (I32 x)); ty = msb.ty }, d1 + d2, 0)
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
    Concat ({ e = Extract ({ e = Val (Num (I64 x)); ty }, d1 + d2, 0); ty }, se)
  | _ -> Concat (msb, lsb)

let rec simplify ?(extract = true) ({ ty; e } as expr : t) : t =
  match e with
  | Val _ -> expr
  | Ptr (base, offset) -> { ty; e = Ptr (base, simplify offset) }
  | Binop (op, e1, e2) ->
    let e1 = simplify e1 in
    let e2 = simplify e2 in
    { ty; e = simplify_binop ty op e1 e2 }
  | Relop (op, e1, e2) ->
    let e1 = simplify e1 in
    let e2 = simplify e2 in
    { ty; e = simplify_relop ty op e1 e2 }
  | Extract (_, _, _) when not extract -> expr
  | Extract (s, h, l) when extract -> { ty; e = simplify_extract s h l }
  | Concat (e1, e2) ->
    let msb = simplify ~extract:false e1 in
    let lsb = simplify ~extract:false e2 in
    { ty; e = simplify_concat msb lsb }
  | _ -> expr

module Infix = struct
  let ( ++ ) e1 e2 = Concat (e1, e2)
end

module Bitv = struct
  let ty_of_cast (type a) (c : a Ty.cast) : Ty.t =
    match c with C32 -> Ty_bitv S32 | C64 -> Ty_bitv S64

  let v (type a) (c : a Ty.cast) (i : a) =
    match c with
    | C32 -> Val (Num (I32 i)) @: Ty_bitv S32
    | C64 -> Val (Num (I64 i)) @: Ty_bitv S64

  let not (c : _ cast) (e : t) = Unop (Not, e) @: ty_of_cast c
end

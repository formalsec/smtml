open Ty

type t = expr Hc.hash_consed

and expr =
  | Val of Value.t
  | Ptr of int32 * t
  | Symbol of Symbol.t
  | Unop of Ty.t * unop * t
  | Binop of Ty.t * binop * t * t
  | Triop of Ty.t * triop * t * t * t
  | Relop of Ty.t * relop * t * t
  | Cvtop of Ty.t * cvtop * t
  | Extract of t * int * int
  | Concat of t * t

module HashedExpr = struct
  type t = expr

  let equal (e1 : expr) (e2 : expr) : bool =
    match (e1, e2) with
    | Val v1, Val v2 -> Value.equal v1 v2
    | Ptr (b1, o1), Ptr (b2, o2) -> b1 = b2 && o1 == o2
    | Symbol s1, Symbol s2 -> Symbol.equal s1 s2
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
    | Extract (e1, h1, l1), Extract (e2, h2, l2) ->
      e1 == e2 && h1 = h2 && l1 = l2
    | Concat (e1, e3), Concat (e2, e4) -> e1 == e2 && e3 == e4
    | _ -> false

  let hash (e : expr) : int =
    let h x = Hashtbl.hash x in
    match e with
    | Val v -> h v
    | Ptr (b, o) -> h (b, o.tag)
    | Symbol s -> h s
    | Unop (ty, op, e) -> h (ty, op, e.tag)
    | Cvtop (ty, op, e) -> h (ty, op, e.tag)
    | Binop (ty, op, e1, e2) -> h (ty, op, e1.tag, e2.tag)
    | Relop (ty, op, e1, e2) -> h (ty, op, e1.tag, e2.tag)
    | Triop (ty, op, e1, e2, e3) -> h (ty, op, e1.tag, e2.tag, e3.tag)
    | Extract (e, hi, lo) -> h (e.tag, hi, lo)
    | Concat (e1, e2) -> h (e1.tag, e2.tag)
end

module Hc = Hc.Make (HashedExpr)

let equal (hte1 : t) (hte2 : t) = hte1.tag == hte2.tag

let hash (hte : t) = hte.tag

let make (e : expr) = Hc.hashcons e

let ( @: ) e _ = make e

let view (hte : t) : expr = hte.node [@@inline]

let mk_symbol s = make (Symbol s)

let is_num (e : t) = match view e with Val (Num _) -> true | _ -> false

let ty (_hte : t) : Ty.t = assert false

let get_symbols (hte : t list) =
  let tbl = Hashtbl.create 64 in
  let rec symbols (hte : t) =
    match view hte with
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

let rec simplify_binop ty (op : binop) (hte1 : t) (hte2 : t) =
  match (view hte1, view hte2) with
  | Val (Num n1), Val (Num n2) -> Val (Num (Eval_numeric.eval_binop ty op n1 n2))
  | Ptr (b1, os1), Ptr (b2, os2) -> (
    match op with
    | Sub when b1 = b2 -> simplify_binop ty Sub os1 os2
    | _ ->
      (* TODO: simplify to i32 here *)
      Binop (ty, op, hte1, hte2) )
  | Ptr (base, offset), _ -> (
    match op with
    | Add ->
      let new_offset = simplify_binop (Ty_bitv 32) Add offset hte2 in
      Ptr (base, make new_offset)
    | Sub ->
      let new_offset = simplify_binop (Ty_bitv 32) Sub offset hte2 in
      Ptr (base, make new_offset)
    | Rem ->
      let rhs = make @@ Val (Value.Num (Num.I32 base)) in
      let addr = make @@ simplify_binop (Ty_bitv 32) Add rhs offset in
      simplify_binop ty Rem addr hte2
    | _ -> Binop (ty, op, hte1, hte2) )
  | _, Ptr (base, offset) -> (
    match op with
    | Add ->
      let new_offset = simplify_binop (Ty_bitv 32) Add offset hte1 in
      Ptr (base, make new_offset)
    | _ -> Binop (ty, op, hte1, hte2) )
  | Val (Num (I32 0l)), _ -> (
    match op with
    | Add | Or -> view hte2
    | And | Div | DivU | Mul | Rem | RemU -> Val (Num (I32 0l))
    | _ -> Binop (ty, op, hte1, hte2) )
  | _, Val (Num (I32 0l)) -> (
    match op with
    | Add | Or | Sub -> view hte1
    | And | Mul -> Val (Num (I32 0l))
    | _ -> Binop (ty, op, hte1, hte2) )
  | Binop (ty, op2, x, { node = Val (Num v1); _ }), Val (Num v2)
    when not (is_num x) -> (
    match (op, op2) with
    | Add, Add ->
      let v = Eval_numeric.eval_binop ty Add v1 v2 in
      Binop (ty, Add, x, make (Val (Num v)))
    (* | Add, Sub | Sub, Add -> *)
    (*   let v = Eval_numeric.eval_binop (I32 Sub) v1 v2 in *)
    (*   Binop (I32 Add, x, Val (Num v)) *)
    | Sub, Sub ->
      let v = Eval_numeric.eval_binop ty Add v1 v2 in
      Binop (ty, Sub, x, make (Val (Num v)))
    | _, _ -> Binop (ty, op, hte1, hte2) )
  (* FIXME: this seems wrong? *)
  (* | Binop (And, _, _), Val (Num (I32 1l)) -> e1.node.e *)
  (* | Val (Num (I32 1l)), Binop (And, _, _) -> e2.node.e *)
  | _ -> Binop (ty, op, hte1, hte2)

let rec simplify_relop ty (op : relop) (hte1 : t) (hte2 : t) =
  match (view hte1, view hte2) with
  | Val (Num v1), Val (Num v2) ->
    Val (if Eval_numeric.eval_relop ty op v1 v2 then True else False)
  | Ptr (_, _), Val (Num (I32 0l)) | Val (Num (I32 0l)), Ptr (_, _) -> (
    match op with
    | Eq -> Val False
    | Ne -> Val True
    | _ -> Relop (ty, op, hte1, hte2) )
  | Ptr (b1, os1), Ptr (b2, os2) -> (
    match op with
    | Eq -> if b1 = b2 then Relop (ty, Eq, os1, os2) else Val False
    | Ne -> if b1 = b2 then Relop (ty, Ne, os1, os2) else Val True
    | (LtU | LeU | GtU | GeU) as op ->
      if b1 = b2 then simplify_relop ty op os1 os2
      else if Eval_numeric.eval_relop ty op (I32 b1) (I32 b2) then Val True
      else Val False
    | _ -> Relop (ty, op, hte1, hte2) )
  | _ -> Relop (ty, op, hte1, hte2)

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

let simplify_extract (hte : t) h l =
  match view hte with
  | Val (Num (I64 x)) ->
    let x' = nland64 (Int64.shift_right x (l * 8)) (h - l) in
    Val (Num (I64 x'))
  | _ -> if h - l = size (ty hte) then view hte else Extract (hte, h, l)

let simplify_concat (msb : t) (lsb : t) =
  match (view msb, view lsb) with
  | ( Extract ({ node = Val (Num (I64 x2)); _ }, h2, l2)
    , Extract ({ node = Val (Num (I64 x1)); _ }, h1, l1) ) ->
    let d1 = h1 - l1 in
    let d2 = h2 - l2 in
    let x1' = nland64 (Int64.shift_right x1 (l1 * 8)) d1 in
    let x2' = nland64 (Int64.shift_right x2 (l2 * 8)) d2 in
    let x = Int64.(logor (shift_left x2' (d1 * 8)) x1') in
    Extract (make @@ Val (Num (I64 x)), d1 + d2, 0)
  | ( Extract ({ node = Val (Num (I32 x2)); _ }, h2, l2)
    , Extract ({ node = Val (Num (I32 x1)); _ }, h1, l1) ) ->
    let d1 = h1 - l1 in
    let d2 = h2 - l2 in
    let x1' = nland32 (Int32.shift_right x1 (l1 * 8)) d1 in
    let x2' = nland32 (Int32.shift_right x2 (l2 * 8)) d2 in
    let x = Int32.(logor (shift_left x2' (d1 * 8)) x1') in
    Extract (make @@ Val (Num (I32 x)), d1 + d2, 0)
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
    Concat (make @@ Extract (make @@ Val (Num (I64 x)), d1 + d2, 0), se)
  | _ -> Concat (msb, lsb)

let rec simplify ?(extract = true) (hte : t) : t =
  match view hte with
  | Val _ -> hte
  | Ptr (base, offset) -> make @@ Ptr (base, simplify offset)
  | Binop (ty, op, e1, e2) ->
    let e1 = simplify e1 in
    let e2 = simplify e2 in
    make @@ simplify_binop ty op e1 e2
  | Relop (ty, op, e1, e2) ->
    let e1 = simplify e1 in
    let e2 = simplify e2 in
    make @@ simplify_relop ty op e1 e2
  | Extract (_, _, _) when not extract -> hte
  | Extract (s, h, l) when extract -> make @@ simplify_extract s h l
  | Concat (e1, e2) ->
    let msb = simplify ~extract:false e1 in
    let lsb = simplify ~extract:false e2 in
    make @@ simplify_concat msb lsb
  | _ -> hte

module Bool = struct
  let v b = make (match b with true -> Val True | false -> Val False)

  let not (b : t) =
    make
      ( match view b with
      | Val True -> Val False
      | Val False -> Val True
      | _ -> Unop (Ty_bool, Not, b) )

  let ( = ) (b1 : t) (b2 : t) =
    make
      ( match (view b1, view b2) with
      | Val True, Val True | Val False, Val False -> Val True
      | _ -> Relop (Ty_bool, Eq, b1, b2) )

  let ( != ) (b1 : t) (b2 : t) =
    make
      ( match (view b1, view b2) with
      | Val True, Val False | Val False, Val True -> Val True
      | _ -> Relop (Ty_bool, Ne, b1, b2) )

  let ( && ) (b1 : t) (b2 : t) =
    make
      ( match (view b1, view b2) with
      | Val True, Val True -> Val True
      | Val False, Val True | Val True, Val False | Val False, Val False ->
        Val False
      | _ -> Binop (Ty_bool, And, b1, b2) )

  let ( || ) (b1 : t) (b2 : t) =
    make
      ( match (view b1, view b2) with
      | Val False, Val False -> Val False
      | Val True, Val True | Val False, Val True | Val True, Val False ->
        Val True
      | _ -> Binop (Ty_bool, Or, b1, b2) )
end

module Make (T : sig
  type elt

  val ty : Ty.t

  val num : elt -> Num.t
end) =
struct
  let v i = Val (Num (T.num i)) @: T.ty

  let sym x = mk_symbol Symbol.(x @: T.ty)

  let ( ~- ) e = Unop (T.ty, Neg, e) @: T.ty

  let ( = ) e1 e2 = Relop (T.ty, Eq, e1, e2) @: T.ty

  let ( != ) e1 e2 = Relop (T.ty, Ne, e1, e2) @: T.ty

  let ( > ) e1 e2 = Relop (T.ty, Gt, e1, e2) @: T.ty

  let ( >= ) e1 e2 = Relop (T.ty, Ge, e1, e2) @: T.ty

  let ( < ) e1 e2 = Relop (T.ty, Lt, e1, e2) @: T.ty

  let ( <= ) e1 e2 = Relop (T.ty, Le, e1, e2) @: T.ty
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
  module F32 = Make (struct
    type elt = float

    let ty = Ty_fp 32

    let num f = Num.F32 (Int32.bits_of_float f)
  end)

  module F64 = Make (struct
    type elt = float

    let ty = Ty_fp 64

    let num f = Num.F64 (Int64.bits_of_float f)
  end)
end

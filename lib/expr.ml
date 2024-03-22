open Ty

type t = expr Hc.hash_consed

and expr =
  | Val of Value.t
  | Ptr of int32 * t
  | Symbol of Symbol.t
  | List of t list
  | Array of t array
  | Tuple of t list
  | App : [> `Op of string ] * t list -> expr
  | Unop of Ty.t * unop * t
  | Binop of Ty.t * binop * t * t
  | Triop of Ty.t * triop * t * t * t
  | Relop of Ty.t * relop * t * t
  | Cvtop of Ty.t * cvtop * t
  | Extract of t * int * int
  | Concat of t * t

module Hc = Hc.Make (struct
  type t = expr

  let list_eq (l1 : 'a list) (l2 : 'a list) : bool =
    if List.compare_lengths l1 l2 = 0 then List.for_all2 ( == ) l1 l2 else false

  let equal (e1 : expr) (e2 : expr) : bool =
    match (e1, e2) with
    | Val v1, Val v2 -> Value.equal v1 v2
    | Ptr (b1, o1), Ptr (b2, o2) -> b1 = b2 && o1 == o2
    | Symbol s1, Symbol s2 -> Symbol.equal s1 s2
    | List l1, List l2 -> list_eq l1 l2
    | Array a1, Array a2 ->
      Array.(length a1 = length a2) && Array.for_all2 ( == ) a1 a2
    | Tuple l1, Tuple l2 -> list_eq l1 l2
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
    | List v -> h v
    | Array es -> h es
    | Tuple es -> h es
    | App (x, es) -> h (x, es)
    | Unop (ty, op, e) -> h (ty, op, e.tag)
    | Cvtop (ty, op, e) -> h (ty, op, e.tag)
    | Binop (ty, op, e1, e2) -> h (ty, op, e1.tag, e2.tag)
    | Relop (ty, op, e1, e2) -> h (ty, op, e1.tag, e2.tag)
    | Triop (ty, op, e1, e2, e3) -> h (ty, op, e1.tag, e2.tag, e3.tag)
    | Extract (e, hi, lo) -> h (e.tag, hi, lo)
    | Concat (e1, e2) -> h (e1.tag, e2.tag)
end)

let equal (hte1 : t) (hte2 : t) = hte1.tag == hte2.tag

let hash (hte : t) = hte.tag

let make (e : expr) = Hc.hashcons e

let ( @: ) e _ = make e

let view (hte : t) : expr = hte.node [@@inline]

let mk_symbol s = make (Symbol s)

let is_num (e : t) = match view e with Val (Num _) -> true | _ -> false

let rec ty (hte : t) : Ty.t =
  match view hte with
  | Val x -> Value.type_of x
  | Ptr _ -> Ty_bitv 32
  | Symbol x -> Symbol.type_of x
  | List _ -> Ty_list
  | Array _ -> Ty_array
  | Tuple _ -> Ty_tuple
  | App _ -> assert false
  | Unop (ty, _, _) -> ty
  | Binop (ty, _, _, _) -> ty
  | Triop (ty, _, _, _, _) -> ty
  | Relop (ty, _, _, _) -> ty
  | Cvtop (ty, _, _) -> ty
  | Extract (_, h, l) -> Ty_bitv ((h - l) * 8)
  | Concat (e1, e2) -> (
    match (ty e1, ty e2) with
    | Ty_bitv n1, Ty_bitv n2 -> Ty_bitv (n1 + n2)
    | t1, t2 -> Log.err "Invalid concat of (%a) with (%a)" Ty.pp t1 Ty.pp t2 )

let rec is_symbolic (v : t) : bool =
  match view v with
  | Val _ -> false
  | Symbol _ -> true
  | Ptr (_, offset) -> is_symbolic offset
  | List vs | Tuple vs -> List.exists is_symbolic vs
  | Array vs -> Array.exists is_symbolic vs
  | App (_, vs) -> List.exists is_symbolic vs
  | Unop (_, _, v) -> is_symbolic v
  | Binop (_, _, v1, v2) -> is_symbolic v1 || is_symbolic v2
  | Triop (_, _, v1, v2, v3) ->
    is_symbolic v1 || is_symbolic v2 || is_symbolic v3
  | Cvtop (_, _, v) -> is_symbolic v
  | Relop (_, _, v1, v2) -> is_symbolic v1 || is_symbolic v2
  | Extract (e, _, _) -> is_symbolic e
  | Concat (e1, e2) -> is_symbolic e1 || is_symbolic e2

let get_symbols (hte : t list) =
  let tbl = Hashtbl.create 64 in
  let rec symbols (hte : t) =
    match view hte with
    | Val _ -> ()
    | Ptr (_, offset) -> symbols offset
    | Symbol s -> Hashtbl.replace tbl s ()
    | List es | Tuple es -> List.iter symbols es
    | Array es -> Array.iter symbols es
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

  let pp_print_array pp_v fmt v =
    let is_first = ref true in
    Array.iter
      (fun v ->
        if !is_first then is_first := false else pp_print_string fmt " ";
        pp_v fmt v )
      v

  let rec pp fmt (hte : t) =
    match view hte with
    | Val v -> Value.pp fmt v
    | Ptr (base, offset) -> fprintf fmt "(Ptr (i32 %ld) %a)" base pp offset
    | Symbol s -> Symbol.pp fmt s
    | List v | Tuple v -> fprintf fmt "(%a)" (pp_print_list pp) v
    | Array v -> fprintf fmt "(%a)" (pp_print_array pp) v
    | App (`Op x, v) -> fprintf fmt "(%s %a)" x (pp_print_list pp) v
    | Unop (ty, op, e) -> fprintf fmt "(%a.%a %a)" Ty.pp ty pp_unop op pp e
    | Binop (ty, op, e1, e2) ->
      fprintf fmt "(%a.%a %a %a)" Ty.pp ty pp_binop op pp e1 pp e2
    | Triop (ty, op, e1, e2, e3) ->
      fprintf fmt "(%a.%a %a %a %a)" Ty.pp ty pp_triop op pp e1 pp e2 pp e3
    | Relop (ty, op, e1, e2) ->
      fprintf fmt "(%a.%a %a %a)" Ty.pp ty pp_relop op pp e1 pp e2
    | Cvtop (ty, op, e) -> fprintf fmt "(%a.%a %a)" Ty.pp ty pp_cvtop op pp e
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

let unop ty (op : unop) (hte : t) : t =
  match view hte with
  | Val ((Int _ | Real _ | Num _) as v) ->
    make (Val (Eval_numeric.eval_unop ty op v))
  | _ -> make (Unop (ty, op, hte))

let rec binop ty (op : binop) (hte1 : t) (hte2 : t) : t =
  match (view hte1, view hte2) with
  | Val (Int _ as v1), Val (Int _ as v2)
  | Val (Real _ as v1), Val (Real _ as v2)
  | Val (Num _ as v1), Val (Num _ as v2) ->
    make (Val (Eval_numeric.eval_binop ty op v1 v2))
  | Ptr (b1, os1), Ptr (b2, os2) -> (
    match op with
    | Sub when b1 = b2 -> binop ty Sub os1 os2
    | _ ->
      (* TODO: simplify to i32 here *)
      make (Binop (ty, op, hte1, hte2)) )
  | Ptr (base, offset), _ -> (
    match op with
    | Add ->
      let new_offset = binop (Ty_bitv 32) Add offset hte2 in
      make (Ptr (base, new_offset))
    | Sub ->
      let new_offset = binop (Ty_bitv 32) Sub offset hte2 in
      make (Ptr (base, new_offset))
    | Rem ->
      let rhs = make (Val (Value.Num (Num.I32 base))) in
      let addr = binop (Ty_bitv 32) Add rhs offset in
      binop ty Rem addr hte2
    | _ -> make (Binop (ty, op, hte1, hte2)) )
  | _, Ptr (base, offset) -> (
    match op with
    | Add -> make (Ptr (base, binop (Ty_bitv 32) Add offset hte1))
    | _ -> make (Binop (ty, op, hte1, hte2)) )
  | Val (Num (I32 0l)), _ -> (
    match op with
    | Add | Or -> hte2
    | And | Div | DivU | Mul | Rem | RemU -> make (Val (Num (I32 0l)))
    | _ -> make (Binop (ty, op, hte1, hte2)) )
  | _, Val (Num (I32 0l)) -> (
    match op with
    | Add | Or | Sub -> hte1
    | And | Mul -> make (Val (Num (I32 0l)))
    | _ -> make (Binop (ty, op, hte1, hte2)) )
  | Binop (ty, op2, x, { node = Val v1; _ }), Val v2 -> (
    match (op, op2) with
    | Add, Add ->
      let v = make (Val (Eval_numeric.eval_binop ty Add v1 v2)) in
      make (Binop (ty, Add, x, v))
    (* | Add, Sub | Sub, Add -> *)
    (*   let v = Eval_numeric.eval_binop (I32 Sub) v1 v2 in *)
    (*   Binop (I32 Add, x, Val (Num v)) *)
    | Sub, Sub ->
      let v = make (Val (Eval_numeric.eval_binop ty Add v1 v2)) in
      make (Binop (ty, Sub, x, v))
    | Mul, Mul ->
      let v = make (Val (Eval_numeric.eval_binop ty Mul v1 v2)) in
      make (Binop (ty, Mul, x, v))
    | _, _ -> make (Binop (ty, op, hte1, hte2)) )
  (* FIXME: this seems wrong? *)
  (* | Binop (_, And, _, _), Val (Num (I32 1l)) -> hte1 *)
  (* | Val (Num (I32 1l)), Binop (_, And, _, _) -> hte2 *)
  | _ -> make (Binop (ty, op, hte1, hte2))

let triop ty (op : triop) (e1 : t) (e2 : t) (e3 : t) : t =
  match op with
  | Ite -> (
    match view e1 with
    | Val True -> e2
    | Val False -> e3
    | _ -> make (Triop (ty, op, e1, e2, e3)) )
  | Seq_extract ->
    make
      ( match (view e1, view e2, view e3) with
      | Val (Str s), Val (Int i), Val (Int len) -> Val (Str (String.sub s i len))
      | _ -> Triop (ty, op, e1, e2, e3) )
  | Seq_replace -> make (Triop (ty, op, e1, e2, e3))
  | Seq_index ->
    make
      ( match (view e1, view e2, view e3) with
      | Val (Str s), Val (Str t), Val (Int i) ->
        let t = String.get t 0 in
        Val (Int (String.index_from s i t))
      | _ -> Triop (ty, op, e1, e2, e3) )

let rec relop ty (op : relop) (hte1 : t) (hte2 : t) : t =
  match (view hte1, view hte2) with
  | Val (Int _ as v1), Val (Int _ as v2)
  | Val (Real _ as v1), Val (Real _ as v2)
  | Val (Num _ as v1), Val (Num _ as v2) ->
    make (Val (if Eval_numeric.eval_relop ty op v1 v2 then True else False))
  | Ptr (b1, os1), Ptr (b2, os2) -> (
    match op with
    | Eq -> make (if b1 = b2 then Relop (ty, Eq, os1, os2) else Val False)
    | Ne -> make (if b1 = b2 then Relop (ty, Ne, os1, os2) else Val True)
    | (LtU | LeU | GtU | GeU) as op ->
      if b1 = b2 then relop ty op os1 os2
      else
        make
          ( if Eval_numeric.eval_relop ty op (Num (I32 b1)) (Num (I32 b2)) then
              Val True
            else Val False )
    | _ -> make (Relop (ty, op, hte1, hte2)) )
  | Val (Num _ as n), Ptr (b, { node = Val (Num _ as o); _ }) ->
    let base = Eval_numeric.eval_binop (Ty_bitv 32) Add (Num (I32 b)) o in
    make (Val (if Eval_numeric.eval_relop ty op n base then True else False))
  | Ptr (b, { node = Val (Num _ as o); _ }), Val (Num _ as n) ->
    let base = Eval_numeric.eval_binop (Ty_bitv 32) Add (Num (I32 b)) o in
    make (Val (if Eval_numeric.eval_relop ty op base n then True else False))
  | _ -> make (Relop (ty, op, hte1, hte2))

let cvtop ty (op : cvtop) (hte : t) : t =
  make
    ( match view hte with
    | Val ((Int _ | Real _ | Num _) as v) ->
      Val (Eval_numeric.eval_cvtop ty op v)
    | _ -> Cvtop (ty, op, hte) )

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

let extract_ (hte : t) ~(high : int) ~(low : int) : t =
  match view hte with
  | Val (Num (I64 x)) ->
    let x' = nland64 (Int64.shift_right x (low * 8)) (high - low) in
    make (Val (Num (I64 x')))
  | _ ->
    if high - low = size (ty hte) then hte else make (Extract (hte, high, low))

let concat ~(msb : t) ~(lsb : t) : t =
  make
    ( match (view msb, view lsb) with
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
          ({ node = Extract ({ node = Val (Num (I64 x1)); _ }, h1, l1); _ }, se)
      )
      when not (is_num se) ->
      let d1 = h1 - l1 in
      let d2 = h2 - l2 in
      let x1' = nland64 (Int64.shift_right x1 (l1 * 8)) d1 in
      let x2' = nland64 (Int64.shift_right x2 (l2 * 8)) d2 in
      let x = Int64.(logor (shift_left x2' (d1 * 8)) x1') in
      Concat (make @@ Extract (make @@ Val (Num (I64 x)), d1 + d2, 0), se)
    | _ -> Concat (msb, lsb) )

let rec simplify_expr ?(extract = true) (hte : t) : t =
  match view hte with
  | Val _ | Symbol _ -> hte
  | Ptr (base, offset) -> make @@ Ptr (base, simplify_expr offset)
  | List es -> make @@ List (List.map simplify_expr es)
  | Array es -> make @@ Array (Array.map simplify_expr es)
  | Tuple es -> make @@ Tuple (List.map simplify_expr es)
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
  | Extract (s, high, low) -> if not extract then hte else extract_ s ~high ~low
  | Concat (e1, e2) ->
    let msb = simplify_expr ~extract:false e1 in
    let lsb = simplify_expr ~extract:false e2 in
    concat ~msb ~lsb

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

  let v b = make (match b with true -> Val True | false -> Val False)

  let not (b : t) =
    match of_val (view b) with
    | Some b -> v (not b)
    | None -> (
      match view b with
      | Unop (Ty_bool, Not, cond) -> cond
      | _ -> make (Unop (Ty_bool, Not, b)) )

  let ( = ) (b1 : t) (b2 : t) =
    make
      ( match (view b1, view b2) with
      | Val True, Val True | Val False, Val False -> Val True
      | _ -> Relop (Ty_bool, Eq, b1, b2) )

  let distinct (b1 : t) (b2 : t) =
    make
      ( match (view b1, view b2) with
      | Val True, Val False | Val False, Val True -> Val True
      | _ -> Relop (Ty_bool, Ne, b1, b2) )

  let and_ (b1 : t) (b2 : t) =
    match (of_val (view b1), of_val (view b2)) with
    | Some b1, Some b2 -> v (b1 && b2)
    | Some true, _ -> b2
    | _, Some true -> b1
    | Some false, _ | _, Some false -> v false
    | _ -> make (Binop (Ty_bool, And, b1, b2))

  let or_ (b1 : t) (b2 : t) =
    match (of_val (view b1), of_val (view b2)) with
    | Some b1, Some b2 -> v (b1 || b2)
    | Some false, _ -> b2
    | _, Some false -> b1
    | Some true, _ | _, Some true -> v true
    | _ -> make (Binop (Ty_bool, Or, b1, b2))

  let ite (c : t) (r1 : t) (r2 : t) = triop Ty_bool Ite c r1 r2
end

module Make (T : sig
  type elt

  val ty : Ty.t

  val num : elt -> Num.t
end) =
struct
  let v i = make (Val (Num (T.num i)))

  let sym x = mk_symbol Symbol.(x @: T.ty)

  let ( ~- ) e = make @@ Unop (T.ty, Neg, e)

  let ( = ) e1 e2 = make @@ Relop (T.ty, Eq, e1, e2)

  let ( != ) e1 e2 = make @@ Relop (T.ty, Ne, e1, e2)

  let ( > ) e1 e2 = make @@ Relop (T.ty, Gt, e1, e2)

  let ( >= ) e1 e2 = make @@ Relop (T.ty, Ge, e1, e2)

  let ( < ) e1 e2 = make @@ Relop (T.ty, Lt, e1, e2)

  let ( <= ) e1 e2 = make @@ Relop (T.ty, Le, e1, e2)
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

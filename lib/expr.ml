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

module Fmt = struct
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

  let pp_query fmt (es : t list) : unit =
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

let pp = Fmt.pp
let pp_list = Fmt.pp_list
let pp_query = Fmt.pp_query
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
    Val (if Eval_numeric.eval_relop ty op v1 v2 then True else False)
  | Ptr (_, _), Val (Num (I32 0l)) | Val (Num (I32 0l)), Ptr (_, _) -> (
    match op with Eq -> Val False | Ne -> Val True | _ -> Relop (op, e1, e2) )
  | Ptr (b1, os1), Ptr (b2, os2) -> (
    let v i = { ty = Ty_bitv S32; e = Val (Num (I32 i)) } in
    match op with
    | Eq -> if b1 = b2 then Relop (Eq, os1, os2) else Val False
    | Ne -> if b1 = b2 then Relop (Ne, os1, os2) else Val True
    | (LtU | LeU | GtU | GeU) as op ->
      if b1 = b2 then Relop (op, os1, os2) else Relop (op, v b1, v b2)
    | _ -> Relop (op, e1, e2) )
  | _ -> Relop (op, e1, e2)

let simplify_cvtop ty (op : cvtop) e =
  match (op, e.e) with
  | Reinterpret_int, Cvtop (Reinterpret_float, e') -> e'
  | _ -> Cvtop (op, e) @: ty

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
  | Ptr (base, offset) -> Ptr (base, simplify offset) @: ty
  | Binop (op, e1, e2) ->
    let e1 = simplify e1 in
    let e2 = simplify e2 in
    simplify_binop ty op e1 e2 @: ty
  | Relop (op, e1, e2) ->
    let e1 = simplify e1 in
    let e2 = simplify e2 in
    simplify_relop ty op e1 e2 @: ty
  | Cvtop (op, e) ->
    let e = simplify e in
    simplify_cvtop ty op e
  | Extract (_, _, _) when not extract -> expr
  | Extract (s, h, l) when extract -> simplify_extract s h l @: ty
  | Concat (e1, e2) ->
    let msb = simplify ~extract:false e1 in
    let lsb = simplify ~extract:false e2 in
    simplify_concat msb lsb @: ty
  | _ -> expr

(** rewrites in a more SMT like compatible term *)
let rec rewrite { e; ty } : t =
  match e with
  | (Val _ | Ptr _ | Symbol _) as v -> v @: ty
  | Unop (op, e) -> Unop (op, rewrite e) @: ty
  | Binop (op, e1, e2) -> Binop (op, rewrite e1, rewrite e2) @: ty
  | Triop (op, e1, e2, e3) ->
    Triop (op, rewrite e1, rewrite e2, rewrite e3) @: ty
  | Relop (Ne, e1, e2) ->
    let e1 = rewrite e1 in
    let e2 = rewrite e2 in
    Unop (Not, Relop (Eq, e1, e2) @: ty) @: Ty_bool
  | Relop (op, e1, e2) -> Relop (op, rewrite e1, rewrite e2) @: ty
  | Cvtop (ToBool, e) ->
    let e' = rewrite e in
    let zero = Val (Value.default e'.ty) @: e'.ty in
    Unop (Not, Relop (Eq, e', zero) @: e'.ty) @: Ty_bool
  | Cvtop (OfBool, e) ->
    (* This rewrite only happens for i32.of_bool so @return i32 *)
    let e' = rewrite e in
    let one = Val (Num (I32 1l)) @: Ty_bitv S32 in
    let zero = Val (Num (I32 0l)) @: Ty_bitv S32 in
    Triop (Ite, e', one, zero) @: Ty_bool
  | Cvtop (op, e) -> Cvtop (op, rewrite e) @: ty
  | Extract (e, h, l) -> Extract (rewrite e, h, l) @: ty
  | Concat (e1, e2) -> Concat (rewrite e1, rewrite e2) @: ty

module Infix = struct
  let ( ++ ) e1 e2 = Concat (e1, e2)
end

module Bitv_ = struct
  let ty_of_cast (type a) (c : a Ty.cast) : Ty.t =
    match c with C32 -> Ty_bitv S32 | C64 -> Ty_bitv S64

  let v (type a) (c : a Ty.cast) (i : a) =
    match c with
    | C32 -> Val (Num (I32 i)) @: Ty_bitv S32
    | C64 -> Val (Num (I64 i)) @: Ty_bitv S64

  let not (c : _ cast) (e : t) = Unop (Not, e) @: ty_of_cast c
end

module Smtlib = struct
  open Smtlib

  let to_sort : Ty.t -> sort = function
    | Ty.Ty_int -> Sort (Sym "Int")
    | Ty.Ty_real -> Sort (Sym "Real")
    | Ty.Ty_bool -> Sort (Sym "Bool")
    | Ty.Ty_str -> Sort (Sym "String")
    | Ty.Ty_bitv S8 -> Sort (Hole ("BitVec", [ I 8 ]))
    | Ty.Ty_bitv S32 -> Sort (Hole ("BitVec", [ I 32 ]))
    | Ty.Ty_bitv S64 -> Sort (Hole ("BitVec", [ I 64 ]))
    | Ty.Ty_fp S32 -> Sort (Sym "Float32")
    | Ty.Ty_fp S64 -> Sort (Sym "Float64")
    | Ty.Ty_fp S8 -> assert false

  let to_type : sort -> Ty.t = function
    | Sort (Sym "Int") -> Ty.Ty_int
    | Sort (Sym "Real") -> Ty.Ty_real
    | Sort (Sym "Bool") -> Ty.Ty_bool
    | Sort (Sym "String") -> Ty.Ty_str
    | Sort (Sym "Float32") -> Ty.Ty_fp S32
    | Sort (Sym "Float64") -> Ty.Ty_fp S64
    | Sort (Hole ("BitVec", [ I 8 ])) -> Ty.Ty_bitv S8
    | Sort (Hole ("BitVec", [ I 32 ])) -> Ty.Ty_bitv S32
    | Sort (Hole ("BitVec", [ I 64 ])) -> Ty.Ty_bitv S64
    | s ->
      Format.kasprintf failwith {|Unsupported sort "%a"|} Smtlib.Fmt.pp_sort s

  let to_const v =
    let open Value in
    let pp = Bitv.M.print in
    let asprintf = Format.asprintf in
    match v with
    | True -> Id (Plain (Sym "true"))
    | False -> Id (Plain (Sym "false"))
    | Int x -> Const (Num x)
    | Real x -> Const (Dec x)
    | Str x -> Const (Str x)
    | Num (I8 x) ->
      (* Prefer more readable format with identifiers *)
      if x >= 0 then Id (Plain (Hole ("bv" ^ string_of_int x, [ I 8 ])))
      else Const (Hex (asprintf "#x%x" x))
    | Num (I32 x) ->
      if x >= 0l then Id (Plain (Hole ("bv" ^ Int32.to_string x, [ I 32 ])))
      else Const (Hex (asprintf "#x%08lx" x))
    | Num (I64 x) ->
      if x >= 0L then Id (Plain (Hole ("bv" ^ Int64.to_string x, [ I 64 ])))
      else Const (Hex (asprintf "#x%016Lx" x))
    | Num (F32 x) ->
      let bitv = Bitv.of_int32_s x in
      let sign = Bin (asprintf "#b%d" (Bool.to_int @@ Bitv.get bitv 31)) in
      let exponent = Bin (asprintf "#b%a" pp (Bitv.sub bitv 23 8)) in
      let significand = Bin (asprintf "#b%a" pp (Bitv.sub bitv 0 23)) in
      App (Plain (Sym "fp"), [ Const sign; Const exponent; Const significand ])
    | Num (F64 x) ->
      let bitv = Bitv.of_int64_s x in
      let sign = Bin (asprintf "#b%d" (Bool.to_int @@ Bitv.get bitv 63)) in
      let exponent = Bin (asprintf "#b%a" pp (Bitv.sub bitv 52 11)) in
      let significand = Bin (asprintf "#b%a" pp (Bitv.sub bitv 0 52)) in
      App (Plain (Sym "fp"), [ Const sign; Const exponent; Const significand ])

  let id_of_unop ty op : qual_identifier =
    let open Ty in
    let arith_unop = function Neg -> Plain (Sym "-") | _ -> assert false in
    let core_unop = function Not -> Plain (Sym "not") | _ -> assert false in
    let str_unop = function
      | Len -> Plain (Sym "str.len")
      | _ -> assert false
    in
    let bitv_unop = function
      | Not -> Plain (Sym "bvnot")
      | Neg -> Plain (Sym "bvneg")
      | _ -> assert false
    in
    let fp_unop = function
      | Neg -> Plain (Sym "fp.neg")
      | Abs -> Plain (Sym "fp.abs")
      | _ -> assert false
    in
    match ty with
    | Ty_int | Ty_real -> arith_unop op
    | Ty_bool -> core_unop op
    | Ty_str -> str_unop op
    | Ty_bitv _ -> bitv_unop op
    | Ty_fp _ -> fp_unop op

  let id_of_binop ty op : qual_identifier * qual_identifier list =
    let open Ty in
    let int_binop = function
      | Add -> Plain (Sym "+")
      | Sub -> Plain (Sym "-")
      | Mul -> Plain (Sym "*")
      | Div -> Plain (Sym "div")
      | Rem -> Plain (Sym "mod")
      | _ -> assert false
    in
    let real_binop = function
      | Add -> Plain (Sym "+")
      | Sub -> Plain (Sym "-")
      | Mul -> Plain (Sym "*")
      | Div -> Plain (Sym "/")
      | _ -> assert false
    in
    let core_binop = function
      | And -> Plain (Sym "and")
      | Or -> Plain (Sym "or")
      | Xor -> Plain (Sym "xor")
      | _ -> assert false
    in
    let bitv_binop = function
      | Add -> Plain (Sym "bvadd")
      | Sub -> Plain (Sym "bvsub")
      | Mul -> Plain (Sym "bvmul")
      | Div -> Plain (Sym "bvsdiv")
      | DivU -> Plain (Sym "bvudiv")
      | And -> Plain (Sym "bvand")
      | Xor -> Plain (Sym "bvxor")
      | Or -> Plain (Sym "bvor")
      | Shl -> Plain (Sym "bvshl")
      | ShrA -> Plain (Sym "bvashr")
      | ShrL -> Plain (Sym "bvlshr")
      | Rem -> Plain (Sym "bvsrem")
      | RemU -> Plain (Sym "bvurem")
      | _ -> assert false
    in
    let fp_binop = function
      | Add -> (Plain (Sym "fp.add"), [ Plain (Sym "RNE") ])
      | Sub -> (Plain (Sym "fp.sub"), [ Plain (Sym "RNE") ])
      | Mul -> (Plain (Sym "fp.mul"), [ Plain (Sym "RNE") ])
      | Div -> (Plain (Sym "fp.div"), [ Plain (Sym "RNE") ])
      | Min -> (Plain (Sym "fp.min"), [])
      | Max -> (Plain (Sym "fp.max"), [])
      | Rem -> (Plain (Sym "fp.rem"), [])
      | _ -> assert false
    in
    match ty with
    | Ty_int -> (int_binop op, [])
    | Ty_real -> (real_binop op, [])
    | Ty_bool -> (core_binop op, [])
    | Ty_str -> assert false
    | Ty_bitv _ -> (bitv_binop op, [])
    | Ty_fp _ -> fp_binop op

  let id_of_triop _ty op : qual_identifier =
    match op with Ty.Ite -> Plain (Sym "ite") | _ -> assert false

  let id_of_relop ty op : qual_identifier =
    let open Ty in
    let arith_relop = function
      | Eq -> Plain (Sym "=")
      | Le -> Plain (Sym "<=")
      | Lt -> Plain (Sym "<")
      | Ge -> Plain (Sym ">=")
      | Gt -> Plain (Sym ">")
      | _ -> assert false
    in
    let core_relop = function Eq -> Plain (Sym "=") | _ -> assert false in
    let bitv_relop = function
      | Eq -> Plain (Sym "=")
      | Lt -> Plain (Sym "bvslt")
      | LtU -> Plain (Sym "bvult")
      | Le -> Plain (Sym "bvsle")
      | LeU -> Plain (Sym "bvule")
      | Gt -> Plain (Sym "bvsgt")
      | GtU -> Plain (Sym "bvugt")
      | Ge -> Plain (Sym "bvsge")
      | GeU -> Plain (Sym "bvuge")
      | Ne -> assert false
    in
    let fp_relop = function
      | Eq -> Plain (Sym "fp.eq")
      | Lt -> Plain (Sym "fp.lt")
      | Le -> Plain (Sym "fp.leq")
      | Gt -> Plain (Sym "fp.gt")
      | Ge -> Plain (Sym "fp.geq")
      | _ -> assert false
    in
    match ty with
    | Ty_int | Ty_real -> arith_relop op
    | Ty_bool -> core_relop op
    | Ty_bitv _ -> bitv_relop op
    | Ty_str -> assert false
    | Ty_fp _ -> fp_relop op

  let id_of_cvtop ty op : qual_identifier * qual_identifier list =
    let open Ty in
    match op with
    | ExtS n -> (Plain (Hole ("sign_extend", [ I n ])), [])
    | ExtU n -> (Plain (Hole ("zero_extend", [ I n ])), [])
    | WrapI64 -> (Plain (Hole ("extract", [ I 31; I 0 ])), [])
    | DemoteF64 -> (Plain (Hole ("to_fp", [ I 8; I 24 ])), [ Plain (Sym "RNE") ])
    | PromoteF32 ->
      (Plain (Hole ("to_fp", [ I 11; I 53 ])), [ Plain (Sym "RNE") ])
    | Reinterpret_float -> (Plain (Sym "fp.to_ieee_bv"), [])
    | Reinterpret_int | ConvertSI32 | ConvertSI64 ->
      let eb, sb =
        match ty with
        | Ty_fp S32 -> (8, 24)
        | Ty_fp S64 -> (11, 53)
        | _ -> assert false
      in
      (Plain (Hole ("to_fp", [ I eb; I sb ])), [])
    | TruncSF32 | TruncSF64 ->
      let m =
        match ty with
        | Ty_bitv S32 -> 32
        | Ty_bitv S64 -> 64
        | _ -> assert false
      in
      (Plain (Hole ("fp.to_sbv", [ I m ])), [ Plain (Sym "RTZ") ])
    | TruncUF32 | TruncUF64 ->
      let m =
        match ty with
        | Ty_bitv S32 -> 32
        | Ty_bitv S64 -> 64
        | _ -> assert false
      in
      (Plain (Hole ("fp.to_ubv", [ I m ])), [ Plain (Sym "RTZ") ])
    | _ -> assert false

  let rec to_term ({ e; ty } : t) : term =
    match e with
    | Val v -> to_const v
    | Ptr (base, offset) ->
      let tb = to_const (Num (I32 base)) in
      let t = to_term offset in
      App (Plain (Sym "bvadd"), [ tb; t ])
    | Unop (op, e) ->
      let id = id_of_unop ty op in
      let t = to_term e in
      App (id, [ t ])
    | Binop (op, e1, e2) ->
      let id, args = id_of_binop ty op in
      let t1 = to_term e1 in
      let t2 = to_term e2 in
      App (id, List.map (fun arg -> Id arg) args @ [ t1; t2 ])
    | Triop (op, e1, e2, e3) ->
      let id = id_of_triop ty op in
      let t1 = to_term e1 in
      let t2 = to_term e2 in
      let t3 = to_term e3 in
      App (id, [ t1; t2; t3 ])
    | Relop (op, e1, e2) ->
      let id = id_of_relop ty op in
      let t1 = to_term e1 in
      let t2 = to_term e2 in
      App (id, [ t1; t2 ])
    | Cvtop (op, e) ->
      let id, args = id_of_cvtop ty op in
      let t = to_term e in
      App (id, List.map (fun arg -> Id arg) args @ [ t ])
    | Symbol x -> Id (Plain (Sym (Symbol.to_string x)))
    | Extract (e, h, l) ->
      let t = to_term e in
      App (Plain (Hole ("extract", [ I ((h * 8) - 1); I (l * 8) ])), [ t ])
    | Concat (e1, e2) ->
      let t1 = to_term e1 in
      let t2 = to_term e2 in
      App (Plain (Sym "concat"), [ t1; t2 ])

  let to_expr (_t : term) : t = assert false

  (* TODO: This can be improved *)
  let to_script es =
    let consts =
      get_symbols es
      |> List.map (fun s ->
             Declare_const (Symbol.to_string s, Symbol.type_of s |> to_sort) )
    in
    consts
    @ List.map (fun e -> Assert (to_term @@ rewrite e)) es
    @ [ Check_sat ]
end

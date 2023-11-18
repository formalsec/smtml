open Ty

exception InvalidRelop

type qt =
  | Forall
  | Exists

type expr =
  | Val of Value.t
  | Ptr of int32 * expr
  | Unop of unop * expr
  | Binop of binop * expr * expr
  | Relop of relop * expr * expr
  | Cvtop of cvtop * expr
  | Triop of triop * expr * expr * expr
  | Symbol of Symbol.t
  | Extract of expr * int * int
  | Concat of expr * expr
  | Quantifier of qt * Symbol.t list * expr * expr list list

type t = expr

let ( ++ ) (e1 : expr) (e2 : expr) = Concat (e1, e2)
let mk_symbol (s : Symbol.t) = Symbol s
let mk_symbol_s (t : Ty.t) (x : string) : expr = Symbol (Symbol.mk_symbol t x)
let is_num (e : expr) : bool = match e with Val (Num _) -> true | _ -> false
let is_val (e : expr) : bool = match e with Val _ -> true | _ -> false
let is_unop (e : expr) : bool = match e with Unop _ -> true | _ -> false
let is_relop (e : expr) : bool = match e with Relop _ -> true | _ -> false
let is_binop (e : expr) : bool = match e with Binop _ -> true | _ -> false
let is_cvtop (e : expr) : bool = match e with Cvtop _ -> true | _ -> false
let is_triop (e : expr) : bool = match e with Triop _ -> true | _ -> false

let is_concrete (e : expr) : bool =
  match e with Val _ | Ptr (_, Val _) -> true | _ -> false

(* let rename_symbols (es : expr list) : expr list = *)
(*   let count = ref 0 *)
(*   and map = Hashtbl.create 0 in *)
(*   let rec rename (e : expr) : expr = *)
(*     match e with *)
(*     | Val _ -> e *)
(*     | SymPtr (i, offset) -> SymPtr (i, rename offset) *)
(*     | Unop (op, e) -> Unop (op, rename e) *)
(*     | Binop (op, e1, e2) -> Binop (op, rename e1, rename e2) *)
(*     | Triop (op, e1, e2, e3) -> Triop (op, rename e1, rename e2, rename e3) *)
(*     | Relop (op, e1, e2) -> Relop (op, rename e1, rename e2) *)
(*     | Cvtop (op, e) -> Cvtop (op, rename e) *)
(*     | Symbol s -> *)
(*       let old_name = Symbol.to_string s in *)
(*       let new_name = *)
(*         if Hashtbl.mem map old_name then Hashtbl.find map old_name *)
(*         else *)
(*           let x = "x" ^ Int.to_string !count in *)
(*           Hashtbl.replace map old_name x; *)
(*           count := !count + 1; *)
(*           x *)
(*       in *)
(*       Symbol (Symbol.rename s new_name) *)
(*     | Extract (e, h, l) -> Extract (rename e, h, l) *)
(*     | Concat (e1, e2) -> Concat (rename e1, rename e2) *)
(*     | Quantifier (qt, vars, e, es) -> *)
(*       Quantifier (qt, vars, rename e, List.map (List.map rename) es) *)
(*   in *)
(*   List.map rename es *)

let type_of (e : expr) : Ty.t option =
  match e with
  | Val v -> Some (Value.type_of v)
  | Ptr _ -> Some (Ty_bitv S32)
  | Binop (op, _, _) -> Some (Ty.type_of op)
  | Triop (op, _, _, _) -> Some (Ty.type_of op)
  | Unop (op, _) -> Some (Ty.type_of op)
  | Relop (op, _, _) -> Some (Ty.type_of op)
  | Cvtop (op, _) -> Some (Ty.type_of op)
  | Symbol s -> Some (Symbol.type_of s)
  | Extract (_, h, l) -> (
    match h - l with
    | 4 -> Some (Ty_bitv S32)
    | 8 -> Some (Ty_bitv S64)
    | _ -> None )
  | Concat _ | Quantifier _ -> None


let concretize_ptr (e : expr) : Num.t option =
  (* TODO: this should work with symbolic pointers *)
  (* would probably introduce Memory Objects here *)
  match e with
  | Val (Num n) -> Some n
  | Ptr (base, Val (Num (I32 offset))) -> Some (I32 (Int32.add base offset))
  | _ -> None

let concretize_base_ptr (e : expr) : int32 option =
  match e with Ptr (base, _) -> Some base | _ -> None

let to_bool (e : expr) : expr option =
  match e with
  | Val _ | Ptr _ -> None
  | (Relop _ as e') | Cvtop (I32 OfBool, e') -> Some e'
  | _ -> Some (Cvtop (I32 ToBool, e))

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

let rec simplify_binop (op : binop) (e1 : expr) (e2 : expr) : expr =
  match op with
  | I32 op -> (
    match (e1, e2) with
    | Val (Num n1), Val (Num n2) ->
      Val (Num (Eval_numeric.eval_binop (I32 op) n1 n2))
    | Ptr (b1, os1), Ptr (b2, os2) -> (
      match op with
      | Sub when b1 = b2 -> simplify_binop (I32 Sub) os1 os2
      | _ -> Binop (I32 op, e1, e2) )
    | Ptr (base, offset), _ -> (
      match op with
      | Add ->
        let new_offset = simplify_binop (I32 Add) offset e2 in
        Ptr (base, new_offset)
      | Sub ->
        let new_offset = simplify_binop (I32 Sub) offset e2 in
        Ptr (base, new_offset)
      | _ -> Binop (I32 op, e1, e2) )
    | _, Ptr (base, offset) -> (
      match op with
      | Add ->
        let new_offset = simplify_binop (I32 Add) offset e1 in
        Ptr (base, new_offset)
      | _ -> Binop (I32 op, e1, e2) )
    | Val (Num (I32 0l)), _ -> (
      match op with
      | Add | Or | Sub -> e2
      | And | DivS | DivU | Mul | RemS | RemU -> Val (Num (I32 0l))
      | _ -> Binop (I32 op, e1, e2) )
    | _, Val (Num (I32 0l)) -> (
      match op with
      | Add | Or | Sub -> e1
      | And | Mul -> Val (Num (I32 0l))
      | _ -> Binop (I32 op, e1, e2) )
    | Binop (I32 op2, x, Val (Num v1)), Val (Num v2) when not (is_num x) -> (
      match (op, op2) with
      | Add, Add ->
        let v = Eval_numeric.eval_binop (I32 Add) v1 v2 in
        Binop (I32 Add, x, Val (Num v))
      | Add, Sub | Sub, Add ->
        let v = Eval_numeric.eval_binop (I32 Sub) v1 v2 in
        Binop (I32 Add, x, Val (Num v))
      | Sub, Sub ->
        let v = Eval_numeric.eval_binop (I32 Add) v1 v2 in
        Binop (I32 Sub, x, Val (Num v))
      | _, _ -> Binop (I32 op, e1, e2) )
    | (bop, Val (Num (I32 1l)) | Val (Num (I32 1l)), bop)
      when is_relop bop && op = And ->
      bop
    | _ -> Binop (I32 op, e1, e2) )
  | I64 op -> (
    match (e1, e2) with
    | Ptr (b1, os1), Ptr (b2, os2) -> (
      match op with
      | Sub when b1 = b2 -> simplify_binop (I64 Sub) os1 os2
      | _ -> Binop (I64 op, e1, e2) )
    | Ptr (base, offset), _ -> (
      match op with
      | Add ->
        let new_offset = simplify_binop (I64 Add) offset e2 in
        Ptr (base, new_offset)
      | Sub ->
        let new_offset = simplify_binop (I64 Sub) offset e2 in
        Ptr (base, new_offset)
      | _ -> Binop (I64 op, e1, e2) )
    | _, Ptr (base, offset) -> (
      match op with
      | Add ->
        let new_offset = simplify_binop (I64 Add) offset e1 in
        Ptr (base, new_offset)
      | _ -> Binop (I64 op, e1, e2) )
    | Val (Num (I64 0L)), _ -> (
      match op with
      | Add | Or | Sub -> e2
      | And | DivS | DivU | Mul | RemS | RemU -> Val (Num (I64 0L))
      | _ -> Binop (I64 op, e1, e2) )
    | _, Val (Num (I64 0L)) -> (
      match op with
      | Add | Or | Sub -> e1
      | And | Mul -> Val (Num (I64 0L))
      | _ -> Binop (I64 op, e1, e2) )
    | Val (Num v1), Val (Num v2) ->
      Val (Num (Eval_numeric.eval_binop (I64 op) v1 v2))
    | Binop (I64 op2, x, Val (Num v1)), Val (Num v2) when not (is_num x) -> (
      match (op, op2) with
      | Add, Add ->
        let v = Eval_numeric.eval_binop (I64 Add) v1 v2 in
        Binop (I64 Add, x, Val (Num v))
      | Add, Sub | Sub, Add ->
        let v = Eval_numeric.eval_binop (I64 Sub) v1 v2 in
        Binop (I64 Add, x, Val (Num v))
      | Sub, Sub ->
        let v = Eval_numeric.eval_binop (I64 Add) v1 v2 in
        Binop (I64 Sub, x, Val (Num v))
      | _, _ -> Binop (I64 op, e1, e2) )
    | (bop, Val (Num (I64 1L)) | Val (Num (I64 1L)), bop)
      when is_relop bop && op = And ->
      bop
    | _ -> Binop (I64 op, e1, e2) )
  | _ -> Binop (op, e1, e2)

let simplify_relop (op : relop) e1 e2 =
  match op with
  | I32 op -> (
    match (e1, e2) with
    | Val (Num v1), Val (Num v2) ->
      Val (Bool (Eval_numeric.eval_relop (I32 op) v1 v2))
    | Ptr (_, _), Val (Num (I32 0l)) | Val (Num (I32 0l)), Ptr (_, _) -> (
      match op with
      | Eq -> Val (Bool false)
      | Ne -> Val (Bool true)
      | _ -> Relop (I32 op, e1, e2) )
    | Ptr (b1, os1), Ptr (b2, os2) -> (
      match op with
      | Eq when b1 = b2 -> Relop (I32 Eq, os1, os2)
      | Eq when b1 <> b2 -> Val (Bool false)
      | Ne when b1 = b2 -> Relop (I32 Ne, os1, os2)
      | Ne when b1 <> b2 -> Val (Bool true)
      | LtU when b1 = b2 -> Relop (I32 LtU, os1, os2)
      | LeU when b1 = b2 -> Relop (I32 LeU, os1, os2)
      | LtU -> Relop (I32 LtU, Val (Num (I32 b1)), Val (Num (I32 b2)))
      | LeU -> Relop (I32 LeU, Val (Num (I32 b1)), Val (Num (I32 b2)))
      | GtU when b1 = b2 -> Relop (I32 GtU, os1, os2)
      | GeU when b1 = b2 -> Relop (I32 GeU, os1, os2)
      | GtU -> Relop (I32 GtU, Val (Num (I32 b1)), Val (Num (I32 b2)))
      | GeU -> Relop (I32 GeU, Val (Num (I32 b1)), Val (Num (I32 b2)))
      | _ -> Relop (I32 op, e1, e2) )
    | _ -> Relop (I32 op, e1, e2) )
  | _ -> Relop (op, e1, e2)

let simplify_extract s h l =
  match s with
  | Val (Num (I64 x)) ->
    let x' = nland64 (Int64.shift_right x (l * 8)) (h - l) in
    Val (Num (I64 x'))
  | _ -> (
    match type_of s with
    | Some t -> if h - l = size t then s else Extract (s, h, l)
    | None -> Extract (s, h, l) )

let simplify_concat msb lsb =
  match (msb, lsb) with
  | Extract (Val (Num (I64 x2)), h2, l2), Extract (Val (Num (I64 x1)), h1, l1)
    ->
    let d1 = h1 - l1
    and d2 = h2 - l2 in
    let x1' = nland64 (Int64.shift_right x1 (l1 * 8)) d1
    and x2' = nland64 (Int64.shift_right x2 (l2 * 8)) d2 in
    let x = Int64.(logor (shift_left x2' (d1 * 8)) x1') in
    Extract (Val (Num (I64 x)), d1 + d2, 0)
  | Extract (Val (Num (I32 x2)), h2, l2), Extract (Val (Num (I32 x1)), h1, l1)
    ->
    let d1 = h1 - l1
    and d2 = h2 - l2 in
    let x1' = nland32 (Int32.shift_right x1 (l1 * 8)) d1
    and x2' = nland32 (Int32.shift_right x2 (l2 * 8)) d2 in
    let x = Int32.(logor (shift_left x2' (d1 * 8)) x1') in
    Extract (Val (Num (I32 x)), d1 + d2, 0)
  | Extract (s1, h, m1), Extract (s2, m2, l) when s1 = s2 && m1 = m2 ->
    Extract (s1, h, l)
  | ( Extract (Val (Num (I64 x2)), h2, l2)
    , Concat (Extract (Val (Num (I64 x1)), h1, l1), se) )
    when not (is_num se) ->
    let d1 = h1 - l1
    and d2 = h2 - l2 in
    let x1' = nland64 (Int64.shift_right x1 (l1 * 8)) d1
    and x2' = nland64 (Int64.shift_right x2 (l2 * 8)) d2 in
    let x = Int64.(logor (shift_left x2' (d1 * 8)) x1') in
    Extract (Val (Num (I64 x)), d1 + d2, 0) ++ se
  | _ -> msb ++ lsb

let rec simplify ?(extract = true) (e : expr) : expr =
  match e with
  | Val v -> Val v
  | Ptr (base, offset) -> Ptr (base, simplify offset)
  | Binop (op, e1, e2) ->
    let e1 = simplify e1 in
    let e2 = simplify e2 in
    simplify_binop op e1 e2
  | Relop (op, e1, e2) ->
    let e1 = simplify e1 in
    let e2 = simplify e2 in
    simplify_relop op e1 e2
  | Extract (_, _, _) when not extract -> e
  | Extract (s, h, l) when extract -> simplify_extract s h l
  | Concat (e1, e2) ->
    let msb = simplify ~extract:false e1 in
    let lsb = simplify ~extract:false e2 in
    simplify_concat msb lsb
  | _ -> e

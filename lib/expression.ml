open Types

exception InvalidRelop

type qt =
  | Forall
  | Exists

type expr =
  | Val of Value.t
  | SymPtr of int32 * expr
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

let mk_symbol_s (t : expr_type) (x : string) : expr =
  Symbol (Symbol.mk_symbol t x)

let is_num (e : expr) : bool = match e with Val (Num _) -> true | _ -> false
let is_val (e : expr) : bool = match e with Val _ -> true | _ -> false
let is_unop (e : expr) : bool = match e with Unop _ -> true | _ -> false
let is_relop (e : expr) : bool = match e with Relop _ -> true | _ -> false
let is_binop (e : expr) : bool = match e with Binop _ -> true | _ -> false
let is_cvtop (e : expr) : bool = match e with Cvtop _ -> true | _ -> false
let is_triop (e : expr) : bool = match e with Triop _ -> true | _ -> false

let is_concrete (e : expr) : bool =
  match e with Val _ | SymPtr (_, Val _) -> true | _ -> false

let rec equal (e1 : expr) (e2 : expr) : bool =
  match (e1, e2) with
  | Val v1, Val v2 -> Value.equal v1 v2
  | SymPtr (b1, o1), SymPtr (b2, o2) -> b1 = b2 && equal o1 o2
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
  | Quantifier (q1, vars1, e1, p1), Quantifier (q2, vars2, e2, p2) ->
    q1 = q2
    && List.equal Symbol.equal vars1 vars2
    && equal e1 e2
    && List.equal (List.equal equal) p1 p2
  | _ -> false

let rec length (e : expr) : int =
  match e with
  | Val _ -> 1
  | SymPtr _ -> 1
  | Unop (_, e) -> 1 + length e
  | Binop (_, e1, e2) -> 1 + length e1 + length e2
  | Triop (_, e1, e2, e3) -> 1 + length e1 + length e2 + length e3
  | Relop (_, e1, e2) -> 1 + length e1 + length e2
  | Cvtop (_, e) -> 1 + length e
  | Symbol _ -> 1
  | Extract (e, _, _) -> 1 + length e
  | Concat (e1, e2) -> 1 + length e1 + length e2
  | Quantifier (_, _, body, _) -> length body

let get_symbols (e : expr list) : Symbol.t list =
  let rec symbols e =
    match e with
    | Val _ -> []
    | SymPtr (_, offset) -> symbols offset
    | Unop (_, e1) -> symbols e1
    | Binop (_, e1, e2) -> symbols e1 @ symbols e2
    | Triop (_, e1, e2, e3) -> symbols e1 @ symbols e2 @ symbols e3
    | Relop (_, e1, e2) -> symbols e1 @ symbols e2
    | Cvtop (_, e) -> symbols e
    | Symbol s -> [ s ]
    | Extract (e, _, _) -> symbols e
    | Concat (e1, e2) -> symbols e1 @ symbols e2
    | Quantifier (_, vars, _, _) -> vars
  in
  List.fold_left
    (fun accum x -> if List.mem x accum then accum else x :: accum)
    []
    (List.concat_map symbols e)

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

let type_of (e : expr) : expr_type option =
  match e with
  | Val v -> Some (Value.type_of v)
  | SymPtr _ -> Some `I32Type
  | Binop (op, _, _) -> Some (Types.type_of op)
  | Triop (op, _, _, _) -> Some (Types.type_of op)
  | Unop (op, _) -> Some (Types.type_of op)
  | Relop (op, _, _) -> Some (Types.type_of op)
  | Cvtop (op, _) -> Some (Types.type_of op)
  | Symbol s -> Some (Symbol.type_of s)
  | Extract (_, h, l) -> (
    match h - l with 4 -> Some `I32Type | 8 -> Some `I64Type | _ -> None )
  | Concat _ | Quantifier _ -> None

let negate_relop (e : expr) : expr =
  match e with
  | Relop (op, e1, e2) -> (
    match op with
    | Int op' -> Relop (Int (I.neg_relop op'), e1, e2)
    | Real op' -> Relop (Real (R.neg_relop op'), e1, e2)
    | Bool op' -> Relop (Bool (B.neg_relop op'), e1, e2)
    | Str op' -> Relop (Str (S.neg_relop op'), e1, e2)
    | I32 op' -> Relop (I32 (I32.neg_relop op'), e1, e2)
    | I64 op' -> Relop (I64 (I64.neg_relop op'), e1, e2)
    | F32 op' -> Relop (F32 (F32.neg_relop op'), e1, e2)
    | F64 op' -> Relop (F64 (F64.neg_relop op'), e1, e2) )
  | _ -> raise InvalidRelop

let pp_unop fmt =
  let fprintf = Format.fprintf in
  function
  | Int op -> fprintf fmt "int.%s" @@ I.string_of_unop op
  | Real op -> fprintf fmt "real.%s" @@ R.string_of_unop op
  | Bool op -> fprintf fmt "bool.%s" @@ B.string_of_unop op
  | Str op -> fprintf fmt "str.%s" @@ S.string_of_unop op
  | I32 op -> fprintf fmt "i32.%s" @@ I32.string_of_unop op
  | I64 op -> fprintf fmt "i64.%s" @@ I64.string_of_unop op
  | F32 op -> fprintf fmt "f32.%s" @@ F32.string_of_unop op
  | F64 op -> fprintf fmt "f64.%s" @@ F64.string_of_unop op

let pp_binop fmt =
  let fprintf = Format.fprintf in
  function
  | Int op -> fprintf fmt "int.%s" @@ I.string_of_binop op
  | Real op -> fprintf fmt "real.%s" @@ R.string_of_binop op
  | Bool op -> fprintf fmt "bool.%s" @@ B.string_of_binop op
  | Str op -> fprintf fmt "str.%s" @@ S.string_of_binop op
  | I32 op -> fprintf fmt "i32.%s" @@ I32.string_of_binop op
  | I64 op -> fprintf fmt "i64.%s" @@ I64.string_of_binop op
  | F32 op -> fprintf fmt "f32.%s" @@ F32.string_of_binop op
  | F64 op -> fprintf fmt "f64.%s" @@ F64.string_of_binop op

let pp_triop fmt =
  let fprintf = Format.fprintf in
  function
  | Int op -> fprintf fmt "int.%s" @@ I.string_of_triop op
  | Real op -> fprintf fmt "real.%s" @@ R.string_of_triop op
  | Bool op -> fprintf fmt "bool.%s" @@ B.string_of_triop op
  | Str op -> fprintf fmt "str.%s" @@ S.string_of_triop op
  | I32 op -> fprintf fmt "i32.%s" @@ I32.string_of_triop op
  | I64 op -> fprintf fmt "i64.%s" @@ I64.string_of_triop op
  | F32 op -> fprintf fmt "f32.%s" @@ F32.string_of_triop op
  | F64 op -> fprintf fmt "f64.%s" @@ F64.string_of_triop op

let pp_relop fmt =
  let fprintf = Format.fprintf in
  function
  | Int op -> fprintf fmt "int.%s" @@ I.string_of_relop op
  | Real op -> fprintf fmt "real.%s" @@ R.string_of_relop op
  | Bool op -> fprintf fmt "bool.%s" @@ B.string_of_relop op
  | Str op -> fprintf fmt "str.%s" @@ S.string_of_relop op
  | I32 op -> fprintf fmt "i32.%s" @@ I32.string_of_relop op
  | I64 op -> fprintf fmt "i64.%s" @@ I64.string_of_relop op
  | F32 op -> fprintf fmt "f32.%s" @@ F32.string_of_relop op
  | F64 op -> fprintf fmt "f64.%s" @@ F64.string_of_relop op

let pp_cvtop fmt =
  let fprintf = Format.fprintf in
  function
  | Int op -> fprintf fmt "int.%s" @@ I.string_of_cvtop op
  | Real op -> fprintf fmt "real.%s" @@ R.string_of_cvtop op
  | Bool op -> fprintf fmt "bool.%s" @@ B.string_of_cvtop op
  | Str op -> fprintf fmt "str.%s" @@ S.string_of_cvtop op
  | I32 op -> fprintf fmt "i32.%s" @@ I32.string_of_cvtop op
  | I64 op -> fprintf fmt "i64.%s" @@ I64.string_of_cvtop op
  | F32 op -> fprintf fmt "f32.%s" @@ F32.string_of_cvtop op
  | F64 op -> fprintf fmt "f64.%s" @@ F64.string_of_cvtop op

let pp_quantifier fmt = function
  | Forall -> Format.pp_print_string fmt "forall"
  | Exists -> Format.pp_print_string fmt "exists"

let pp_vars fmt vars =
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ", ")
    Symbol.pp fmt vars

let rec pp fmt (e : expr) =
  let fprintf = Format.fprintf in
  match e with
  | Val v -> fprintf fmt "%a" Value.pp v
  | SymPtr (base, offset) -> fprintf fmt "(i32.add (i32 %ld) %a)" base pp offset
  | Unop (op, e) -> fprintf fmt "(%a %a)" pp_unop op pp e
  | Binop (op, e1, e2) -> fprintf fmt "(%a %a %a)" pp_binop op pp e1 pp e2
  | Triop (op, e1, e2, e3) ->
    fprintf fmt "(%a %a %a %a)" pp_triop op pp e1 pp e2 pp e3
  | Relop (op, e1, e2) -> fprintf fmt "(%a %a %a)" pp_relop op pp e1 pp e2
  | Cvtop (op, e) -> fprintf fmt "(%a %a)" pp_cvtop op pp e
  | Symbol s -> fprintf fmt "%a" Symbol.pp s
  | Extract (e, h, l) -> fprintf fmt "(extract %a %d %d)" pp e l h
  | Concat (e1, e2) -> fprintf fmt "(++ %a %a)" pp e1 pp e2
  | Quantifier (qt, vars, body, _) ->
    fprintf fmt "%a (%a) %a" pp_quantifier qt pp_vars vars pp body

let to_string e = Format.asprintf "%a" pp e

let pp_list fmt (exprs : expr list) =
  Format.pp_print_list ~pp_sep:Format.pp_print_space pp fmt exprs

let string_of_list (exprs : expr list) : string =
  match exprs with
  | [] -> ""
  | [ x ] -> Format.asprintf "%a" pp x
  | _ -> Format.asprintf "(and %a)" pp_list exprs

let to_smt (es : expr list) : string =
  let symbols =
    List.map
      (fun s ->
        let x = Symbol.to_string s
        and t = Symbol.type_of s in
        Format.sprintf "(declare-fun %s %s)" x (Types.string_of_type t) )
      (get_symbols es)
  in
  let es' = List.map (fun e -> Format.sprintf "(assert %s)" (to_string e)) es in
  String.concat "\n" (symbols @ es' @ [ "(check-sat)" ])

let string_of_values (el : (Num.t * t) list) : string =
  List.fold_left
    (fun a (n, e) -> a ^ Num.to_string n ^ ", " ^ to_string e ^ "\n")
    "" el

let rec get_ptr (e : expr) : Num.t option =
  (* FIXME: this function can be "simplified" *)
  match e with
  | Quantifier _ | Val _ -> None
  | SymPtr (base, _) -> Some (I32 base)
  | Unop (_, e) -> get_ptr e
  | Binop (_, e1, e2) ->
    let p1 = get_ptr e1 in
    if Option.is_some p1 then p1 else get_ptr e2
  | Triop (_, e1, e2, e3) ->
    let p1 = get_ptr e1 in
    if Option.is_some p1 then p1
    else
      let p2 = get_ptr e2 in
      if Option.is_some p2 then p2 else get_ptr e3
  | Relop (_, e1, e2) ->
    let p1 = get_ptr e1 in
    if Option.is_some p1 then p1 else get_ptr e2
  | Cvtop (_, e) -> get_ptr e
  | Symbol _ -> None
  | Extract (e, _, _) -> get_ptr e
  | Concat (e1, e2) ->
    (* assume concatenation of only one ptr *)
    let p1 = get_ptr e1 in
    if Option.is_some p1 then p1 else get_ptr e2

let concretize_ptr (e : expr) : Num.t option =
  (* TODO: this should work with symbolic pointers *)
  (* would probably introduce Memory Objects here *)
  match e with
  | Val (Num n) -> Some n
  | SymPtr (base, Val (Num (I32 offset))) -> Some (I32 (Int32.add base offset))
  | _ -> None

let concretize_base_ptr (e : expr) : int32 option =
  match e with SymPtr (base, _) -> Some base | _ -> None

let to_bool (e : expr) : expr option =
  match e with
  | Val _ | SymPtr _ -> None
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

let rec simplify ?(extract = true) (e : expr) : expr =
  match e with
  | Val v -> Val v
  | SymPtr (base, offset) -> SymPtr (base, simplify offset)
  | Binop (I32 op, e1, e2) -> (
    let e1' = simplify e1
    and e2' = simplify e2 in
    match (e1', e2') with
    | SymPtr (b1, os1), SymPtr (b2, os2) -> (
      match op with
      | Sub when b1 = b2 -> simplify (Binop (I32 Sub, os1, os2))
      | _ -> Binop (I32 op, e1', e2') )
    | SymPtr (base, offset), _ -> (
      match op with
      | Add ->
        let new_offset = simplify (Binop (I32 Add, offset, e2')) in
        simplify (SymPtr (base, new_offset))
      | Sub ->
        let new_offset = simplify (Binop (I32 Sub, offset, e2')) in
        simplify (SymPtr (base, new_offset))
      | _ -> Binop (I32 op, e1', e2') )
    | _, SymPtr (base, offset) -> (
      match op with
      | Add ->
        let new_offset = simplify (Binop (I32 Add, offset, e1')) in
        simplify (SymPtr (base, new_offset))
      | _ -> Binop (I32 op, e1', e2') )
    | Val (Num (I32 0l)), _ -> (
      match op with
      | Add | Or | Sub -> e2'
      | And | DivS | DivU | Mul | RemS | RemU -> Val (Num (I32 0l))
      | _ -> Binop (I32 op, e1', e2') )
    | _, Val (Num (I32 0l)) -> (
      match op with
      | Add | Or | Sub -> e1'
      | And | Mul -> Val (Num (I32 0l))
      | _ -> Binop (I32 op, e1', e2') )
    | Val (Num n1), Val (Num n2) ->
      Val (Num (Eval_numeric.eval_binop (I32 op) n1 n2))
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
      | _, _ -> Binop (I32 op, e1', e2') )
    | (bop, Val (Num (I32 1l)) | Val (Num (I32 1l)), bop)
      when is_relop bop && op = And ->
      bop
    | _ -> Binop (I32 op, e1', e2') )
  | Binop (I64 op, e1, e2) -> (
    let e1' = simplify e1
    and e2' = simplify e2 in
    match (e1', e2') with
    | SymPtr (b1, os1), SymPtr (b2, os2) -> (
      match op with
      | Sub when b1 = b2 -> simplify (Binop (I64 Sub, os1, os2))
      | _ -> Binop (I64 op, e1', e2') )
    | SymPtr (base, offset), _ -> (
      match op with
      | Add ->
        let new_offset = simplify (Binop (I64 Add, offset, e2')) in
        simplify (SymPtr (base, new_offset))
      | Sub ->
        let new_offset = simplify (Binop (I64 Sub, offset, e2')) in
        simplify (SymPtr (base, new_offset))
      | _ -> Binop (I64 op, e1', e2') )
    | _, SymPtr (base, offset) -> (
      match op with
      | Add ->
        let new_offset = simplify (Binop (I64 Add, offset, e1')) in
        simplify (SymPtr (base, new_offset))
      | _ -> Binop (I64 op, e1', e2') )
    | Val (Num (I64 0L)), _ -> (
      match op with
      | Add | Or | Sub -> e2'
      | And | DivS | DivU | Mul | RemS | RemU -> Val (Num (I64 0L))
      | _ -> Binop (I64 op, e1', e2') )
    | _, Val (Num (I64 0L)) -> (
      match op with
      | Add | Or | Sub -> e1'
      | And | Mul -> Val (Num (I64 0L))
      | _ -> Binop (I64 op, e1', e2') )
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
      | _, _ -> Binop (I64 op, e1', e2') )
    | (bop, Val (Num (I64 1L)) | Val (Num (I64 1L)), bop)
      when is_relop bop && op = And ->
      bop
    | _ -> Binop (I64 op, e1', e2') )
  | Relop (I32 op, e1, e2) -> (
    let e1' = simplify e1
    and e2' = simplify e2 in
    match (e1', e2') with
    | Val (Num v1), Val (Num v2) ->
      let ret = Eval_numeric.eval_relop (I32 op) v1 v2 in
      Val (Num (Num.num_of_bool ret))
    | SymPtr (_, _), Val (Num (I32 0l)) | Val (Num (I32 0l)), SymPtr (_, _) -> (
      match op with
      | Eq -> Val (Num (I32 0l))
      | Ne -> Val (Num (I32 1l))
      | _ -> Relop (I32 op, e1', e2') )
    | SymPtr (b1, os1), SymPtr (b2, os2) -> (
      match op with
      | Eq when b1 = b2 -> Relop (I32 Eq, os1, os2)
      | Eq when b1 <> b2 -> Val (Num (I32 0l))
      | Ne when b1 = b2 -> Relop (I32 Ne, os1, os2)
      | Ne when b1 <> b2 -> Val (Num (I32 1l))
      | LtU when b1 = b2 -> Relop (I32 LtU, os1, os2)
      | LeU when b1 = b2 -> Relop (I32 LeU, os1, os2)
      | LtU -> Relop (I32 LtU, Val (Num (I32 b1)), Val (Num (I32 b2)))
      | LeU -> Relop (I32 LeU, Val (Num (I32 b1)), Val (Num (I32 b2)))
      | GtU when b1 = b2 -> Relop (I32 GtU, os1, os2)
      | GeU when b1 = b2 -> Relop (I32 GeU, os1, os2)
      | GtU -> Relop (I32 GtU, Val (Num (I32 b1)), Val (Num (I32 b2)))
      | GeU -> Relop (I32 GeU, Val (Num (I32 b1)), Val (Num (I32 b2)))
      | _ -> Relop (I32 op, e1', e2') )
    | _ -> Relop (I32 op, e1', e2') )
  | Extract (_, _, _) when not extract -> e
  | Extract (s, h, l) when extract -> (
    match s with
    | Val (Num (I64 x)) ->
      let x' = nland64 (Int64.shift_right x (l * 8)) (h - l) in
      Val (Num (I64 x'))
    | _ -> (
      match type_of s with
      | Some t -> if h - l = size t then s else e
      | None -> e ) )
  | Concat (e1, e2) -> (
    let e1' = simplify ~extract:false e1
    and e2' = simplify ~extract:false e2 in
    match (e1', e2') with
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
    | _ -> e1' ++ e2' )
  | _ -> e

let mk_relop ?(reduce : bool = true) (e : expr) (t : num_type) : expr =
  let e = if reduce then simplify e else e in
  if is_relop e then e
  else
    let zero = Value.Num (Num.default_value t) in
    let e' =
      match t with
      | `I32Type -> Relop (I32 Ne, e, Val zero)
      | `I64Type -> Relop (I64 Ne, e, Val zero)
      | `F32Type -> Relop (F32 Ne, e, Val zero)
      | `F64Type -> Relop (F64 Ne, e, Val zero)
    in
    simplify e'

open Base
open Types

exception InvalidRelop

type expr =
  | Str of String.t
  | Num of Num.t
  | SymPtr of Int32.t * expr
  | Binop of binop * expr * expr
  | Unop of unop * expr
  | Relop of relop * expr * expr
  | Cvtop of cvtop * expr
  | Symbolic of expr_type * String.t
  | Extract of expr * Int.t * Int.t
  | Concat of expr * expr

type t = expr
type pc = expr List.t
type value = Num.t * expr

let ( ++ ) (e1 : expr) (e2 : expr) = Concat (e1, e2)
let symbolic (t : expr_type) (x : String.t) : expr = Symbolic (t, x)

let negate_relop (e : expr) : expr =
  match e with
  (* Relop *)
  | Relop (Int op, e1, e2) -> Relop (Int (I.neg_relop op), e1, e2)
  | Relop (I32 op, e1, e2) -> Relop (I32 (I32.neg_relop op), e1, e2)
  | Relop (I64 op, e1, e2) -> Relop (I64 (I64.neg_relop op), e1, e2)
  | Relop (F32 op, e1, e2) -> Relop (F32 (F32.neg_relop op), e1, e2)
  | Relop (F64 op, e1, e2) -> Relop (F64 (F64.neg_relop op), e1, e2)
  | _ -> raise InvalidRelop

(** Measure complexity of formulas *)
let rec length (e : expr) : Int.t =
  match e with
  | Str _ -> 1
  | Num _ -> 1
  | SymPtr _ -> 1
  | Unop (_, e) -> 1 + length e
  | Binop (_, e1, e2) -> 1 + length e1 + length e2
  | Relop (_, e1, e2) -> 1 + length e1 + length e2
  | Cvtop (_, e) -> 1 + length e
  | Symbolic (_, _) -> 1
  | Extract (e, _, _) -> 1 + length e
  | Concat (e1, e2) -> 1 + length e1 + length e2

(** Retrieves the symbolic variables *)
let rec get_symbols (e : expr) : (String.t * expr_type) List.t =
  match e with
  | Str _ -> []
  | Num _ -> []
  | SymPtr (_, offset) -> get_symbols offset
  | Unop (_, e1) -> get_symbols e1
  | Binop (_, e1, e2) -> get_symbols e1 @ get_symbols e2
  | Relop (_, e1, e2) -> get_symbols e1 @ get_symbols e2
  | Cvtop (_, e) -> get_symbols e
  | Symbolic (t, x) -> [ (x, t) ]
  | Extract (e, _, _) -> get_symbols e
  | Concat (e1, e2) -> get_symbols e1 @ get_symbols e2

(**  String representation of a expr  *)
let rec to_string (e : expr) : String.t =
  match e with
  | Str s -> "(Str \"" ^ s ^ "\")"
  | Num n -> Num.string_of_num n
  | SymPtr (base, offset) ->
      let str_o = to_string offset in
      "(SymPtr " ^ Int32.to_string base ^ " + " ^ str_o ^ ")"
  | Unop (op, e) ->
      let str_op =
        match op with
        | Int op -> I.string_of_unop op
        | Str op -> S.string_of_unop op
        | I32 op -> I32.string_of_unop op
        | I64 op -> I64.string_of_unop op
        | F32 op -> F32.string_of_unop op
        | F64 op -> F64.string_of_unop op
      in
      "(" ^ str_op ^ " " ^ to_string e ^ ")"
  | Binop (op, e1, e2) ->
      let str_op =
        match op with
        | Int op -> I.string_of_binop op
        | Str op -> S.string_of_binop op
        | I32 op -> I32.string_of_binop op
        | I64 op -> I64.string_of_binop op
        | F32 op -> F32.string_of_binop op
        | F64 op -> F64.string_of_binop op
      in
      "(" ^ str_op ^ " " ^ to_string e1 ^ ", " ^ to_string e2 ^ ")"
  | Relop (op, e1, e2) ->
      let str_op =
        match op with
        | Int op -> I.string_of_relop op
        | Str op -> S.string_of_relop op
        | I32 op -> I32.string_of_relop op
        | I64 op -> I64.string_of_relop op
        | F32 op -> F32.string_of_relop op
        | F64 op -> F64.string_of_relop op
      in
      "(" ^ str_op ^ " " ^ to_string e1 ^ ", " ^ to_string e2 ^ ")"
  | Cvtop (op, e) ->
      let str_op =
        match op with
        | Int op -> I.string_of_cvtop op
        | Str op -> S.string_of_cvtop op
        | I32 op -> I32.string_of_cvtop op
        | I64 op -> I64.string_of_cvtop op
        | F32 op -> F32.string_of_cvtop op
        | F64 op -> F64.string_of_cvtop op
      in
      "(" ^ str_op ^ " " ^ to_string e ^ ")"
  | Symbolic (t, x) -> "(" ^ string_of_type t ^ " #" ^ x ^ ")"
  | Extract (e, h, l) ->
      "(Extract " ^ to_string e ^ ", " ^ Int.to_string h ^ " " ^ Int.to_string l
      ^ ")"
  | Concat (e1, e2) -> "(Concat " ^ to_string e1 ^ " " ^ to_string e2 ^ ")"

let rec pp_to_string (e : expr) : String.t =
  match e with
  | Str s -> "\"" ^ s ^ "\""
  | Num n -> Num.string_of_num n
  | SymPtr (base, offset) ->
      let str_o = pp_to_string offset in
      "(SymPtr " ^ Int32.to_string base ^ " + " ^ str_o ^ ")"
  (* I32 *)
  | Unop (op, e) ->
      let str_op =
        match op with
        | Int op -> I.pp_string_of_unop op
        | Str op -> S.pp_string_of_unop op
        | I32 op -> I32.pp_string_of_unop op
        | I64 op -> I64.pp_string_of_unop op
        | F32 op -> F32.pp_string_of_unop op
        | F64 op -> F64.pp_string_of_unop op
      in
      "(" ^ str_op ^ " " ^ pp_to_string e ^ ")"
  | Binop (op, e1, e2) ->
      let str_op =
        match op with
        | Int op -> I.pp_string_of_binop op
        | Str op -> S.pp_string_of_binop op
        | I32 op -> I32.pp_string_of_binop op
        | I64 op -> I64.pp_string_of_binop op
        | F32 op -> F32.pp_string_of_binop op
        | F64 op -> F64.pp_string_of_binop op
      in
      "(" ^ str_op ^ " " ^ pp_to_string e1 ^ ", " ^ pp_to_string e2 ^ ")"
  | Relop (op, e1, e2) ->
      let str_op =
        match op with
        | Int op -> I.pp_string_of_relop op
        | Str op -> S.pp_string_of_relop op
        | I32 op -> I32.pp_string_of_relop op
        | I64 op -> I64.pp_string_of_relop op
        | F32 op -> F32.pp_string_of_relop op
        | F64 op -> F64.pp_string_of_relop op
      in
      "(" ^ str_op ^ " " ^ pp_to_string e1 ^ ", " ^ pp_to_string e2 ^ ")"
  | Cvtop (op, e) ->
      let str_op =
        match op with
        | Int op -> I.pp_string_of_cvtop op
        | Str op -> S.pp_string_of_cvtop op
        | I32 op -> I32.pp_string_of_cvtop op
        | I64 op -> I64.pp_string_of_cvtop op
        | F32 op -> F32.pp_string_of_cvtop op
        | F64 op -> F64.pp_string_of_cvtop op
      in
      "(" ^ str_op ^ " " ^ pp_to_string e ^ ")"
  | Symbolic (_, x) -> "#" ^ x
  | Extract (e, h, l) ->
      pp_to_string e ^ "[" ^ Int.to_string l ^ ":" ^ Int.to_string h ^ "]"
  | Concat (e1, e2) ->
      let str_e1 = pp_to_string e1 and str_e2 = pp_to_string e2 in
      "(" ^ str_e1 ^ " ++ " ^ str_e2 ^ ")"

(**  String representation of a list of path conditions  *)
let string_of_pc (pc : pc) : String.t =
  List.fold_left ~init:"" ~f:(fun acc c -> acc ^ pp_to_string c ^ ";\n  ") pc

let pp_string_of_pc (pc : pc) : String.t =
  List.fold_left ~init:"" ~f:(fun acc e -> acc ^ pp_to_string e ^ ";  ") pc

let string_of_values (el : value List.t) : String.t =
  List.fold_left ~init:""
    ~f:(fun a (n, e) -> a ^ Num.string_of_num n ^ ", " ^ pp_to_string e ^ "\n")
    el

let rec type_of (e : expr) : expr_type =
  let rec concat_length (e' : expr) : Int.t =
    match e' with
    | Str _ -> assert false
    | Num n -> size (Types.type_of_num n)
    | SymPtr _ -> 4
    | Binop (op, _, _) -> size (Types.type_of op)
    | Unop (op, _) -> size (Types.type_of op)
    | Relop (op, _, _) -> size (Types.type_of op)
    | Cvtop (op, _) -> size (Types.type_of op)
    | Symbolic (t, _) -> size t
    | Concat (e1, e2) -> concat_length e1 + concat_length e2
    | Extract (_, h, l) -> h - l
  in
  match e with
  | Str _ -> `StrType
  | Num n -> Types.type_of_num n
  | SymPtr _ -> `I32Type
  | Binop (op, _, _) -> Types.type_of op
  | Unop (op, _) -> Types.type_of op
  | Relop (op, _, _) -> Types.type_of op
  | Cvtop (op, _) -> Types.type_of op
  | Symbolic (t, _) -> t
  | Extract (_, h, l) -> (
      match h - l with
      | 4 -> `I32Type
      | 8 -> `I64Type
      | _ -> failwith "unsupported type length")
  | Concat (e1, e2) -> (
      let len = concat_length (e1 ++ e2) in
      let len =
        if len < 4 then size (type_of e1) + size (type_of e2) else len
      in
      match len with
      | 4 -> `I32Type
      | 8 -> `I64Type
      | _ -> failwith "unsupported type length")

let rec get_ptr (e : expr) : Num.t Option.t =
  (* FIXME: this function can be "simplified" *)
  match e with
  | Str _ -> None
  | Num _ -> None
  | SymPtr (base, _) -> Some (I32 base)
  | Unop (_, e) -> get_ptr e
  | Binop (_, e1, e2) ->
      let p1 = get_ptr e1 in
      if Option.is_some p1 then p1 else get_ptr e2
  | Relop (_, e1, e2) ->
      let p1 = get_ptr e1 in
      if Option.is_some p1 then p1 else get_ptr e2
  | Cvtop (_, e) -> get_ptr e
  | Symbolic _ -> None
  | Extract (e, _, _) -> get_ptr e
  | Concat (e1, e2) ->
      (* assume concatenation of only one ptr *)
      let p1 = get_ptr e1 in
      if Option.is_some p1 then p1 else get_ptr e2

let concretize_ptr (e : expr) : Num.t Option.t =
  (* TODO: this should work with symbolic pointers *)
  (* would probably introduce Memory Objects here *)
  let open Int32 in
  match e with
  | Num n -> Some n
  | SymPtr (base, Num (I32 offset)) -> Some (I32 (base + offset))
  | _ -> None

let concretize_base_ptr (e : expr) : Int32.t Option.t =
  match e with SymPtr (base, _) -> Some base | _ -> None

let is_num (e : expr) : Bool.t = match e with Num _ -> true | _ -> false
let is_relop (e : expr) : Bool.t = match e with Relop _ -> true | _ -> false

let is_concrete (e : expr) : Bool.t =
  match e with Num _ | SymPtr (_, Num _) -> true | _ -> false

let to_relop (e : expr) : expr Option.t =
  if is_concrete e then None
  else if is_relop e then Some e
  else Some (Relop (I32 Ne, e, Num (I32 0l)))

let nland64 (x : Int64.t) (n : Int.t) =
  let rec loop x' n' acc =
    if n' = 0 then Int64.(x' land acc)
    else loop x' (n' - 1) Int64.(shift_left acc 8 lor 0xffL)
  in
  loop x n 0L

let nland32 (x : Int32.t) (n : Int.t) =
  let rec loop x' n' acc =
    if n' = 0 then Int32.(x' land acc)
    else loop x' (n' - 1) Int32.(shift_left acc 8 lor 0xffl)
  in
  loop x n 0l

let rec simplify ?(extract = true) (e : expr) : expr =
  match e with
  | Num n -> Num n
  | SymPtr (base, offset) -> SymPtr (base, simplify offset)
  | Binop (I32 op, e1, e2) -> (
      let e1' = simplify e1 and e2' = simplify e2 in
      match (e1', e2') with
      | SymPtr (b1, os1), SymPtr (b2, os2) -> (
          match op with
          | Sub when Int32.(b1 = b2) -> simplify (Binop (I32 Sub, os1, os2))
          | _ -> Binop (I32 op, e1', e2'))
      | SymPtr (base, offset), _ -> (
          match op with
          | Add ->
              let new_offset = simplify (Binop (I32 Add, offset, e2')) in
              simplify (SymPtr (base, new_offset))
          | Sub ->
              let new_offset = simplify (Binop (I32 Sub, offset, e2')) in
              simplify (SymPtr (base, new_offset))
          | _ -> Binop (I32 op, e1', e2'))
      | _, SymPtr (base, offset) -> (
          match op with
          | Add ->
              let new_offset = simplify (Binop (I32 Add, offset, e1')) in
              simplify (SymPtr (base, new_offset))
          | _ -> Binop (I32 op, e1', e2'))
      | Num (I32 0l), _ -> (
          match op with
          | Add | Or | Sub -> e2'
          | And | DivS | DivU | Mul | RemS | RemU -> Num (I32 0l)
          | _ -> Binop (I32 op, e1', e2'))
      | _, Num (I32 0l) -> (
          match op with
          | Add | Or | Sub -> e1'
          | And | Mul -> Num (I32 0l)
          | _ -> Binop (I32 op, e1', e2'))
      | Num n1, Num n2 -> Num (Eval_numeric.eval_binop (I32 op) n1 n2)
      | Binop (I32 op2, x, Num v1), Num v2 when not (is_num x) -> (
          match (op, op2) with
          | Add, Add ->
              let v = Eval_numeric.eval_binop (I32 Add) v1 v2 in
              Binop (I32 Add, x, Num v)
          | Add, Sub | Sub, Add ->
              let v = Eval_numeric.eval_binop (I32 Sub) v1 v2 in
              Binop (I32 Add, x, Num v)
          | Sub, Sub ->
              let v = Eval_numeric.eval_binop (I32 Add) v1 v2 in
              Binop (I32 Sub, x, Num v)
          | _, _ -> Binop (I32 op, e1', e2'))
      | (bop, Num (I32 1l) | Num (I32 1l), bop)
        when is_relop bop && Caml.( = ) op And ->
          bop
      | _ -> Binop (I32 op, e1', e2'))
  | Binop (I64 op, e1, e2) -> (
      let e1' = simplify e1 and e2' = simplify e2 in
      match (e1', e2') with
      | SymPtr (b1, os1), SymPtr (b2, os2) -> (
          match op with
          | Sub when Int32.(b1 = b2) -> simplify (Binop (I64 Sub, os1, os2))
          | _ -> Binop (I64 op, e1', e2'))
      | SymPtr (base, offset), _ -> (
          match op with
          | Add ->
              let new_offset = simplify (Binop (I64 Add, offset, e2')) in
              simplify (SymPtr (base, new_offset))
          | Sub ->
              let new_offset = simplify (Binop (I64 Sub, offset, e2')) in
              simplify (SymPtr (base, new_offset))
          | _ -> Binop (I64 op, e1', e2'))
      | _, SymPtr (base, offset) -> (
          match op with
          | Add ->
              let new_offset = simplify (Binop (I64 Add, offset, e1')) in
              simplify (SymPtr (base, new_offset))
          | _ -> Binop (I64 op, e1', e2'))
      | Num (I64 0L), _ -> (
          match op with
          | Add | Or | Sub -> e2'
          | And | DivS | DivU | Mul | RemS | RemU -> Num (I64 0L)
          | _ -> Binop (I64 op, e1', e2'))
      | _, Num (I64 0L) -> (
          match op with
          | Add | Or | Sub -> e1'
          | And | Mul -> Num (I64 0L)
          | _ -> Binop (I64 op, e1', e2'))
      | Num v1, Num v2 -> Num (Eval_numeric.eval_binop (I64 op) v1 v2)
      | Binop (I64 op2, x, Num v1), Num v2 when not (is_num x) -> (
          match (op, op2) with
          | Add, Add ->
              let v = Eval_numeric.eval_binop (I64 Add) v1 v2 in
              Binop (I64 Add, x, Num v)
          | Add, Sub | Sub, Add ->
              let v = Eval_numeric.eval_binop (I64 Sub) v1 v2 in
              Binop (I64 Add, x, Num v)
          | Sub, Sub ->
              let v = Eval_numeric.eval_binop (I64 Add) v1 v2 in
              Binop (I64 Sub, x, Num v)
          | _, _ -> Binop (I64 op, e1', e2'))
      | (bop, Num (I64 1L) | Num (I64 1L), bop)
        when is_relop bop && Caml.( = ) op And ->
          bop
      | _ -> Binop (I64 op, e1', e2'))
  | Relop (I32 op, e1, e2) -> (
      let e1' = simplify e1 and e2' = simplify e2 in
      match (e1', e2') with
      | Num v1, Num v2 ->
          let ret = Eval_numeric.eval_relop (I32 op) v1 v2 in
          Num (Num.num_of_bool ret)
      | SymPtr (_, _), Num (I32 0l) | Num (I32 0l), SymPtr (_, _) -> (
          match op with
          | Eq -> Num (I32 0l)
          | Ne -> Num (I32 1l)
          | _ -> Relop (I32 op, e1', e2'))
      | SymPtr (b1, os1), SymPtr (b2, os2) -> (
          let open Int32 in
          match op with
          | Eq when b1 = b2 -> Relop (I32 Eq, os1, os2)
          | Eq when b1 <> b2 -> Num (I32 0l)
          | Ne when b1 = b2 -> Relop (I32 Ne, os1, os2)
          | Ne when b1 <> b2 -> Num (I32 1l)
          | LtU when b1 = b2 -> Relop (I32 LtU, os1, os2)
          | LeU when b1 = b2 -> Relop (I32 LeU, os1, os2)
          | LtU -> Relop (I32 LtU, Num (I32 b1), Num (I32 b2))
          | LeU -> Relop (I32 LeU, Num (I32 b1), Num (I32 b2))
          | GtU when b1 = b2 -> Relop (I32 GtU, os1, os2)
          | GeU when b1 = b2 -> Relop (I32 GeU, os1, os2)
          | GtU -> Relop (I32 GtU, Num (I32 b1), Num (I32 b2))
          | GeU -> Relop (I32 GeU, Num (I32 b1), Num (I32 b2))
          | _ -> Relop (I32 op, e1', e2'))
      | _ -> Relop (I32 op, e1', e2'))
  | Extract (_, _, _) when not extract -> e
  | Extract (s, h, l) when extract -> (
      match s with
      | Num (I64 x) ->
          let x' = nland64 (Int64.shift_right x (l * 8)) (h - l) in
          Num (I64 x')
      | _ when h - l = size (type_of s) -> s
      | _ -> e)
  | Concat (e1, e2) -> (
      let e1' = simplify ~extract:false e1
      and e2' = simplify ~extract:false e2 in
      match (e1', e2') with
      | Extract (Num (I64 x2), h2, l2), Extract (Num (I64 x1), h1, l1) ->
          let d1 = h1 - l1 and d2 = h2 - l2 in
          let x1' = nland64 (Int64.shift_right x1 (l1 * 8)) d1
          and x2' = nland64 (Int64.shift_right x2 (l2 * 8)) d2 in
          let x = Int64.(shift_left x2' (Int.( * ) d1 8) lor x1') in
          Extract (Num (I64 x), d1 + d2, 0)
      | Extract (Num (I32 x2), h2, l2), Extract (Num (I32 x1), h1, l1) ->
          let d1 = h1 - l1 and d2 = h2 - l2 in
          let x1' = nland32 (Int32.shift_right x1 (l1 * 8)) d1
          and x2' = nland32 (Int32.shift_right x2 (l2 * 8)) d2 in
          let x = Int32.(shift_left x2' (Int.( * ) d1 8) lor x1') in
          Extract (Num (I32 x), d1 + d2, 0)
      | Extract (s1, h, m1), Extract (s2, m2, l)
        when Caml.( = ) s1 s2 && m1 = m2 ->
          Extract (s1, h, l)
      | ( Extract (Num (I64 x2), h2, l2),
          Concat (Extract (Num (I64 x1), h1, l1), se) )
        when not (is_num se) ->
          let d1 = h1 - l1 and d2 = h2 - l2 in
          let x1' = nland64 (Int64.shift_right x1 (l1 * 8)) d1
          and x2' = nland64 (Int64.shift_right x2 (l2 * 8)) d2 in
          let x = Int64.(shift_left x2' (Int.( * ) d1 8) lor x1') in
          Extract (Num (I64 x), d1 + d2, 0) ++ se
      | _ -> e1' ++ e2')
  | _ -> e

let mk_relop ?(reduce : bool = true) (e : expr) (t : num_type) : expr =
  let e = if reduce then simplify e else e in
  if is_relop e then e
  else
    let zero = Num.default_value t in
    let e' =
      match t with
      | `IntType -> Relop (Int Ne, e, Num zero)
      | `I32Type -> Relop (I32 Ne, e, Num zero)
      | `I64Type -> Relop (I64 Ne, e, Num zero)
      | `F32Type -> Relop (F32 Ne, e, Num zero)
      | `F64Type -> Relop (F64 Ne, e, Num zero)
    in
    simplify e'

let insert_pc ?(neg : bool = false) (e : expr) (pc : pc) : pc =
  let cond =
    let c = to_relop (simplify e) in
    if neg then Option.map ~f:negate_relop c else c
  in
  Option.fold ~init:pc ~f:(fun pc a -> a :: pc) cond

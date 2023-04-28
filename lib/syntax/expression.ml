open Base
open Types

exception InvalidRelop

type qt = Forall | Exists

type expr =
  | Val of Value.t
  | SymPtr of Int32.t * expr
  | Unop of unop * expr
  | Binop of binop * expr * expr
  | Relop of relop * expr * expr
  | Cvtop of cvtop * expr
  | Triop of triop * expr * expr * expr
  | Symbol of expr_type * String.t
  | Extract of expr * Int.t * Int.t
  | Concat of expr * expr
  | Quantifier of qt * (string * expr_type) list * expr * expr list list

type t = expr
type pc = expr List.t

let ( ++ ) (e1 : expr) (e2 : expr) = Concat (e1, e2)
let mk_symbol (t : expr_type) (x : String.t) : expr = Symbol (t, x)
let is_num (e : expr) : Bool.t = match e with Val (Num _) -> true | _ -> false
let is_val (e : expr) : Bool.t = match e with Val _ -> true | _ -> false
let is_unop (e : expr) : Bool.t = match e with Unop _ -> true | _ -> false
let is_relop (e : expr) : Bool.t = match e with Relop _ -> true | _ -> false
let is_binop (e : expr) : Bool.t = match e with Binop _ -> true | _ -> false
let is_cvtop (e : expr) : Bool.t = match e with Cvtop _ -> true | _ -> false
let is_triop (e : expr) : Bool.t = match e with Triop _ -> true | _ -> false

let is_concrete (e : expr) : Bool.t =
  match e with Val _ | SymPtr (_, Val _) -> true | _ -> false

let rec equal (e1 : expr) (e2 : expr) : Bool.t =
  match (e1, e2) with
  | Val v1, Val v2 -> Value.equal v1 v2
  | SymPtr (b1, o1), SymPtr (b2, o2) -> Int32.(b1 = b2) && equal o1 o2
  | Unop (op1, e1), Unop (op2, e2) -> Caml.( = ) op1 op2 && equal e1 e2
  | Cvtop (op1, e1), Cvtop (op2, e2) -> Caml.( = ) op1 op2 && equal e1 e2
  | Binop (op1, e1, e3), Binop (op2, e2, e4) ->
      Caml.( = ) op1 op2 && equal e1 e2 && equal e3 e4
  | Relop (op1, e1, e3), Relop (op2, e2, e4) ->
      Caml.( = ) op1 op2 && equal e1 e2 && equal e3 e4
  | Triop (op1, e1, e3, e5), Triop (op2, e2, e4, e6) ->
      Caml.( = ) op1 op2 && equal e1 e2 && equal e3 e4 && equal e5 e6
  | Symbol (t1, x1), Symbol (t2, x2) -> Caml.( = ) t1 t2 && String.equal x1 x2
  | Extract (e1, h1, l1), Extract (e2, h2, l2) ->
      equal e1 e2 && Int.(h1 = h2) && Int.(l1 = l2)
  | Concat (e1, e3), Concat (e2, e4) -> equal e1 e2 && equal e3 e4
  | Quantifier (q1, vars1, e1, p1), Quantifier (q2, vars2, e2, p2) ->
      let eq (x1, t1) (x2, t2) = Caml.( = ) t1 t2 && String.equal x1 x2 in
      Caml.( = ) q1 q2 && List.equal eq vars1 vars2 && equal e1 e2
      && List.equal (List.equal equal) p1 p2
  | _ -> false

let rec length (e : expr) : Int.t =
  match e with
  | Val _ -> 1
  | SymPtr _ -> 1
  | Unop (_, e) -> 1 + length e
  | Binop (_, e1, e2) -> 1 + length e1 + length e2
  | Triop (_, e1, e2, e3) -> 1 + length e1 + length e2 + length e3
  | Relop (_, e1, e2) -> 1 + length e1 + length e2
  | Cvtop (_, e) -> 1 + length e
  | Symbol (_, _) -> 1
  | Extract (e, _, _) -> 1 + length e
  | Concat (e1, e2) -> 1 + length e1 + length e2
  | Quantifier (_, _, body, _) -> length body

let get_symbols (e : expr) : (String.t * expr_type) List.t =
  let rec symbols e =
    match e with
    | Val _ -> []
    | SymPtr (_, offset) -> symbols offset
    | Unop (_, e1) -> symbols e1
    | Binop (_, e1, e2) -> symbols e1 @ symbols e2
    | Triop (_, e1, e2, e3) -> symbols e1 @ symbols e2 @ symbols e3
    | Relop (_, e1, e2) -> symbols e1 @ symbols e2
    | Cvtop (_, e) -> symbols e
    | Symbol (t, x) -> [ (x, t) ]
    | Extract (e, _, _) -> symbols e
    | Concat (e1, e2) -> symbols e1 @ symbols e2
    | Quantifier (_, vars, _, _) -> vars
  in
  let equal (x1, _) (x2, _) = String.equal x1 x2 in
  List.fold (symbols e) ~init:[] ~f:(fun accum x ->
      if List.mem accum x ~equal then accum else x :: accum)

let rec type_of (e : expr) : expr_type =
  (* FIXME: this function can be "simplified" *)
  let rec concat_length (e' : expr) : Int.t =
    match e' with
    | Quantifier _ -> assert false
    | Val v -> size (Value.type_of v)
    | SymPtr _ -> 4
    | Binop (op, _, _) -> size (Types.type_of op)
    | Triop (op, _, _, _) -> size (Types.type_of op)
    | Unop (op, _) -> size (Types.type_of op)
    | Relop (op, _, _) -> size (Types.type_of op)
    | Cvtop (op, _) -> size (Types.type_of op)
    | Symbol (t, _) -> size t
    | Concat (e1, e2) -> concat_length e1 + concat_length e2
    | Extract (_, h, l) -> h - l
  in
  match e with
  | Val v -> Value.type_of v
  | SymPtr _ -> `I32Type
  | Binop (op, _, _) -> Types.type_of op
  | Triop (op, _, _, _) -> Types.type_of op
  | Unop (op, _) -> Types.type_of op
  | Relop (op, _, _) -> Types.type_of op
  | Cvtop (op, _) -> Types.type_of op
  | Symbol (t, _) -> t
  | Extract (_, h, l) ->
      let d = h - l in
      if d = 4 then `I32Type
      else if d = 8 then `I64Type
      else failwith "unsupported type length"
  | Concat (e1, e2) ->
      let len = concat_length (e1 ++ e2) in
      let len =
        if len < 4 then size (type_of e1) + size (type_of e2) else len
      in
      if len = 4 then `I32Type
      else if len = 8 then `I64Type
      else failwith "unsupported type length"
  | Quantifier _ -> assert false

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
      | F64 op' -> Relop (F64 (F64.neg_relop op'), e1, e2))
  | _ -> raise InvalidRelop

(**  String representation of a expr  *)
let rec to_string (e : expr) : String.t =
  match e with
  | Val v -> Value.to_string v
  | SymPtr (base, offset) ->
      let str_o = to_string offset in
      "(SymPtr " ^ Int32.to_string base ^ " + " ^ str_o ^ ")"
  | Unop (op, e) ->
      let str_op =
        match op with
        | Int op -> I.string_of_unop op
        | Real op -> R.string_of_unop op
        | Bool op -> B.string_of_unop op
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
        | Real op -> R.string_of_binop op
        | Bool op -> B.string_of_binop op
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
        | Real op -> R.string_of_relop op
        | Bool op -> B.string_of_relop op
        | Str op -> S.string_of_relop op
        | I32 op -> I32.string_of_relop op
        | I64 op -> I64.string_of_relop op
        | F32 op -> F32.string_of_relop op
        | F64 op -> F64.string_of_relop op
      in
      "(" ^ str_op ^ " " ^ to_string e1 ^ ", " ^ to_string e2 ^ ")"
  | Triop (op, e1, e2, e3) ->
      let str_op =
        match op with
        | Int op -> I.string_of_triop op
        | Real op -> R.string_of_triop op
        | Bool op -> B.string_of_triop op
        | Str op -> S.string_of_triop op
        | I32 op -> I32.string_of_triop op
        | I64 op -> I64.string_of_triop op
        | F32 op -> F32.string_of_triop op
        | F64 op -> F64.string_of_triop op
      in
      "(" ^ str_op ^ " " ^ to_string e1 ^ ", " ^ to_string e2 ^ ", "
      ^ to_string e3 ^ ")"
  | Cvtop (op, e) ->
      let str_op =
        match op with
        | Int op -> I.string_of_cvtop op
        | Real op -> R.string_of_cvtop op
        | Bool op -> B.string_of_cvtop op
        | Str op -> S.string_of_cvtop op
        | I32 op -> I32.string_of_cvtop op
        | I64 op -> I64.string_of_cvtop op
        | F32 op -> F32.string_of_cvtop op
        | F64 op -> F64.string_of_cvtop op
      in
      "(" ^ str_op ^ " " ^ to_string e ^ ")"
  | Symbol (t, x) -> "(" ^ string_of_type t ^ " #" ^ x ^ ")"
  | Extract (e, h, l) ->
      "(Extract " ^ to_string e ^ ", " ^ Int.to_string h ^ " " ^ Int.to_string l
      ^ ")"
  | Concat (e1, e2) -> "(Concat " ^ to_string e1 ^ " " ^ to_string e2 ^ ")"
  | Quantifier (qt, vars, body, _) ->
      let qt' = match qt with Forall -> "Forall" | Exists -> "Exists" in
      let xs' = String.concat ~sep:", " (List.map ~f:(fun (x, _) -> x) vars) in
      qt' ^ "(" ^ xs' ^ ")" ^ to_string body

let rec pp_to_string (e : expr) : String.t =
  match e with
  | Val v -> Value.to_string v
  | SymPtr (base, offset) ->
      let str_o = pp_to_string offset in
      "(SymPtr " ^ Int32.to_string base ^ " + " ^ str_o ^ ")"
  (* I32 *)
  | Unop (op, e) ->
      let str_op =
        match op with
        | Int op -> I.pp_string_of_unop op
        | Real op -> R.pp_string_of_unop op
        | Bool op -> B.pp_string_of_unop op
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
        | Real op -> R.pp_string_of_binop op
        | Bool op -> B.pp_string_of_binop op
        | Str op -> S.pp_string_of_binop op
        | I32 op -> I32.pp_string_of_binop op
        | I64 op -> I64.pp_string_of_binop op
        | F32 op -> F32.pp_string_of_binop op
        | F64 op -> F64.pp_string_of_binop op
      in
      "(" ^ str_op ^ " " ^ pp_to_string e1 ^ ", " ^ pp_to_string e2 ^ ")"
  | Triop (op, e1, e2, e3) ->
      let str_op =
        match op with
        | Int op -> I.pp_string_of_triop op
        | Real op -> R.pp_string_of_triop op
        | Bool op -> B.pp_string_of_triop op
        | Str op -> S.pp_string_of_triop op
        | I32 op -> I32.pp_string_of_triop op
        | I64 op -> I64.pp_string_of_triop op
        | F32 op -> F32.pp_string_of_triop op
        | F64 op -> F64.pp_string_of_triop op
      in
      "(" ^ str_op ^ " " ^ pp_to_string e1 ^ ", " ^ pp_to_string e2
      ^ pp_to_string e3 ^ ")"
  | Relop (op, e1, e2) ->
      let str_op =
        match op with
        | Int op -> I.pp_string_of_relop op
        | Real op -> R.pp_string_of_relop op
        | Bool op -> B.pp_string_of_relop op
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
        | Real op -> R.pp_string_of_cvtop op
        | Bool op -> B.pp_string_of_cvtop op
        | Str op -> S.pp_string_of_cvtop op
        | I32 op -> I32.pp_string_of_cvtop op
        | I64 op -> I64.pp_string_of_cvtop op
        | F32 op -> F32.pp_string_of_cvtop op
        | F64 op -> F64.pp_string_of_cvtop op
      in
      "(" ^ str_op ^ " " ^ pp_to_string e ^ ")"
  | Symbol (_, x) -> "#" ^ x
  | Extract (e, h, l) ->
      pp_to_string e ^ "[" ^ Int.to_string l ^ ":" ^ Int.to_string h ^ "]"
  | Concat (e1, e2) ->
      let str_e1 = pp_to_string e1 and str_e2 = pp_to_string e2 in
      "(" ^ str_e1 ^ " ++ " ^ str_e2 ^ ")"
  | Quantifier (qt, vars, body, _) ->
      let qt' = match qt with Forall -> "Forall" | Exists -> "Exists" in
      let xs' = String.concat ~sep:", " (List.map ~f:(fun (x, _) -> x) vars) in
      qt' ^ "(" ^ xs' ^ ")" ^ pp_to_string body

let string_of_pc (pc : pc) : String.t =
  List.fold_left ~init:"" ~f:(fun acc c -> acc ^ pp_to_string c ^ ";\n  ") pc

let pp_string_of_pc (pc : pc) : String.t =
  List.fold_left ~init:"" ~f:(fun acc e -> acc ^ pp_to_string e ^ ";  ") pc

let string_of_values (el : (Num.t * t) List.t) : String.t =
  List.fold_left ~init:""
    ~f:(fun a (n, e) -> a ^ Num.to_string n ^ ", " ^ pp_to_string e ^ "\n")
    el

let rec get_ptr (e : expr) : Num.t Option.t =
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

let concretize_ptr (e : expr) : Num.t Option.t =
  (* TODO: this should work with symbolic pointers *)
  (* would probably introduce Memory Objects here *)
  let open Int32 in
  match e with
  | Val (Num n) -> Some n
  | SymPtr (base, Val (Num (I32 offset))) -> Some (I32 (base + offset))
  | _ -> None

let concretize_base_ptr (e : expr) : Int32.t Option.t =
  match e with SymPtr (base, _) -> Some base | _ -> None

let to_relop (e : expr) : expr Option.t =
  if is_concrete e then None
  else if is_relop e then Some e
  else Some (Relop (I32 Ne, e, Val (Num (I32 0l))))

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
  | Val v -> Val v
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
      | Val (Num (I32 0l)), _ -> (
          match op with
          | Add | Or | Sub -> e2'
          | And | DivS | DivU | Mul | RemS | RemU -> Val (Num (I32 0l))
          | _ -> Binop (I32 op, e1', e2'))
      | _, Val (Num (I32 0l)) -> (
          match op with
          | Add | Or | Sub -> e1'
          | And | Mul -> Val (Num (I32 0l))
          | _ -> Binop (I32 op, e1', e2'))
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
          | _, _ -> Binop (I32 op, e1', e2'))
      | (bop, Val (Num (I32 1l)) | Val (Num (I32 1l)), bop)
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
      | Val (Num (I64 0L)), _ -> (
          match op with
          | Add | Or | Sub -> e2'
          | And | DivS | DivU | Mul | RemS | RemU -> Val (Num (I64 0L))
          | _ -> Binop (I64 op, e1', e2'))
      | _, Val (Num (I64 0L)) -> (
          match op with
          | Add | Or | Sub -> e1'
          | And | Mul -> Val (Num (I64 0L))
          | _ -> Binop (I64 op, e1', e2'))
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
          | _, _ -> Binop (I64 op, e1', e2'))
      | (bop, Val (Num (I64 1L)) | Val (Num (I64 1L)), bop)
        when is_relop bop && Caml.( = ) op And ->
          bop
      | _ -> Binop (I64 op, e1', e2'))
  | Relop (I32 op, e1, e2) -> (
      let e1' = simplify e1 and e2' = simplify e2 in
      match (e1', e2') with
      | Val (Num v1), Val (Num v2) ->
          let ret = Eval_numeric.eval_relop (I32 op) v1 v2 in
          Val (Num (Num.num_of_bool ret))
      | SymPtr (_, _), Val (Num (I32 0l)) | Val (Num (I32 0l)), SymPtr (_, _)
        -> (
          match op with
          | Eq -> Val (Num (I32 0l))
          | Ne -> Val (Num (I32 1l))
          | _ -> Relop (I32 op, e1', e2'))
      | SymPtr (b1, os1), SymPtr (b2, os2) -> (
          let open Int32 in
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
          | _ -> Relop (I32 op, e1', e2'))
      | _ -> Relop (I32 op, e1', e2'))
  | Extract (_, _, _) when not extract -> e
  | Extract (s, h, l) when extract -> (
      match s with
      | Val (Num (I64 x)) ->
          let x' = nland64 (Int64.shift_right x (l * 8)) (h - l) in
          Val (Num (I64 x'))
      | _ when h - l = size (type_of s) -> s
      | _ -> e)
  | Concat (e1, e2) -> (
      let e1' = simplify ~extract:false e1
      and e2' = simplify ~extract:false e2 in
      match (e1', e2') with
      | ( Extract (Val (Num (I64 x2)), h2, l2),
          Extract (Val (Num (I64 x1)), h1, l1) ) ->
          let d1 = h1 - l1 and d2 = h2 - l2 in
          let x1' = nland64 (Int64.shift_right x1 (l1 * 8)) d1
          and x2' = nland64 (Int64.shift_right x2 (l2 * 8)) d2 in
          let x = Int64.(shift_left x2' (Int.( * ) d1 8) lor x1') in
          Extract (Val (Num (I64 x)), d1 + d2, 0)
      | ( Extract (Val (Num (I32 x2)), h2, l2),
          Extract (Val (Num (I32 x1)), h1, l1) ) ->
          let d1 = h1 - l1 and d2 = h2 - l2 in
          let x1' = nland32 (Int32.shift_right x1 (l1 * 8)) d1
          and x2' = nland32 (Int32.shift_right x2 (l2 * 8)) d2 in
          let x = Int32.(shift_left x2' (Int.( * ) d1 8) lor x1') in
          Extract (Val (Num (I32 x)), d1 + d2, 0)
      | Extract (s1, h, m1), Extract (s2, m2, l)
        when Caml.( = ) s1 s2 && m1 = m2 ->
          Extract (s1, h, l)
      | ( Extract (Val (Num (I64 x2)), h2, l2),
          Concat (Extract (Val (Num (I64 x1)), h1, l1), se) )
        when not (is_num se) ->
          let d1 = h1 - l1 and d2 = h2 - l2 in
          let x1' = nland64 (Int64.shift_right x1 (l1 * 8)) d1
          and x2' = nland64 (Int64.shift_right x2 (l2 * 8)) d2 in
          let x = Int64.(shift_left x2' (Int.( * ) d1 8) lor x1') in
          Extract (Val (Num (I64 x)), d1 + d2, 0) ++ se
      | _ -> e1' ++ e2')
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

let add_constraint ?(neg : bool = false) (e : expr) (pc : expr) : expr =
  let cond =
    let c = to_relop (simplify e) in
    if neg then Option.map ~f:negate_relop c else c
  in
  match (cond, pc) with
  | None, _ -> pc
  | Some cond, Val (Bool true) -> cond
  | Some cond, _ -> Binop (Bool B.And, cond, pc)

let insert_pc ?(neg : bool = false) (e : expr) (pc : pc) : pc =
  let cond =
    let c = to_relop (simplify e) in
    if neg then Option.map ~f:negate_relop c else c
  in
  Option.fold ~init:pc ~f:(fun pc a -> a :: pc) cond

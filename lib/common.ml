open Base
open Z3
open Types

exception Error of string

let ctx =
  Z3.mk_context
    [ ("model", "true"); ("proof", "false"); ("unsat_core", "false") ]

(** Sorts *)
let int_sort = Arithmetic.Integer.mk_sort ctx

let str_sort = Seq.mk_string_sort ctx
let bv32_sort = BitVector.mk_sort ctx 32
let bv64_sort = BitVector.mk_sort ctx 64
let fp32_sort = FloatingPoint.mk_sort_single ctx
let fp64_sort = FloatingPoint.mk_sort_double ctx

(** Rounding Modes *)
let rne = FloatingPoint.RoundingMode.mk_rne ctx

let rtz = FloatingPoint.RoundingMode.mk_rtz ctx

(** Match WASM Type with Z3 Sort *)
let get_sort (e : Types.expr_type) : Z3.Sort.sort =
  match e with
  | `IntType -> int_sort
  | `StrType -> str_sort
  | `I32Type -> bv32_sort
  | `I64Type -> bv64_sort
  | `F32Type -> fp32_sort
  | `F64Type -> fp64_sort

(** Bool helper - cast bool to bv *)
let encode_bool (to_bv : bool) (cond : Expr.expr) : Expr.expr =
  let bv_true = BitVector.mk_numeral ctx "1" 32
  and bv_false = BitVector.mk_numeral ctx "0" 32 in
  if to_bv then Boolean.mk_ite ctx cond bv_true bv_false else cond

module IntZ3Op = struct
  open I

  let encode_num (i : Int.t) : Expr.expr = Expr.mk_numeral_int ctx i int_sort
  let encode_unop (_ : unop) (_ : Expr.expr) : Expr.expr = assert false

  let encode_binop (op : binop) (e1 : Expr.expr) (e2 : Expr.expr) : Expr.expr =
    let op' =
      match op with
      | Add -> fun v1 v2 -> Arithmetic.mk_add ctx [ v1; v2 ]
      | Sub -> fun v1 v2 -> Arithmetic.mk_sub ctx [ v1; v2 ]
      | Mul -> fun v1 v2 -> Arithmetic.mk_mul ctx [ v1; v2 ]
      | Div -> Arithmetic.mk_div ctx
      | Rem -> Arithmetic.Integer.mk_rem ctx
      | _ -> raise (Error "Unsupported integer operations")
    in
    op' e1 e2

  let encode_relop (op : relop) (e1 : Expr.expr) (e2 : Expr.expr) : Expr.expr =
    let op' =
      match op with
      | Eq -> Boolean.mk_eq ctx
      | Ne -> fun v1 v2 -> Boolean.mk_not ctx (Boolean.mk_eq ctx v1 v2)
      | Lt -> Arithmetic.mk_lt ctx
      | Gt -> Arithmetic.mk_gt ctx
      | Le -> Arithmetic.mk_le ctx
      | Ge -> Arithmetic.mk_ge ctx
    in
    op' e1 e2

  let encode_cvtop (_ : cvtop) (_ : Expr.expr) : Expr.expr = assert false
end

module StrZ3Op = struct
  open S

  let encode_str (s : String.t) : Expr.expr = Seq.mk_string ctx s

  let encode_unop (_ : unop) (_ : Expr.expr) : Expr.expr =
    raise (Error "Not implemented")

  let encode_binop (_ : binop) (_ : Expr.expr) (_ : Expr.expr) : Expr.expr =
    raise (Error "Not implemented")

  let encode_relop (_ : relop) (_ : Expr.expr) (_ : Expr.expr) : Expr.expr =
    raise (Error "Not implemented")

  let encode_cvtop (_ : cvtop) (_ : Expr.expr) : Expr.expr =
    raise (Error "Not implemented")
end

module I32Z3Op = struct
  open I32

  let encode_num (i : Int32.t) : Expr.expr =
    Expr.mk_numeral_int ctx (Int32.to_int_exn i) bv32_sort

  let encode_unop (_ : unop) (_ : Expr.expr) : Expr.expr =
    failwith "Zi32: encode_unop: Construct not supported yet"

  let encode_binop (op : binop) (e1 : Expr.expr) (e2 : Expr.expr) : Expr.expr =
    let op' =
      match op with
      | Add -> BitVector.mk_add ctx
      | Sub -> BitVector.mk_sub ctx
      | Mul -> BitVector.mk_mul ctx
      | DivS -> BitVector.mk_sdiv ctx
      | DivU -> BitVector.mk_udiv ctx
      | And -> BitVector.mk_and ctx
      | Xor -> BitVector.mk_xor ctx
      | Or -> BitVector.mk_or ctx
      | Shl -> BitVector.mk_shl ctx
      | ShrS -> BitVector.mk_ashr ctx
      | ShrU -> BitVector.mk_lshr ctx
      | RemS -> BitVector.mk_srem ctx
      | RemU -> BitVector.mk_urem ctx
    in
    op' e1 e2

  let encode_relop ?(to_bv = false) (op : relop) (e1 : Expr.expr)
      (e2 : Expr.expr) : Expr.expr =
    let op' =
      match op with
      | Eq -> Boolean.mk_eq ctx
      | Ne -> fun x1 x2 -> Boolean.mk_not ctx (Boolean.mk_eq ctx x1 x2)
      | LtU -> BitVector.mk_ult ctx
      | LtS -> BitVector.mk_slt ctx
      | LeU -> BitVector.mk_ule ctx
      | LeS -> BitVector.mk_sle ctx
      | GtU -> BitVector.mk_ugt ctx
      | GtS -> BitVector.mk_sgt ctx
      | GeU -> BitVector.mk_uge ctx
      | GeS -> BitVector.mk_sge ctx
    in
    encode_bool to_bv (op' e1 e2)

  let encode_cvtop (op : cvtop) (e : Expr.expr) : Expr.expr =
    let op' =
      match op with
      | TruncSF32 -> fun f -> FloatingPoint.mk_to_sbv ctx rtz f 32
      | TruncUF32 -> fun f -> FloatingPoint.mk_to_ubv ctx rtz f 32
      | TruncSF64 -> fun f -> FloatingPoint.mk_to_sbv ctx rtz f 32
      | TruncUF64 -> fun f -> FloatingPoint.mk_to_ubv ctx rtz f 32
      | ReinterpretFloat -> FloatingPoint.mk_to_ieee_bv ctx
      | WrapI64 | ExtendSI32 | ExtendUI32 -> assert false
    in
    op' e
end

module I64Z3Op = struct
  open I64

  let encode_num (i : Int64.t) : Expr.expr =
    Expr.mk_numeral_int ctx (Int64.to_int_exn i) bv64_sort

  let encode_unop (_ : unop) (_ : Expr.expr) : Expr.expr =
    failwith "Zi64: encode_unop: Construct not supported yet"

  let encode_binop (op : binop) (e1 : Expr.expr) (e2 : Expr.expr) : Expr.expr =
    let op' =
      match op with
      | Add -> BitVector.mk_add ctx
      | Sub -> BitVector.mk_sub ctx
      | Mul -> BitVector.mk_mul ctx
      | DivS -> BitVector.mk_sdiv ctx
      | DivU -> BitVector.mk_udiv ctx
      | And -> BitVector.mk_and ctx
      | Xor -> BitVector.mk_xor ctx
      | Or -> BitVector.mk_or ctx
      | Shl -> BitVector.mk_shl ctx
      | ShrS -> BitVector.mk_ashr ctx
      | ShrU -> BitVector.mk_lshr ctx
      | RemS -> BitVector.mk_srem ctx
      | RemU -> BitVector.mk_urem ctx
    in
    op' e1 e2

  let encode_relop ?(to_bv = false) (op : relop) (e1 : Expr.expr)
      (e2 : Expr.expr) : Expr.expr =
    let op' =
      match op with
      | Eq -> Boolean.mk_eq ctx
      | Ne -> fun x1 x2 -> Boolean.mk_not ctx (Boolean.mk_eq ctx x1 x2)
      | LtU -> BitVector.mk_ult ctx
      | LtS -> BitVector.mk_slt ctx
      | LeU -> BitVector.mk_ule ctx
      | LeS -> BitVector.mk_sle ctx
      | GtU -> BitVector.mk_ugt ctx
      | GtS -> BitVector.mk_sgt ctx
      | GeU -> BitVector.mk_uge ctx
      | GeS -> BitVector.mk_sge ctx
    in
    encode_bool to_bv (op' e1 e2)

  let encode_cvtop (op : cvtop) (e : Expr.expr) : Expr.expr =
    let op' =
      match op with
      | ExtendSI32 -> BitVector.mk_sign_ext ctx 32
      | ExtendUI32 -> BitVector.mk_zero_ext ctx 32
      (* rounding towards zero (aka truncation) *)
      | TruncSF32 -> fun f -> FloatingPoint.mk_to_sbv ctx rtz f 64
      | TruncUF32 -> fun f -> FloatingPoint.mk_to_ubv ctx rtz f 64
      | TruncSF64 -> fun f -> FloatingPoint.mk_to_sbv ctx rtz f 64
      | TruncUF64 -> fun f -> FloatingPoint.mk_to_ubv ctx rtz f 64
      | ReinterpretFloat -> FloatingPoint.mk_to_ieee_bv ctx
      | WrapI64 -> assert false
    in
    op' e
end

module F32Z3Op = struct
  open F32

  let encode_num (f : Int32.t) : Expr.expr =
    FloatingPoint.mk_numeral_f ctx (Int32.float_of_bits f) fp32_sort

  let encode_unop (op : unop) (e : Expr.expr) : Expr.expr =
    let op' =
      match op with
      | Neg -> FloatingPoint.mk_neg ctx
      | Abs -> FloatingPoint.mk_abs ctx
      | Sqrt -> FloatingPoint.mk_sqrt ctx rne
      | Nearest -> FloatingPoint.mk_round_to_integral ctx rne
    in
    op' e

  let encode_binop (op : binop) (e1 : Expr.expr) (e2 : Expr.expr) : Expr.expr =
    let op' =
      match op with
      | Add -> FloatingPoint.mk_add ctx rne
      | Sub -> FloatingPoint.mk_sub ctx rne
      | Mul -> FloatingPoint.mk_mul ctx rne
      | Div -> FloatingPoint.mk_div ctx rne
      | Min -> FloatingPoint.mk_min ctx
      | Max -> FloatingPoint.mk_max ctx
    in
    op' e1 e2

  let encode_relop ?(to_bv = false) (op : relop) (e1 : Expr.expr)
      (e2 : Expr.expr) : Expr.expr =
    let op' =
      match op with
      | Eq -> FloatingPoint.mk_eq ctx
      | Ne -> fun x1 x2 -> Boolean.mk_not ctx (FloatingPoint.mk_eq ctx x1 x2)
      | Lt -> FloatingPoint.mk_lt ctx
      | Le -> FloatingPoint.mk_leq ctx
      | Gt -> FloatingPoint.mk_gt ctx
      | Ge -> FloatingPoint.mk_geq ctx
    in
    encode_bool to_bv (op' e1 e2)

  let encode_cvtop (op : cvtop) (e : Expr.expr) : Expr.expr =
    let op' =
      match op with
      | DemoteF64 -> fun bv -> FloatingPoint.mk_to_fp_float ctx rne bv fp32_sort
      | ConvertSI32 ->
          fun bv -> FloatingPoint.mk_to_fp_signed ctx rne bv fp32_sort
      | ConvertUI32 ->
          fun bv -> FloatingPoint.mk_to_fp_unsigned ctx rne bv fp32_sort
      | ConvertSI64 ->
          fun bv -> FloatingPoint.mk_to_fp_signed ctx rne bv fp32_sort
      | ConvertUI64 ->
          fun bv -> FloatingPoint.mk_to_fp_unsigned ctx rne bv fp32_sort
      | ReinterpretInt -> fun bv -> FloatingPoint.mk_to_fp_bv ctx bv fp32_sort
      | PromoteF32 -> assert false
    in
    op' e
end

module F64Z3Op = struct
  open F64

  let encode_num (f : Int64.t) : Expr.expr =
    FloatingPoint.mk_numeral_f ctx (Int64.float_of_bits f) fp64_sort

  let encode_unop (op : unop) (e : Expr.expr) : Expr.expr =
    let op' =
      match op with
      | Neg -> FloatingPoint.mk_neg ctx
      | Abs -> FloatingPoint.mk_abs ctx
      | Sqrt -> FloatingPoint.mk_sqrt ctx rne
      | Nearest -> FloatingPoint.mk_round_to_integral ctx rne
    in
    op' e

  let encode_binop (op : binop) (e1 : Expr.expr) (e2 : Expr.expr) : Expr.expr =
    let op' =
      match op with
      | Add -> FloatingPoint.mk_add ctx rne
      | Sub -> FloatingPoint.mk_sub ctx rne
      | Mul -> FloatingPoint.mk_mul ctx rne
      | Div -> FloatingPoint.mk_div ctx rne
      | Min -> FloatingPoint.mk_min ctx
      | Max -> FloatingPoint.mk_max ctx
    in
    op' e1 e2

  let encode_relop ?(to_bv = false) (op : relop) (e1 : Expr.expr)
      (e2 : Expr.expr) : Expr.expr =
    let op' =
      match op with
      | Eq -> FloatingPoint.mk_eq ctx
      | Ne -> fun x1 x2 -> Boolean.mk_not ctx (FloatingPoint.mk_eq ctx x1 x2)
      | Lt -> FloatingPoint.mk_lt ctx
      | Le -> FloatingPoint.mk_leq ctx
      | Gt -> FloatingPoint.mk_gt ctx
      | Ge -> FloatingPoint.mk_geq ctx
    in
    encode_bool to_bv (op' e1 e2)

  let encode_cvtop (op : cvtop) (e : Expr.expr) : Expr.expr =
    let op' =
      match op with
      | PromoteF32 ->
          fun bv -> FloatingPoint.mk_to_fp_float ctx rne bv fp64_sort
      | ConvertSI32 ->
          fun bv -> FloatingPoint.mk_to_fp_signed ctx rne bv fp64_sort
      | ConvertUI32 ->
          fun bv -> FloatingPoint.mk_to_fp_unsigned ctx rne bv fp64_sort
      | ConvertSI64 ->
          fun bv -> FloatingPoint.mk_to_fp_signed ctx rne bv fp64_sort
      | ConvertUI64 ->
          fun bv -> FloatingPoint.mk_to_fp_unsigned ctx rne bv fp64_sort
      | ReinterpretInt -> fun bv -> FloatingPoint.mk_to_fp_bv ctx bv fp64_sort
      | DemoteF64 -> assert false
    in
    op' e
end

let num i i32 i64 f32 f64 : Num.t -> Expr.expr = function
  | Int x -> i x
  | I32 x -> i32 x
  | I64 x -> i64 x
  | F32 x -> f32 x
  | F64 x -> f64 x

let op i s i32 i64 f32 f64 = function
  | Int x -> i x
  | Str x -> s x
  | I32 x -> i32 x
  | I64 x -> i64 x
  | F32 x -> f32 x
  | F64 x -> f64 x

let encode_num =
  num IntZ3Op.encode_num I32Z3Op.encode_num I64Z3Op.encode_num
    F32Z3Op.encode_num F64Z3Op.encode_num

let encode_unop =
  op IntZ3Op.encode_unop StrZ3Op.encode_unop I32Z3Op.encode_unop
    I64Z3Op.encode_unop F32Z3Op.encode_unop F64Z3Op.encode_unop

let encode_binop =
  op IntZ3Op.encode_binop StrZ3Op.encode_binop I32Z3Op.encode_binop
    I64Z3Op.encode_binop F32Z3Op.encode_binop F64Z3Op.encode_binop

let encode_relop ~to_bv =
  op IntZ3Op.encode_relop StrZ3Op.encode_relop
    (I32Z3Op.encode_relop ~to_bv)
    (I64Z3Op.encode_relop ~to_bv)
    (F32Z3Op.encode_relop ~to_bv)
    (F64Z3Op.encode_relop ~to_bv)

let encode_cvtop =
  op IntZ3Op.encode_cvtop StrZ3Op.encode_cvtop I32Z3Op.encode_cvtop
    I64Z3Op.encode_cvtop F32Z3Op.encode_cvtop F64Z3Op.encode_cvtop

let rec encode_expr ?(bool_to_bv = false) (e : Expression.t) : Expr.expr =
  let open Expression in
  match e with
  | Num v -> encode_num v
  | Str s -> StrZ3Op.encode_str s
  | SymPtr (base, offset) ->
      let base' = encode_num (I32 base) in
      let offset' = encode_expr offset in
      I32Z3Op.encode_binop I32.Add base' offset'
  | Unop (op, e) ->
      let e' = encode_expr e in
      encode_unop op e'
  | Binop (Int op, e1, e2) ->
      let e1' = encode_expr e1 and e2' = encode_expr e2 in
      encode_binop (Int op) e1' e2'
  | Binop (op, e1, e2) ->
      let e1' = encode_expr ~bool_to_bv:true e1
      and e2' = encode_expr ~bool_to_bv:true e2 in
      encode_binop op e1' e2'
  | Relop (Int op, e1, e2) ->
      let e1' = encode_expr e1 and e2' = encode_expr e2 in
      encode_relop ~to_bv:false (Int op) e1' e2'
  | Relop (op, e1, e2) ->
      let e1' = encode_expr ~bool_to_bv:true e1
      and e2' = encode_expr ~bool_to_bv:true e2 in
      encode_relop ~to_bv:bool_to_bv op e1' e2'
  | Cvtop (op, e) ->
      let e' = encode_expr e in
      encode_cvtop op e'
  | Symbolic (t, x) -> Expr.mk_const_s ctx x (get_sort t)
  | Extract (e, h, l) ->
      let e' = encode_expr ~bool_to_bv:true e in
      BitVector.mk_extract ctx ((h * 8) - 1) (l * 8) e'
  | Concat (e1, e2) ->
      let e1' = encode_expr e1 and e2' = encode_expr e2 in
      BitVector.mk_concat ctx e1' e2'

let rec encode_formula (a : Formula.t) : Expr.expr =
  let open Formula in
  match a with
  | True -> Boolean.mk_true ctx
  | False -> Boolean.mk_false ctx
  | Relop e -> encode_expr e
  | Not c -> Boolean.mk_not ctx (encode_formula c)
  | And (c1, c2) ->
      let c1' = encode_formula c1 and c2' = encode_formula c2 in
      Boolean.mk_and ctx [ c1'; c2' ]
  | Or (c1, c2) ->
      let c1' = encode_formula c1 and c2' = encode_formula c2 in
      Boolean.mk_or ctx [ c1'; c2' ]

let set (s : string) (i : int) (n : char) =
  let bs = Bytes.of_string s in
  Bytes.set bs i n;
  Bytes.to_string bs

let int64_of_int (i : Expr.expr) : int64 =
  assert (Expr.is_numeral i);
  Int64.of_string (Arithmetic.Integer.numeral_to_string i)

let int64_of_bv (bv : Expr.expr) : int64 =
  assert (Expr.is_numeral bv);
  Int64.of_string (BitVector.numeral_to_string bv)

let int64_of_fp (fp : Expr.expr) (ebits : int) (sbits : int) : int64 =
  assert (Expr.is_numeral fp);
  if FloatingPoint.is_numeral_nan ctx fp then
    if FloatingPoint.is_numeral_negative ctx fp then
      if sbits = 23 then Int64.of_int32 0xffc0_0000l else 0xfff8_0000_0000_0000L
    else if sbits = 23 then Int64.of_int32 0x7fc0_0000l
    else 0x7ff8_0000_0000_0000L
  else if FloatingPoint.is_numeral_inf ctx fp then
    if FloatingPoint.is_numeral_negative ctx fp then
      if sbits = 23 then Int64.of_int32 (Int32.bits_of_float (-.(1.0 /. 0.0)))
      else Int64.bits_of_float (-.(1.0 /. 0.0))
    else if sbits = 23 then Int64.of_int32 (Int32.bits_of_float (1.0 /. 0.0))
    else Int64.bits_of_float (1.0 /. 0.0)
  else if FloatingPoint.is_numeral_zero ctx fp then
    if FloatingPoint.is_numeral_negative ctx fp then
      if sbits = 23 then Int64.of_int32 0x8000_0000l else 0x8000_0000_0000_0000L
    else if sbits = 23 then Int64.of_int32 (Int32.bits_of_float 0.0)
    else Int64.bits_of_float 0.0
  else
    let fp = Expr.to_string fp in
    let fp = Caml.String.sub fp 4 (String.length fp - 5) in
    let fp_list =
      List.map ~f:(fun fp -> set fp 0 '0') (String.split ~on:' ' fp)
    in
    let bit_list = List.map ~f:(fun fp -> Int64.of_string fp) fp_list in
    let fp_sign = Int64.shift_left (List.nth_exn bit_list 0) (ebits + sbits)
    and exponent = Int64.shift_left (List.nth_exn bit_list 1) sbits
    and fraction = List.nth_exn bit_list 2 in
    Int64.(fp_sign lor (exponent lor fraction))

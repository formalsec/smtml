(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2024 formalsec *)
(* Written by the Smtml programmers *)

(* Adapted from: *)
(* - https://github.com/WebAssembly/spec/blob/main/interpreter/exec/ixx.ml, *)
(* - https://github.com/WebAssembly/spec/blob/main/interpreter/exec/fxx.ml, and *)
(* - https://github.com/WebAssembly/spec/blob/main/interpreter/exec *)

type op_type =
  [ `Unop of Ty.Unop.t
  | `Binop of Ty.Binop.t
  | `Relop of Ty.Relop.t
  | `Triop of Ty.Triop.t
  | `Cvtop of Ty.Cvtop.t
  | `Naryop of Ty.Naryop.t
  ]
[@@deriving show]

type type_error_info =
  { index : int
  ; value : Value.t
  ; ty : Ty.t
  ; op : op_type
  ; msg : string
  }
[@@deriving show]

type error_kind =
  [ `Divide_by_zero
  | `Conversion_to_integer
  | `Integer_overflow
  | `Index_out_of_bounds
  | `Invalid_format_conversion
  | `Unsupported_operator of op_type * Ty.t
  | `Unsupported_theory of Ty.t
  | `Type_error of type_error_info
  ]

let pp_error_kind fmt err =
  match err with
  | `Divide_by_zero -> Fmt.string fmt "Division by zero"
  | `Conversion_to_integer ->
    Fmt.string fmt "Failed to convert value to integer"
  | `Integer_overflow ->
    Fmt.string fmt "Integer overflow: result is outside the representable range"
  | `Index_out_of_bounds -> Fmt.string fmt "Index out of bounds"
  | `Invalid_format_conversion ->
    Fmt.string fmt "Invalid format conversion string"
  | `Unsupported_operator (op, ty) ->
    Fmt.pf fmt "The operator '%a' is not supported for type '%a'" pp_op_type op
      Ty.pp ty
  | `Unsupported_theory ty ->
    Fmt.pf fmt "The theory for type '%a' is not currently supported" Ty.pp ty
  | `Type_error { index; value; ty; op; _ } ->
    Fmt.pf fmt
      "@[<h>Type error: Argument %d of operation '%a' expected type '%a', but \
       received value '%a' instead.@]"
      index pp_op_type op Ty.pp ty Value.pp value

exception Eval_error of error_kind

exception Value of Ty.t

(* Exception helpers *)

let eval_error kind = raise (Eval_error kind)

let type_error n v ty op msg =
  eval_error (`Type_error { index = n; value = v; ty; op; msg })

let err_str n op ty_expected ty_actual =
  Fmt.str "Argument %d of %a expected type %a but got %a instead." n pp_op_type
    op Ty.pp ty_expected Ty.pp ty_actual

let raise_type_mismatch n op v expected_ty =
  let actual_ty = Value.type_of v in
  let msg = err_str n op expected_ty actual_ty in
  type_error n v expected_ty op msg

(* Coercion helpers *)

let[@inline] of_int n op v =
  match v with Value.Int x -> x | _ -> raise_type_mismatch n op v Ty_int

let[@inline] to_int x = Value.Int x

let[@inline] of_real n op v =
  match v with Value.Real x -> x | _ -> raise_type_mismatch n op v Ty_real

let[@inline] to_real x = Value.Real x

let[@inline] of_bool n op v =
  match v with
  | Value.True -> true
  | False -> false
  | _ -> raise_type_mismatch n op v Ty_bool

let[@inline] to_bool x = if x then Value.True else False

let[@inline] of_str n op v =
  match v with Value.Str x -> x | _ -> raise_type_mismatch n op v Ty_str

let[@inline] to_str x = Value.Str x

let[@inline] of_list n op v =
  match v with Value.List x -> x | _ -> raise_type_mismatch n op v Ty_list

let[@inline] of_bitv n op v =
  match v with Value.Bitv x -> x | _ -> raise_type_mismatch n op v (Ty_bitv 0)

let[@inline] int32_of_bitv n op v = of_bitv n op v |> Bitvector.to_int32

let[@inline] int64_of_bitv n op v = of_bitv n op v |> Bitvector.to_int64

let[@inline] to_bitv x = Value.Bitv x

let[@inline] bitv_of_int32 x = to_bitv (Bitvector.of_int32 x)

let[@inline] bitv_of_int64 x = to_bitv (Bitvector.of_int64 x)

let[@inline] of_fp32 n op v : int32 =
  match v with
  | Value.Num (F32 f) -> f
  | _ -> raise_type_mismatch n op v (Ty_fp 32)

let[@inline] to_fp32 (x : int32) = Value.Num (F32 x)

let[@inline] fp32_of_float (x : float) = to_fp32 (Int32.bits_of_float x)

let[@inline] of_fp64 n op v : int64 =
  match v with
  | Value.Num (F64 f) -> f
  | _ -> raise_type_mismatch n op v (Ty_fp 64)

let[@inline] to_fp64 (x : int64) = Value.Num (F64 x)

let[@inline] fp64_of_float (x : float) = to_fp64 (Int64.bits_of_float x)

(* Operator evaluation *)

module Int = struct
  let[@inline] unop (op : Ty.Unop.t) (v : Value.t) : Value.t =
    let v = of_int 1 (`Unop op) v in
    match op with
    | Neg -> to_int (Int.neg v)
    | Not -> to_int (Int.lognot v)
    | Abs -> to_int (Int.abs v)
    | _ -> eval_error (`Unsupported_operator (`Unop op, Ty_int))

  let exp_by_squaring x n =
    let rec exp_by_squaring2 y x n =
      if n < 0 then exp_by_squaring2 y (1 / x) ~-n
      else if n = 0 then y
      else if n mod 2 = 0 then exp_by_squaring2 y (x * x) (n / 2)
      else begin
        assert (n mod 2 = 1);
        exp_by_squaring2 (x * y) (x * x) ((n - 1) / 2)
      end
    in
    exp_by_squaring2 1 x n

  let[@inline] binop (op : Ty.Binop.t) (v1 : Value.t) (v2 : Value.t) : Value.t =
    let v1 = of_int 1 (`Binop op) v1 in
    let v2 = of_int 2 (`Binop op) v2 in
    match op with
    | Add -> to_int (Int.add v1 v2)
    | Sub -> to_int (Int.sub v1 v2)
    | Mul -> to_int (Int.mul v1 v2)
    | Div -> to_int (Int.div v1 v2)
    | Rem -> to_int (Int.rem v1 v2)
    | Pow -> to_int (exp_by_squaring v1 v2)
    | Min -> to_int (Int.min v1 v2)
    | Max -> to_int (Int.max v1 v2)
    | And -> to_int (Int.logand v1 v2)
    | Or -> to_int (Int.logor v1 v2)
    | Xor -> to_int (Int.logxor v1 v2)
    | Shl -> to_int (Int.shift_left v1 v2)
    | ShrL -> to_int (Int.shift_right_logical v1 v2)
    | ShrA -> to_int (Int.shift_right v1 v2)
    | _ -> eval_error (`Unsupported_operator (`Binop op, Ty_int))

  let[@inline] relop (op : Ty.Relop.t) (v1 : Value.t) (v2 : Value.t) : bool =
    let a = of_int 1 (`Relop op) v1 in
    let b = of_int 2 (`Relop op) v2 in
    match op with
    | Lt -> a < b
    | Le -> a <= b
    | Gt -> a > b
    | Ge -> a >= b
    | Eq -> Int.equal a b
    | Ne -> not (Int.equal a b)
    | _ -> eval_error (`Unsupported_operator (`Relop op, Ty_int))

  let[@inline] int_of_bool v =
    match v with Value.True -> 1 | False -> 0 | _ -> assert false

  let[@inline] cvtop (op : Ty.Cvtop.t) (v : Value.t) : Value.t =
    match op with
    | OfBool -> to_int (int_of_bool v)
    | Reinterpret_float -> Int (Int.of_float (of_real 1 (`Cvtop op) v))
    | _ -> eval_error (`Unsupported_operator (`Cvtop op, Ty_int))
end

module Real = struct
  let[@inline] unop (op : Ty.Unop.t) (v : Value.t) : Value.t =
    let v = of_real 1 (`Unop op) v in
    match op with
    | Neg -> to_real @@ Float.neg v
    | Abs -> to_real @@ Float.abs v
    | Sqrt -> to_real @@ Float.sqrt v
    | Nearest -> to_real @@ Float.round v
    | Ceil -> to_real @@ Float.ceil v
    | Floor -> to_real @@ Float.floor v
    | Trunc -> to_real @@ Float.trunc v
    | Is_nan -> if Float.is_nan v then Value.True else Value.False
    | _ -> eval_error (`Unsupported_operator (`Unop op, Ty_real))

  let[@inline] binop (op : Ty.Binop.t) (v1 : Value.t) (v2 : Value.t) : Value.t =
    let a = of_real 1 (`Binop op) v1 in
    let b = of_real 2 (`Binop op) v2 in
    match op with
    | Add -> to_real (Float.add a b)
    | Sub -> to_real (Float.sub a b)
    | Mul -> to_real (Float.mul a b)
    | Div -> to_real (Float.div a b)
    | Rem -> to_real (Float.rem a b)
    | Min -> to_real (Float.min a b)
    | Max -> to_real (Float.max a b)
    | Pow -> to_real (Float.pow a b)
    | _ -> eval_error (`Unsupported_operator (`Binop op, Ty_real))

  let[@inline] relop (op : Ty.Relop.t) (v1 : Value.t) (v2 : Value.t) : bool =
    let a = of_real 1 (`Relop op) v1 in
    let b = of_real 2 (`Relop op) v2 in
    match op with
    | Lt -> Float.Infix.(a < b)
    | Le -> Float.Infix.(a <= b)
    | Gt -> Float.Infix.(a > b)
    | Ge -> Float.Infix.(a >= b)
    | Eq -> Float.Infix.(a = b)
    | Ne -> Float.Infix.(a <> b)
    | _ -> eval_error (`Unsupported_operator (`Relop op, Ty_real))

  let[@inline] cvtop (op : Ty.Cvtop.t) (v : Value.t) : Value.t =
    let op' = `Cvtop op in
    match op with
    | ToString -> Str (Float.to_string (of_real 1 op' v))
    | OfString -> begin
      match float_of_string_opt (of_str 1 op' v) with
      | None -> eval_error `Invalid_format_conversion
      | Some v -> to_real v
    end
    | Reinterpret_int -> to_real (float_of_int (of_int 1 op' v))
    | Reinterpret_float -> to_int (Float.to_int (of_real 1 op' v))
    | _ -> eval_error (`Unsupported_operator (op', Ty_real))
end

module Bool = struct
  let[@inline] unop (op : Ty.Unop.t) v =
    let b = of_bool 1 (`Unop op) v in
    match op with
    | Not -> to_bool (not b)
    | _ -> eval_error (`Unsupported_operator (`Unop op, Ty_bool))

  let xor b1 b2 =
    match (b1, b2) with
    | true, true -> false
    | true, false -> true
    | false, true -> true
    | false, false -> false

  let[@inline] binop (op : Ty.Binop.t) v1 v2 =
    let a = of_bool 1 (`Binop op) v1 in
    let b = of_bool 2 (`Binop op) v2 in
    match op with
    | And -> to_bool (a && b)
    | Or -> to_bool (a || b)
    | Xor -> to_bool (xor a b)
    | _ -> eval_error (`Unsupported_operator (`Binop op, Ty_bool))

  let[@inline] triop (op : Ty.Triop.t) c v1 v2 =
    match op with
    | Ite -> ( match of_bool 1 (`Triop op) c with true -> v1 | false -> v2 )
    | _ -> eval_error (`Unsupported_operator (`Triop op, Ty_bool))

  let[@inline] relop (op : Ty.Relop.t) v1 v2 =
    match op with
    | Eq -> Value.equal v1 v2
    | Ne -> not (Value.equal v1 v2)
    | _ -> eval_error (`Unsupported_operator (`Relop op, Ty_bool))

  let[@inline] naryop (op : Ty.Naryop.t) vs =
    match op with
    | Logand ->
      let exists_false =
        let i = ref 0 in
        List.find_map
          (fun e ->
            incr i;
            let b = of_bool !i (`Naryop op) e in
            if not b then Some () else None )
          vs
      in
      if Option.is_some exists_false then Value.False else Value.True
    | Logor ->
      let exists_true =
        let i = ref 0 in
        List.find_map
          (fun e ->
            incr i;
            let b = of_bool !i (`Naryop op) e in
            if b then Some () else None )
          vs
      in
      if Option.is_some exists_true then Value.True else Value.False
    | _ -> eval_error (`Unsupported_operator (`Naryop op, Ty_bool))
end

module Str = struct
  let replace s t t' =
    let len_s = String.length s in
    let len_t = String.length t in
    let rec loop i =
      if i >= len_s then s
      else if i + len_t > len_s then s
      else if String.equal (String.sub s i len_t) t then
        let s' = Fmt.str "%s%s" (String.sub s 0 i) t' in
        let s'' = String.sub s (i + len_t) (len_s - i - len_t) in
        Fmt.str "%s%s" s' s''
      else loop (i + 1)
    in
    loop 0

  let indexof s sub start =
    let len_s = String.length s in
    let len_sub = String.length sub in
    let max_i = len_s - 1 in
    let rec loop i =
      if i > max_i then ~-1
      else if i + len_sub > len_s then ~-1
      else if String.equal sub (String.sub s i len_sub) then i
      else loop (i + 1)
    in
    if start <= 0 then loop 0 else loop start

  let contains s sub = if indexof s sub 0 < 0 then false else true

  let[@inline] unop (op : Ty.Unop.t) v =
    let str = of_str 1 (`Unop op) v in
    match op with
    | Length -> to_int (String.length str)
    | Trim -> to_str (String.trim str)
    | _ -> eval_error (`Unsupported_operator (`Unop op, Ty_str))

  let[@inline] binop (op : Ty.Binop.t) v1 v2 =
    let op' = `Binop op in
    let str = of_str 1 op' v1 in
    match op with
    | At -> begin
      let i = of_int 2 op' v2 in
      try to_str (Fmt.str "%c" (String.get str i))
      with Invalid_argument _ -> eval_error `Index_out_of_bounds
    end
    | String_prefix ->
      to_bool (String.starts_with ~prefix:str (of_str 2 op' v2))
    | String_suffix -> to_bool (String.ends_with ~suffix:str (of_str 2 op' v2))
    | String_contains -> to_bool (contains str (of_str 2 op' v2))
    | _ -> eval_error (`Unsupported_operator (op', Ty_str))

  let[@inline] triop (op : Ty.Triop.t) v1 v2 v3 =
    let op' = `Triop op in
    let str = of_str 1 op' v1 in
    match op with
    | String_extract -> begin
      let i = of_int 2 op' v2 in
      let len = of_int 3 op' v3 in
      try to_str (String.sub str i len)
      with Invalid_argument _ -> eval_error `Index_out_of_bounds
    end
    | String_replace ->
      let t = of_str 2 op' v2 in
      let t' = of_str 2 op' v3 in
      to_str (replace str t t')
    | String_index ->
      let t = of_str 2 op' v2 in
      let i = of_int 3 op' v3 in
      to_int (indexof str t i)
    | _ -> eval_error (`Unsupported_operator (`Triop op, Ty_str))

  let[@inline] relop (op : Ty.Relop.t) v1 v2 =
    let a = of_str 1 (`Relop op) v1 in
    let b = of_str 2 (`Relop op) v2 in
    let cmp = String.compare a b in
    match op with
    | Lt -> cmp < 0
    | Le -> cmp <= 0
    | Gt -> cmp > 0
    | Ge -> cmp >= 0
    | Eq -> cmp = 0
    | Ne -> cmp <> 0
    | _ -> eval_error (`Unsupported_operator (`Relop op, Ty_str))

  let[@inline] cvtop (op : Ty.Cvtop.t) v =
    let op' = `Cvtop op in
    match op with
    | String_to_code ->
      let str = of_str 1 op' v in
      to_int (Char.code str.[0])
    | String_from_code ->
      let code = of_int 1 op' v in
      to_str (String.make 1 (Char.chr code))
    | String_to_int -> begin
      let s = of_str 1 op' v in
      match int_of_string_opt s with
      | None -> eval_error `Invalid_format_conversion
      | Some x -> to_int x
    end
    | String_from_int -> to_str (string_of_int (of_int 1 op' v))
    | String_to_float -> begin
      let s = of_str 1 op' v in
      match float_of_string_opt s with
      | None -> eval_error `Invalid_format_conversion
      | Some f -> to_real f
    end
    | _ -> eval_error (`Unsupported_operator (`Cvtop op, Ty_str))

  let[@inline] naryop (op : Ty.Naryop.t) vs =
    let op' = `Naryop op in
    match op with
    | Concat ->
      let _, s =
        List.fold_left
          (fun (i, acc) v ->
            let s = of_str i op' v in
            (i + 1, String.cat acc s) )
          (0, "") vs
      in
      to_str s
    | _ -> eval_error (`Unsupported_operator (`Naryop op, Ty_str))
end

module Lst = struct
  let[@inline] unop (op : Ty.Unop.t) (v : Value.t) : Value.t =
    let lst = of_list 1 (`Unop op) v in
    match op with
    | Head -> begin
      (* FIXME: Exception handling *)
      match lst with
      | hd :: _tl -> hd
      | [] -> assert false
    end
    | Tail -> begin
      (* FIXME: Exception handling *)
      match lst with
      | _hd :: tl -> List tl
      | [] -> assert false
    end
    | Length -> to_int (List.length lst)
    | Reverse -> List (List.rev lst)
    | _ -> eval_error (`Unsupported_operator (`Unop op, Ty_list))

  let[@inline] binop (op : Ty.Binop.t) v1 v2 =
    let op' = `Binop op in
    match op with
    | At ->
      let lst = of_list 1 op' v1 in
      let i = of_int 2 op' v2 in
      (* TODO: change datastructure? *)
      begin match List.nth_opt lst i with
      | None -> eval_error `Index_out_of_bounds
      | Some v -> v
      end
    | List_cons -> List (v1 :: of_list 1 op' v2)
    | List_append -> List (of_list 1 op' v1 @ of_list 2 op' v2)
    | _ -> eval_error (`Unsupported_operator (`Binop op, Ty_list))

  let[@inline] triop (op : Ty.Triop.t) (v1 : Value.t) (v2 : Value.t)
    (v3 : Value.t) : Value.t =
    let op' = `Triop op in
    match op with
    | List_set ->
      let lst = of_list 1 op' v1 in
      let i = of_int 2 op' v2 in
      let rec set i lst v acc =
        match (i, lst) with
        | 0, _ :: tl -> List.rev_append acc (v :: tl)
        | i, hd :: tl -> set (i - 1) tl v (hd :: acc)
        | _, [] -> eval_error `Index_out_of_bounds
      in
      List (set i lst v3 [])
    | _ -> eval_error (`Unsupported_operator (`Triop op, Ty_list))

  let[@inline] naryop (op : Ty.Naryop.t) (vs : Value.t list) : Value.t =
    let op' = `Naryop op in
    match op with
    | Concat -> List (List.concat_map (of_list 0 op') vs)
    | _ -> eval_error (`Unsupported_operator (`Naryop op, Ty_list))
end

module I64 = struct
  let cmp_u x op y = op Int64.(add x min_int) Int64.(add y min_int) [@@inline]

  let lt_u x y = cmp_u x Int64.Infix.( < ) y [@@inline]
end

module Bitv = struct
  let[@inline] unop op bv =
    let bv = of_bitv 1 (`Unop op) bv in
    match op with
    | Ty.Unop.Neg -> to_bitv (Bitvector.neg bv)
    | Not -> to_bitv (Bitvector.lognot bv)
    | Clz -> to_bitv (Bitvector.clz bv)
    | Ctz -> to_bitv (Bitvector.ctz bv)
    | Popcnt -> to_bitv (Bitvector.popcnt bv)
    | _ ->
      eval_error
        (`Unsupported_operator (`Unop op, Ty_bitv (Bitvector.numbits bv)))

  let[@inline] binop op bv1 bv2 =
    let bv1 = of_bitv 1 (`Binop op) bv1 in
    let bv2 = of_bitv 2 (`Binop op) bv2 in
    match op with
    | Ty.Binop.Add -> to_bitv (Bitvector.add bv1 bv2)
    | Sub -> to_bitv (Bitvector.sub bv1 bv2)
    | Mul -> to_bitv (Bitvector.mul bv1 bv2)
    | Div -> to_bitv (Bitvector.div bv1 bv2)
    | DivU -> to_bitv (Bitvector.div_u bv1 bv2)
    | Rem -> to_bitv (Bitvector.rem bv1 bv2)
    | RemU -> to_bitv (Bitvector.rem_u bv1 bv2)
    | And -> to_bitv (Bitvector.logand bv1 bv2)
    | Or -> to_bitv (Bitvector.logor bv1 bv2)
    | Xor -> to_bitv (Bitvector.logxor bv1 bv2)
    | Shl -> to_bitv (Bitvector.shl bv1 bv2)
    | ShrL -> to_bitv (Bitvector.lshr bv1 bv2)
    | ShrA -> to_bitv (Bitvector.ashr bv1 bv2)
    | Rotl -> to_bitv (Bitvector.rotate_left bv1 bv2)
    | Rotr -> to_bitv (Bitvector.rotate_right bv1 bv2)
    | _ -> eval_error (`Unsupported_operator (`Binop op, Ty_bitv 0))

  let[@inline] relop op bv1 bv2 =
    let bv1 = of_bitv 1 (`Relop op) bv1 in
    let bv2 = of_bitv 2 (`Relop op) bv2 in
    match op with
    | Ty.Relop.Lt -> Bitvector.lt bv1 bv2
    | LtU -> Bitvector.lt_u bv1 bv2
    | Le -> Bitvector.le bv1 bv2
    | LeU -> Bitvector.le_u bv1 bv2
    | Gt -> Bitvector.gt bv1 bv2
    | GtU -> Bitvector.gt_u bv1 bv2
    | Ge -> Bitvector.ge bv1 bv2
    | GeU -> Bitvector.ge_u bv1 bv2
    | Eq -> Bitvector.equal bv1 bv2
    | Ne -> not @@ Bitvector.equal bv1 bv2

  let[@inline] cvtop op bv =
    let bv = of_bitv 1 (`Cvtop op) bv in
    match op with
    | Ty.Cvtop.Sign_extend m -> to_bitv (Bitvector.sign_extend m bv)
    | Ty.Cvtop.Zero_extend m -> to_bitv (Bitvector.zero_extend m bv)
    | _ ->
      eval_error
        (`Unsupported_operator (`Cvtop op, Ty_bitv (Bitvector.numbits bv)))
end

module F32 = struct
  (* Stolen from Owi *)
  let[@inline] abs x = Int32.logand x Int32.max_int

  let[@inline] neg x = Int32.logxor x Int32.min_int

  let[@inline] unop (op : Ty.Unop.t) (v : Value.t) : Value.t =
    let f = Int32.float_of_bits (of_fp32 1 (`Unop op) v) in
    match op with
    | Neg -> to_fp32 @@ neg @@ of_fp32 1 (`Unop op) v
    | Abs -> to_fp32 @@ abs @@ of_fp32 1 (`Unop op) v
    | Sqrt -> fp32_of_float @@ Float.sqrt f
    | Nearest -> fp32_of_float @@ Float.round f
    | Ceil -> fp32_of_float @@ Float.ceil f
    | Floor -> fp32_of_float @@ Float.floor f
    | Trunc -> fp32_of_float @@ Float.trunc f
    | Is_nan -> if Float.is_nan f then Value.True else Value.False
    | _ -> eval_error (`Unsupported_operator (`Unop op, Ty_fp 32))

  (* Stolen from Owi *)
  let[@inline] copy_sign x y =
    Int32.logor (abs x) (Int32.logand y Int32.min_int)

  let[@inline] binop (op : Ty.Binop.t) (v1 : Value.t) (v2 : Value.t) : Value.t =
    let a = Int32.float_of_bits @@ of_fp32 1 (`Binop op) v1 in
    let b = Int32.float_of_bits @@ of_fp32 1 (`Binop op) v2 in
    match op with
    | Add -> fp32_of_float @@ Float.add a b
    | Sub -> fp32_of_float @@ Float.sub a b
    | Mul -> fp32_of_float @@ Float.mul a b
    | Div -> fp32_of_float @@ Float.div a b
    | Rem -> fp32_of_float @@ Float.rem a b
    | Min -> fp32_of_float @@ Float.min a b
    | Max -> fp32_of_float @@ Float.max a b
    | Copysign ->
      let a = of_fp32 1 (`Binop op) v1 in
      let b = of_fp32 1 (`Binop op) v2 in
      to_fp32 (copy_sign a b)
    | _ -> eval_error (`Unsupported_operator (`Binop op, Ty_fp 32))

  let[@inline] relop (op : Ty.Relop.t) (v1 : Value.t) (v2 : Value.t) : bool =
    let a = Int32.float_of_bits @@ of_fp32 1 (`Relop op) v1 in
    let b = Int32.float_of_bits @@ of_fp32 2 (`Relop op) v2 in
    match op with
    | Eq -> Float.Infix.(a = b)
    | Ne -> Float.Infix.(a <> b)
    | Lt -> Float.Infix.(a < b)
    | Le -> Float.Infix.(a <= b)
    | Gt -> Float.Infix.(a > b)
    | Ge -> Float.Infix.(a >= b)
    | _ -> eval_error (`Unsupported_operator (`Relop op, Ty_fp 32))
end

module F64 = struct
  (* Stolen from owi *)
  let[@inline] abs x = Int64.logand x Int64.max_int

  let[@inline] neg x = Int64.logxor x Int64.min_int

  let[@inline] unop (op : Ty.Unop.t) (v : Value.t) : Value.t =
    let f = Int64.float_of_bits @@ of_fp64 1 (`Unop op) v in
    match op with
    | Neg -> to_fp64 @@ neg @@ of_fp64 1 (`Unop op) v
    | Abs -> to_fp64 @@ abs @@ of_fp64 1 (`Unop op) v
    | Sqrt -> fp64_of_float @@ Float.sqrt f
    | Nearest -> fp64_of_float @@ Float.round f
    | Ceil -> fp64_of_float @@ Float.ceil f
    | Floor -> fp64_of_float @@ Float.floor f
    | Trunc -> fp64_of_float @@ Float.trunc f
    | Is_nan -> if Float.is_nan f then Value.True else Value.False
    | _ -> Fmt.failwith {|unop: Unsupported f32 operator "%a"|} Ty.Unop.pp op

  let copy_sign x y = Int64.logor (abs x) (Int64.logand y Int64.min_int)

  let[@inline] binop (op : Ty.Binop.t) (v1 : Value.t) (v2 : Value.t) : Value.t =
    let a = Int64.float_of_bits @@ of_fp64 1 (`Binop op) v1 in
    let b = Int64.float_of_bits @@ of_fp64 2 (`Binop op) v2 in
    match op with
    | Add -> fp64_of_float @@ Float.add a b
    | Sub -> fp64_of_float @@ Float.sub a b
    | Mul -> fp64_of_float @@ Float.mul a b
    | Div -> fp64_of_float @@ Float.div a b
    | Rem -> fp64_of_float @@ Float.rem a b
    | Min -> fp64_of_float @@ Float.min a b
    | Max -> fp64_of_float @@ Float.max a b
    | Copysign ->
      let a = of_fp64 1 (`Binop op) v1 in
      let b = of_fp64 2 (`Binop op) v2 in
      to_fp64 @@ copy_sign a b
    | _ -> eval_error (`Unsupported_operator (`Binop op, Ty_fp 64))

  let[@inline] relop (op : Ty.Relop.t) (v1 : Value.t) (v2 : Value.t) : bool =
    let a = Int64.float_of_bits @@ of_fp64 1 (`Relop op) v1 in
    let b = Int64.float_of_bits @@ of_fp64 2 (`Relop op) v2 in
    match op with
    | Eq -> Float.Infix.(a = b)
    | Ne -> Float.Infix.(a <> b)
    | Lt -> Float.Infix.(a < b)
    | Le -> Float.Infix.(a <= b)
    | Gt -> Float.Infix.(a > b)
    | Ge -> Float.Infix.(a >= b)
    | _ -> eval_error (`Unsupported_operator (`Relop op, Ty_fp 64))
end

module I32CvtOp = struct
  let trunc_f32_s (x : int32) =
    if Int32.Infix.(x <> x) then eval_error `Conversion_to_integer
    else
      let xf = Int32.float_of_bits x in
      if
        Float.Infix.(
          xf >= -.Int32.(to_float min_int) || xf < Int32.(to_float min_int) )
      then eval_error `Integer_overflow
      else Int32.of_float xf

  let trunc_f32_u (x : int32) =
    if Int32.Infix.(x <> x) then eval_error `Conversion_to_integer
    else
      let xf = Int32.float_of_bits x in
      if Float.Infix.(xf >= -.Int32.(to_float min_int) *. 2.0 || xf <= -1.0)
      then eval_error `Integer_overflow
      else Int32.of_float xf

  let trunc_f64_s (x : int64) =
    if Int64.Infix.(x <> x) then eval_error `Conversion_to_integer
    else
      let xf = Int64.float_of_bits x in
      if
        Float.Infix.(
          xf >= -.Int64.(to_float min_int) || xf < Int64.(to_float min_int) )
      then eval_error `Integer_overflow
      else Int32.of_float xf

  let trunc_f64_u (x : int64) =
    if Int64.Infix.(x <> x) then eval_error `Conversion_to_integer
    else
      let xf = Int64.float_of_bits x in
      if Float.Infix.(xf >= -.Int64.(to_float min_int) *. 2.0 || xf <= -1.0)
      then eval_error `Integer_overflow
      else Int32.of_float xf

  let trunc_sat_f32_s x =
    if Int32.Infix.(x <> x) then 0l
    else
      let xf = Int32.float_of_bits x in
      if Float.Infix.(xf < Int32.(to_float min_int)) then Int32.min_int
      else if Float.Infix.(xf >= -.Int32.(to_float min_int)) then Int32.max_int
      else Int32.of_float xf

  let trunc_sat_f32_u x =
    if Int32.Infix.(x <> x) then 0l
    else
      let xf = Int32.float_of_bits x in
      if Float.Infix.(xf <= -1.0) then 0l
      else if Float.Infix.(xf >= -.Int32.(to_float min_int) *. 2.0) then -1l
      else Int32.of_float xf

  let trunc_sat_f64_s x =
    if Int64.Infix.(x <> x) then 0l
    else
      let xf = Int64.float_of_bits x in
      if Float.Infix.(xf < Int64.(to_float min_int)) then Int32.min_int
      else if Float.Infix.(xf >= -.Int64.(to_float min_int)) then Int32.max_int
      else Int32.of_float xf

  let trunc_sat_f64_u x =
    if Int64.Infix.(x <> x) then 0l
    else
      let xf = Int64.float_of_bits x in
      if Float.Infix.(xf <= -1.0) then 0l
      else if Float.Infix.(xf >= -.Int64.(to_float min_int) *. 2.0) then -1l
      else Int32.of_float xf

  let cvtop op v =
    let op' = `Cvtop op in
    match op with
    | Ty.Cvtop.WrapI64 -> bitv_of_int32 (Int64.to_int32 (int64_of_bitv 1 op' v))
    | TruncSF32 -> bitv_of_int32 (trunc_f32_s (of_fp32 1 op' v))
    | TruncUF32 -> bitv_of_int32 (trunc_f32_u (of_fp32 1 op' v))
    | TruncSF64 -> bitv_of_int32 (trunc_f64_s (of_fp64 1 op' v))
    | TruncUF64 -> bitv_of_int32 (trunc_f64_u (of_fp64 1 op' v))
    | Trunc_sat_f32_s -> bitv_of_int32 (trunc_sat_f32_s (of_fp32 1 op' v))
    | Trunc_sat_f32_u -> bitv_of_int32 (trunc_sat_f32_u (of_fp32 1 op' v))
    | Trunc_sat_f64_s -> bitv_of_int32 (trunc_sat_f64_s (of_fp64 1 op' v))
    | Trunc_sat_f64_u -> bitv_of_int32 (trunc_sat_f64_u (of_fp64 1 op' v))
    | Reinterpret_float -> bitv_of_int32 (of_fp32 1 op' v)
    | Sign_extend n -> to_bitv (Bitvector.sign_extend n (of_bitv 1 op' v))
    | Zero_extend n -> to_bitv (Bitvector.zero_extend n (of_bitv 1 op' v))
    | OfBool -> v (* v is already a number here *)
    | ToBool | _ -> eval_error (`Unsupported_operator (op', Ty_bitv 32))
end

module I64CvtOp = struct
  let extend_i32_u (x : int32) =
    Int64.(logand (of_int32 x) 0x0000_0000_ffff_ffffL)

  let trunc_f32_s (x : int32) =
    if Int32.Infix.(x <> x) then eval_error `Conversion_to_integer
    else
      let xf = Int32.float_of_bits x in
      if
        Float.Infix.(
          xf >= -.Int64.(to_float min_int) || xf < Int64.(to_float min_int) )
      then eval_error `Integer_overflow
      else Int64.of_float xf

  let trunc_f32_u (x : int32) =
    if Int32.Infix.(x <> x) then eval_error `Conversion_to_integer
    else
      let xf = Int32.float_of_bits x in
      if Float.Infix.(xf >= -.Int64.(to_float min_int) *. 2.0 || xf <= -1.0)
      then eval_error `Integer_overflow
      else if Float.Infix.(xf >= -.Int64.(to_float min_int)) then
        Int64.(logxor (of_float (xf -. 0x1p63)) min_int)
      else Int64.of_float xf

  let trunc_f64_s (x : int64) =
    if Int64.Infix.(x <> x) then eval_error `Conversion_to_integer
    else
      let xf = Int64.float_of_bits x in
      if
        Float.Infix.(
          xf >= -.Int64.(to_float min_int) || xf < Int64.(to_float min_int) )
      then eval_error `Integer_overflow
      else Int64.of_float xf

  let trunc_f64_u (x : int64) =
    if Int64.Infix.(x <> x) then eval_error `Conversion_to_integer
    else
      let xf = Int64.float_of_bits x in
      if Float.Infix.(xf >= -.Int64.(to_float min_int) *. 2.0 || xf <= -1.0)
      then eval_error `Integer_overflow
      else if Float.Infix.(xf >= -.Int64.(to_float min_int)) then
        Int64.(logxor (of_float (xf -. 0x1p63)) min_int)
      else Int64.of_float xf

  let trunc_sat_f32_s (x : int32) =
    if Int32.Infix.(x <> x) then 0L
    else
      let xf = Int32.float_of_bits x in
      if Float.Infix.(xf < Int64.(to_float min_int)) then Int64.min_int
      else if Float.Infix.(xf >= -.Int64.(to_float min_int)) then Int64.max_int
      else Int64.of_float xf

  let trunc_sat_f32_u (x : int32) =
    if Int32.Infix.(x <> x) then 0L
    else
      let xf = Int32.float_of_bits x in
      if Float.Infix.(xf <= -1.0) then 0L
      else if Float.Infix.(xf >= -.Int64.(to_float min_int) *. 2.0) then -1L
      else if Float.Infix.(xf >= -.Int64.(to_float min_int)) then
        Int64.(logxor (of_float (xf -. 0x1p63)) min_int)
      else Int64.of_float xf

  let trunc_sat_f64_s (x : int64) =
    if Int64.Infix.(x <> x) then 0L
    else
      let xf = Int64.float_of_bits x in
      if Float.Infix.(xf < Int64.(to_float min_int)) then Int64.min_int
      else if Float.Infix.(xf >= -.Int64.(to_float min_int)) then Int64.max_int
      else Int64.of_float xf

  let trunc_sat_f64_u (x : int64) =
    if Int64.Infix.(x <> x) then 0L
    else
      let xf = Int64.float_of_bits x in
      if Float.Infix.(xf <= -1.0) then 0L
      else if Float.Infix.(xf >= -.Int64.(to_float min_int) *. 2.0) then -1L
      else if Float.Infix.(xf >= -.Int64.(to_float min_int)) then
        Int64.(logxor (of_float (xf -. 0x1p63)) min_int)
      else Int64.of_float xf

  let cvtop (op : Ty.Cvtop.t) (v : Value.t) : Value.t =
    let op' = `Cvtop op in
    match op with
    | Sign_extend n -> to_bitv (Bitvector.sign_extend n (of_bitv 1 op' v))
    | Zero_extend n -> to_bitv (Bitvector.zero_extend n (of_bitv 1 op' v))
    | TruncSF32 -> bitv_of_int64 (trunc_f32_s (of_fp32 1 op' v))
    | TruncUF32 -> bitv_of_int64 (trunc_f32_u (of_fp32 1 op' v))
    | TruncSF64 -> bitv_of_int64 (trunc_f64_s (of_fp64 1 op' v))
    | TruncUF64 -> bitv_of_int64 (trunc_f64_u (of_fp64 1 op' v))
    | Trunc_sat_f32_s -> bitv_of_int64 (trunc_sat_f32_s (of_fp32 1 op' v))
    | Trunc_sat_f32_u -> bitv_of_int64 (trunc_sat_f32_u (of_fp32 1 op' v))
    | Trunc_sat_f64_s -> bitv_of_int64 (trunc_sat_f64_s (of_fp64 1 op' v))
    | Trunc_sat_f64_u -> bitv_of_int64 (trunc_sat_f64_u (of_fp64 1 op' v))
    | Reinterpret_float -> bitv_of_int64 (of_fp64 1 op' v)
    | WrapI64 -> type_error 1 v (Ty_bitv 64) op' "Cannot wrapI64 on an I64"
    | ToBool | OfBool | _ -> eval_error (`Unsupported_operator (op', Ty_bitv 64))
end

module F32CvtOp = struct
  let demote_f64 x =
    let xf = Int64.float_of_bits x in
    if Float.Infix.(xf = xf) then Int32.bits_of_float xf
    else
      let nan64bits = x in
      let sign_field =
        Int64.(shift_left (shift_right_logical nan64bits 63) 31)
      in
      let significand_field =
        Int64.(shift_right_logical (shift_left nan64bits 12) 41)
      in
      let fields = Int64.logor sign_field significand_field in
      Int32.logor 0x7fc0_0000l (Int64.to_int32 fields)

  let convert_i32_s x = Int32.bits_of_float (Int32.to_float x)

  let convert_i32_u x =
    Int32.bits_of_float
      Int32.(
        Int32.Infix.(
          if x >= 0l then to_float x
          else to_float (logor (shift_right_logical x 1) (logand x 1l)) *. 2.0 ) )

  let convert_i64_s x =
    Int32.bits_of_float
      Int64.(
        Int64.Infix.(
          if abs x < 0x10_0000_0000_0000L then to_float x
          else
            let r = if logand x 0xfffL = 0L then 0L else 1L in
            to_float (logor (shift_right x 12) r) *. 0x1p12 ) )

  let convert_i64_u x =
    Int32.bits_of_float
      Int64.(
        Int64.Infix.(
          if I64.lt_u x 0x10_0000_0000_0000L then to_float x
          else
            let r = if logand x 0xfffL = 0L then 0L else 1L in
            to_float (logor (shift_right_logical x 12) r) *. 0x1p12 ) )

  let cvtop (op : Ty.Cvtop.t) (v : Value.t) : Value.t =
    let op' = `Cvtop op in
    match op with
    | DemoteF64 -> to_fp32 (demote_f64 (of_fp64 1 op' v))
    | ConvertSI32 -> to_fp32 (convert_i32_s (int32_of_bitv 1 op' v))
    | ConvertUI32 -> to_fp32 (convert_i32_u (int32_of_bitv 1 op' v))
    | ConvertSI64 -> to_fp32 (convert_i64_s (int64_of_bitv 1 op' v))
    | ConvertUI64 -> to_fp32 (convert_i64_u (int64_of_bitv 1 op' v))
    | Reinterpret_int -> to_fp32 (int32_of_bitv 1 op' v)
    | PromoteF32 -> type_error 1 v (Ty_fp 32) op' "F64 must promote F32"
    | ToString | OfString | _ ->
      eval_error (`Unsupported_operator (op', Ty_fp 32))
end

module F64CvtOp = struct
  let promote_f32 x =
    let xf = Int32.float_of_bits x in
    if Float.Infix.(xf = xf) then Int64.bits_of_float xf
    else
      let nan32bits = I64CvtOp.extend_i32_u x in
      let sign_field =
        Int64.(shift_left (shift_right_logical nan32bits 31) 63)
      in
      let significand_field =
        Int64.(shift_right_logical (shift_left nan32bits 41) 12)
      in
      let fields = Int64.logor sign_field significand_field in
      Int64.logor 0x7ff8_0000_0000_0000L fields

  let convert_i32_s x = Int64.bits_of_float (Int32.to_float x)

  (*
   * Unlike the other convert_u functions, the high half of the i32 range is
   * within the range where f32 can represent odd numbers, so we can't do the
   * shift. Instead, we can use int64 signed arithmetic.
   *)
  let convert_i32_u x =
    Int64.bits_of_float
      Int64.(to_float (logand (of_int32 x) 0x0000_0000_ffff_ffffL))

  let convert_i64_s x = Int64.bits_of_float (Int64.to_float x)

  (*
   * Values in the low half of the int64 range can be converted with a signed
   * conversion. The high half is beyond the range where f64 can represent odd
   * numbers, so we can shift the value right, adjust the least significant
   * bit to round correctly, do a conversion, and then scale it back up.
   *)
  let convert_i64_u (x : int64) =
    Int64.bits_of_float
      Int64.(
        Int64.Infix.(
          if x >= 0L then to_float x
          else to_float (logor (shift_right_logical x 1) (logand x 1L)) *. 2.0 ) )

  let cvtop (op : Ty.Cvtop.t) v : Value.t =
    let op' = `Cvtop op in
    match op with
    | PromoteF32 -> to_fp64 (promote_f32 (of_fp32 1 op' v))
    | ConvertSI32 -> to_fp64 (convert_i32_s (int32_of_bitv 1 op' v))
    | ConvertUI32 -> to_fp64 (convert_i32_u (int32_of_bitv 1 op' v))
    | ConvertSI64 -> to_fp64 (convert_i64_s (int64_of_bitv 1 op' v))
    | ConvertUI64 -> to_fp64 (convert_i64_u (int64_of_bitv 1 op' v))
    | Reinterpret_int -> to_fp64 (int64_of_bitv 1 op' v)
    | DemoteF64 -> type_error 1 v (Ty_fp 64) op' "F32 must demote a F64"
    | ToString | OfString | _ ->
      eval_error (`Unsupported_operator (op', Ty_fp 64))
end

(* Dispatch *)

let unop ty op v =
  match ty with
  | Ty.Ty_int -> Int.unop op v
  | Ty_real -> Real.unop op v
  | Ty_bool -> Bool.unop op v
  | Ty_str -> Str.unop op v
  | Ty_list -> Lst.unop op v
  | Ty_bitv _ -> Bitv.unop op v
  | Ty_fp 32 -> F32.unop op v
  | Ty_fp 64 -> F64.unop op v
  | Ty_fp _ | Ty_app | Ty_unit | Ty_none | Ty_regexp | Ty_roundingMode ->
    eval_error (`Unsupported_theory ty)

let binop ty op v1 v2 =
  match ty with
  | Ty.Ty_int -> Int.binop op v1 v2
  | Ty_real -> Real.binop op v1 v2
  | Ty_bool -> Bool.binop op v1 v2
  | Ty_str -> Str.binop op v1 v2
  | Ty_list -> Lst.binop op v1 v2
  | Ty_bitv _ -> Bitv.binop op v1 v2
  | Ty_fp 32 -> F32.binop op v1 v2
  | Ty_fp 64 -> F64.binop op v1 v2
  | Ty_fp _ | Ty_app | Ty_unit | Ty_none | Ty_regexp | Ty_roundingMode ->
    eval_error (`Unsupported_theory ty)

let triop ty op v1 v2 v3 =
  match ty with
  | Ty.Ty_bool -> Bool.triop op v1 v2 v3
  | Ty_str -> Str.triop op v1 v2 v3
  | Ty_list -> Lst.triop op v1 v2 v3
  | ty -> eval_error (`Unsupported_theory ty)

let relop ty op v1 v2 =
  match ty with
  | Ty.Ty_int -> Int.relop op v1 v2
  | Ty_real -> Real.relop op v1 v2
  | Ty_bool -> Bool.relop op v1 v2
  | Ty_str -> Str.relop op v1 v2
  | Ty_bitv _ -> Bitv.relop op v1 v2
  | Ty_fp 32 -> F32.relop op v1 v2
  | Ty_fp 64 -> F64.relop op v1 v2
  | ty -> eval_error (`Unsupported_theory ty)

let cvtop ty op v =
  match ty with
  | Ty.Ty_int -> Int.cvtop op v
  | Ty_real -> Real.cvtop op v
  | Ty_str -> Str.cvtop op v
  | Ty_bitv 32 -> I32CvtOp.cvtop op v
  | Ty_bitv 64 -> I64CvtOp.cvtop op v
  (* Remaining fall into arbitrary-width bv cvtop operations *)
  | Ty_bitv _m -> Bitv.cvtop op v
  | Ty_fp 32 -> F32CvtOp.cvtop op v
  | Ty_fp 64 -> F64CvtOp.cvtop op v
  | ty -> eval_error (`Unsupported_theory ty)

let naryop ty op vs =
  match ty with
  | Ty.Ty_bool -> Bool.naryop op vs
  | Ty_str -> Str.naryop op vs
  | Ty_list -> Lst.naryop op vs
  | ty -> eval_error (`Unsupported_theory ty)

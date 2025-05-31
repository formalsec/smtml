(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2024 formalsec *)
(* Written by the Smtml programmers *)

(* Adapted from: *)
(* - https://github.com/WebAssembly/spec/blob/main/interpreter/exec/ixx.ml, *)
(* - https://github.com/WebAssembly/spec/blob/main/interpreter/exec/fxx.ml, and *)
(* - https://github.com/WebAssembly/spec/blob/main/interpreter/exec *)

(* TODO: This module should be concrete or a part of the reducer *)

type op_type =
  [ `Unop of Ty.Unop.t
  | `Binop of Ty.Binop.t
  | `Relop of Ty.Relop.t
  | `Triop of Ty.Triop.t
  | `Cvtop of Ty.Cvtop.t
  | `Naryop of Ty.Naryop.t
  ]

let pp_op_type fmt = function
  | `Unop op -> Fmt.pf fmt "unop '%a'" Ty.Unop.pp op
  | `Binop op -> Fmt.pf fmt "binop '%a'" Ty.Binop.pp op
  | `Relop op -> Fmt.pf fmt "relop '%a'" Ty.Relop.pp op
  | `Triop op -> Fmt.pf fmt "triop '%a'" Ty.Triop.pp op
  | `Cvtop op -> Fmt.pf fmt "cvtop '%a'" Ty.Cvtop.pp op
  | `Naryop op -> Fmt.pf fmt "naryop '%a'" Ty.Naryop.pp op

exception Value of Ty.t

(* FIXME: use snake case instead *)
exception
  TypeError of
    { index : int
    ; value : Value.t
    ; ty : Ty.t
    ; op : op_type
    ; msg : string
    }

(* FIXME: use snake case instead *)
exception DivideByZero

exception Conversion_to_integer

exception Integer_overflow

(* FIXME: use snake case instead *)
exception Index_out_of_bounds

let of_arg f n v op msg =
  try f v
  with Value t -> raise (TypeError { index = n; value = v; ty = t; op; msg })
[@@inline]

let err_str n op ty_expected ty_actual =
  Fmt.str "Argument %d of %a expected type %a but got %a instead." n pp_op_type
    op Ty.pp ty_expected Ty.pp ty_actual

module Int = struct
  let to_value (i : int) : Value.t = Int i [@@inline]

  let of_value (n : int) (op : op_type) (v : Value.t) : int =
    of_arg
      (function Int i -> i | _ -> raise_notrace (Value Ty_int))
      n v op
      (err_str n op Ty_int (Value.type_of v))
  [@@inline]

  let str_value (n : int) (op : op_type) (v : Value.t) : string =
    of_arg
      (function Str str -> str | _ -> raise_notrace (Value Ty_str))
      n v op
      (err_str n op Ty_str (Value.type_of v))

  let unop (op : Ty.Unop.t) (v : Value.t) : Value.t =
    let f =
      match op with
      | Neg -> Int.neg
      | Not -> Int.lognot
      | Abs -> Int.abs
      | _ -> Fmt.failwith {|unop: Unsupported int operator "%a"|} Ty.Unop.pp op
    in
    to_value (f (of_value 1 (`Unop op) v))

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

  let binop (op : Ty.Binop.t) (v1 : Value.t) (v2 : Value.t) : Value.t =
    let f =
      match op with
      | Add -> Int.add
      | Sub -> Int.sub
      | Mul -> Int.mul
      | Div -> Int.div
      | Rem -> Int.rem
      | Pow -> exp_by_squaring
      | Min -> Int.min
      | Max -> Int.max
      | And -> Int.logand
      | Or -> Int.logor
      | Xor -> Int.logxor
      | Shl -> Int.shift_left
      | ShrL -> Int.shift_right_logical
      | ShrA -> Int.shift_right
      | _ ->
        Fmt.failwith {|binop: Unsupported int operator "%a"|} Ty.Binop.pp op
    in
    to_value (f (of_value 1 (`Binop op) v1) (of_value 2 (`Binop op) v2))

  let relop (op : Ty.Relop.t) (v1 : Value.t) (v2 : Value.t) : bool =
    let f =
      match op with
      | Lt -> ( < )
      | Le -> ( <= )
      | Gt -> ( > )
      | Ge -> ( >= )
      | _ ->
        Fmt.failwith {|relop: Unsupported int operator "%a"|} Ty.Relop.pp op
    in
    f (of_value 1 (`Relop op) v1) (of_value 2 (`Relop op) v2)

  let of_bool : Value.t -> int = function
    | True -> 1
    | False -> 0
    | _ -> assert false
  [@@inline]

  let cvtop (op : Ty.Cvtop.t) (v : Value.t) : Value.t =
    match op with
    | OfBool -> to_value (of_bool v)
    | Reinterpret_float ->
      Int (Int.of_float (match v with Real v -> v | _ -> assert false))
    | ToString -> Str (string_of_int (of_value 1 (`Cvtop op) v))
    | OfString -> begin
      let s = str_value 1 (`Cvtop op) v in
      match int_of_string_opt s with
      | None -> raise (Invalid_argument "int_of_string")
      | Some i -> Int i
    end
    | _ -> Fmt.failwith {|cvtop: Unsupported int operator "%a"|} Ty.Cvtop.pp op
end

module Real = struct
  let to_value (v : float) : Value.t = Real v [@@inline]

  let of_value (n : int) (op : op_type) (v : Value.t) : float =
    of_arg
      (function Real v -> v | _ -> raise_notrace (Value Ty_int))
      n v op
      (err_str n op Ty_real (Value.type_of v))
  [@@inline]

  let unop (op : Ty.Unop.t) (v : Value.t) : Value.t =
    let v = of_value 1 (`Unop op) v in
    match op with
    | Neg -> to_value @@ Float.neg v
    | Abs -> to_value @@ Float.abs v
    | Sqrt -> to_value @@ Float.sqrt v
    | Nearest -> to_value @@ Float.round v
    | Ceil -> to_value @@ Float.ceil v
    | Floor -> to_value @@ Float.floor v
    | Trunc -> to_value @@ Float.trunc v
    | Is_nan -> if Float.is_nan v then Value.True else Value.False
    | _ -> Fmt.failwith {|unop: Unsupported real operator "%a"|} Ty.Unop.pp op

  let binop (op : Ty.Binop.t) (v1 : Value.t) (v2 : Value.t) : Value.t =
    let f =
      match op with
      | Add -> Float.add
      | Sub -> Float.sub
      | Mul -> Float.mul
      | Div -> Float.div
      | Rem -> Float.rem
      | Min -> Float.min
      | Max -> Float.max
      | Pow -> Float.pow
      | _ ->
        Fmt.failwith {|binop: Unsupported real operator "%a"|} Ty.Binop.pp op
    in
    to_value (f (of_value 1 (`Binop op) v1) (of_value 2 (`Binop op) v2))

  let relop (op : Ty.Relop.t) (v1 : Value.t) (v2 : Value.t) : bool =
    let f =
      match op with
      | Lt -> Float.Infix.( < )
      | Le -> Float.Infix.( <= )
      | Gt -> Float.Infix.( > )
      | Ge -> Float.Infix.( >= )
      | Eq -> Float.Infix.( = )
      | Ne -> Float.Infix.( <> )
      | _ ->
        Fmt.failwith {|relop: Unsupported real operator "%a"|} Ty.Relop.pp op
    in
    f (of_value 1 (`Relop op) v1) (of_value 2 (`Relop op) v2)

  let cvtop (op : Ty.Cvtop.t) (v : Value.t) : Value.t =
    let op' = `Cvtop op in
    match op with
    | ToString -> Str (Float.to_string (of_value 1 op' v))
    | OfString ->
      let v = match v with Str v -> v | _ -> raise_notrace (Value Ty_str) in
      begin
        match Float.of_string_opt v with
        | None -> raise (Invalid_argument "float_of_int")
        | Some v -> to_value v
      end
    | Reinterpret_int ->
      let v = match v with Int v -> v | _ -> raise_notrace (Value Ty_int) in
      to_value (float_of_int v)
    | Reinterpret_float -> Int (Float.to_int (of_value 1 op' v))
    | _ -> Fmt.failwith {|cvtop: Unsupported real operator "%a"|} Ty.Cvtop.pp op
end

module Bool = struct
  let to_value (b : bool) : Value.t = if b then True else False [@@inline]

  let of_value (n : int) (op : op_type) (v : Value.t) : bool =
    of_arg
      (function
        | True -> true | False -> false | _ -> raise_notrace (Value Ty_bool) )
      n v op
      (err_str n op Ty_bool (Value.type_of v))
  [@@inline]

  let unop (op : Ty.Unop.t) v =
    let b = of_value 1 (`Unop op) v in
    match op with
    | Not -> to_value (not b)
    | _ -> Fmt.failwith {|unop: Unsupported bool operator "%a"|} Ty.Unop.pp op

  let xor b1 b2 =
    match (b1, b2) with
    | true, true -> false
    | true, false -> true
    | false, true -> true
    | false, false -> false

  let binop (op : Ty.Binop.t) v1 v2 =
    let f =
      match op with
      | And -> ( && )
      | Or -> ( || )
      | Xor -> xor
      | _ ->
        Fmt.failwith {|binop: Unsupported bool operator "%a"|} Ty.Binop.pp op
    in
    to_value (f (of_value 1 (`Binop op) v1) (of_value 2 (`Binop op) v2))

  let triop (op : Ty.Triop.t) c v1 v2 =
    match op with
    | Ite -> ( match of_value 1 (`Triop op) c with true -> v1 | false -> v2 )
    | _ -> Fmt.failwith {|triop: Unsupported bool operator "%a"|} Ty.Triop.pp op

  let relop (op : Ty.Relop.t) v1 v2 =
    match op with
    | Eq -> Value.equal v1 v2
    | Ne -> not (Value.equal v1 v2)
    | _ -> Fmt.failwith {|relop: Unsupported bool operator "%a"|} Ty.Relop.pp op

  let naryop (op : Ty.Naryop.t) vs =
    let b =
      match op with
      | Logand ->
        List.fold_left ( && ) true
          (List.mapi (fun i -> of_value i (`Naryop op)) vs)
      | Logor ->
        List.fold_left ( || ) false
          (List.mapi (fun i -> of_value i (`Naryop op)) vs)
      | _ ->
        Fmt.failwith {|naryop: Unsupported bool operator "%a"|} Ty.Naryop.pp op
    in
    to_value b
end

module Str = struct
  let to_value (str : string) : Value.t = Str str [@@inline]

  let of_value (n : int) (op : op_type) (v : Value.t) : string =
    of_arg
      (function Str str -> str | _ -> raise_notrace (Value Ty_str))
      n v op
      (err_str n op Ty_str (Value.type_of v))
  [@@inline]

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

  let unop (op : Ty.Unop.t) v =
    let str = of_value 1 (`Unop op) v in
    match op with
    | Length -> Int.to_value (String.length str)
    | Trim -> to_value (String.trim str)
    | _ -> Fmt.failwith {|unop: Unsupported str operator "%a"|} Ty.Unop.pp op

  let binop (op : Ty.Binop.t) v1 v2 =
    let op' = `Binop op in
    let str = of_value 1 op' v1 in
    match op with
    | At -> (
      let i = Int.of_value 2 op' v2 in
      try to_value (Fmt.str "%c" (String.get str i))
      with Invalid_argument _ -> raise Index_out_of_bounds )
    | String_prefix ->
      Bool.to_value (String.starts_with ~prefix:str (of_value 2 op' v2))
    | String_suffix ->
      Bool.to_value (String.ends_with ~suffix:str (of_value 2 op' v2))
    | String_contains -> Bool.to_value (contains str (of_value 2 op' v2))
    | _ -> Fmt.failwith {|binop: Unsupported str operator "%a"|} Ty.Binop.pp op

  let triop (op : Ty.Triop.t) v1 v2 v3 =
    let op' = `Triop op in
    let str = of_value 1 op' v1 in
    match op with
    | String_extract ->
      let i = Int.of_value 2 op' v2 in
      let len = Int.of_value 3 op' v3 in
      to_value (String.sub str i len)
    | String_replace ->
      let t = of_value 2 op' v2 in
      let t' = of_value 2 op' v3 in
      to_value (replace str t t')
    | String_index ->
      let t = of_value 2 op' v2 in
      let i = Int.of_value 3 op' v3 in
      Int.to_value (indexof str t i)
    | _ -> Fmt.failwith {|triop: Unsupported str operator "%a"|} Ty.Triop.pp op

  let relop (op : Ty.Relop.t) v1 v2 =
    let f =
      match op with
      | Lt -> ( < )
      | Le -> ( <= )
      | Gt -> ( > )
      | Ge -> ( >= )
      | Eq -> ( = )
      | Ne -> ( <> )
      | _ ->
        Fmt.failwith {|relop: Unsupported string operator "%a"|} Ty.Relop.pp op
    in
    let f x y = f (String.compare x y) 0 in
    f (of_value 1 (`Relop op) v1) (of_value 2 (`Relop op) v2)

  let cvtop (op : Ty.Cvtop.t) v =
    let op' = `Cvtop op in
    match op with
    | String_to_code ->
      let str = of_value 1 op' v in
      Int.to_value (Char.code str.[0])
    | String_from_code ->
      let code = Int.of_value 1 op' v in
      to_value (String.make 1 (Char.chr code))
    | String_to_int ->
      let s = of_value 1 op' v in
      let i =
        match int_of_string_opt s with
        | None -> raise (Invalid_argument "string_to_int")
        | Some i -> i
      in
      Int.to_value i
    | String_from_int -> to_value (string_of_int (Int.of_value 1 op' v))
    | String_to_float ->
      let s = of_value 1 op' v in
      let f =
        match float_of_string_opt s with
        | None -> raise (Invalid_argument "string_to_float")
        | Some f -> f
      in
      Real.to_value f
    | _ -> Fmt.failwith {|cvtop: Unsupported str operator "%a"|} Ty.Cvtop.pp op

  let naryop (op : Ty.Naryop.t) vs =
    let op' = `Naryop op in
    match op with
    | Concat -> to_value (String.concat "" (List.map (of_value 0 op') vs))
    | _ ->
      Fmt.failwith {|naryop: Unsupported str operator "%a"|} Ty.Naryop.pp op
end

module Lst = struct
  let of_value (n : int) (op : op_type) (v : Value.t) : Value.t list =
    of_arg
      (function List lst -> lst | _ -> raise_notrace (Value Ty_list))
      n v op
      (err_str n op Ty_list (Value.type_of v))
  [@@inline]

  let unop (op : Ty.Unop.t) (v : Value.t) : Value.t =
    let lst = of_value 1 (`Unop op) v in
    match op with
    | Head -> begin match lst with hd :: _tl -> hd | [] -> assert false end
    | Tail -> begin
      match lst with _hd :: tl -> List tl | [] -> assert false
    end
    | Length -> Int.to_value (List.length lst)
    | Reverse -> List (List.rev lst)
    | _ -> Fmt.failwith {|unop: Unsupported list operator "%a"|} Ty.Unop.pp op

  let binop (op : Ty.Binop.t) v1 v2 =
    let op' = `Binop op in
    match op with
    | At ->
      let lst = of_value 1 op' v1 in
      let i = Int.of_value 2 op' v2 in
      begin
        (* TODO: change datastructure? *)
        match List.nth_opt lst i with
        | None -> raise Index_out_of_bounds
        | Some v -> v
      end
    | List_cons -> List (v1 :: of_value 1 op' v2)
    | List_append -> List (of_value 1 op' v1 @ of_value 2 op' v2)
    | _ -> Fmt.failwith {|binop: Unsupported list operator "%a"|} Ty.Binop.pp op

  let triop (op : Ty.Triop.t) (v1 : Value.t) (v2 : Value.t) (v3 : Value.t) :
    Value.t =
    let op' = `Triop op in
    match op with
    | List_set ->
      let lst = of_value 1 op' v1 in
      let i = Int.of_value 2 op' v2 in
      let rec set i lst v acc =
        match (i, lst) with
        | 0, _ :: tl -> List.rev_append acc (v :: tl)
        | i, hd :: tl -> set (i - 1) tl v (hd :: acc)
        | _, [] -> raise Index_out_of_bounds
      in
      List (set i lst v3 [])
    | _ -> Fmt.failwith {|triop: Unsupported list operator "%a"|} Ty.Triop.pp op

  let naryop (op : Ty.Naryop.t) (vs : Value.t list) : Value.t =
    let op' = `Naryop op in
    match op with
    | Concat -> List (List.concat_map (of_value 0 op') vs)
    | _ ->
      Fmt.failwith {|naryop: Unsupported list operator "%a"|} Ty.Naryop.pp op
end

module I64 = struct
  let cmp_u x op y = op Int64.(add x min_int) Int64.(add y min_int) [@@inline]

  let lt_u x y = cmp_u x Int64.Infix.( < ) y [@@inline]
end

module Bitv = struct
  let to_value bv : Value.t = Bitv bv [@@inline]

  let i32_to_value v = to_value @@ Bitvector.of_int32 v

  let i64_to_value v = to_value @@ Bitvector.of_int64 v

  let of_value (n : int) (op : op_type) (v : Value.t) : Bitvector.t =
    let todo = Ty.Ty_bitv 32 in
    of_arg
      (function Bitv bv -> bv | _ -> raise_notrace (Value todo))
      n v op
      (err_str n op todo (Value.type_of v))

  let i32_of_value n op v = of_value n op v |> Bitvector.to_int32

  let i64_of_value n op v = of_value n op v |> Bitvector.to_int64

  let unop op bv =
    let bv = of_value 1 (`Unop op) bv in
    to_value
    @@
    match op with
    | Ty.Unop.Neg -> Bitvector.neg bv
    | Not -> Bitvector.lognot bv
    | Clz -> Bitvector.clz bv
    | Ctz -> Bitvector.ctz bv
    | Popcnt -> Bitvector.popcnt bv
    | _ ->
      Fmt.failwith {|unop: Unsupported bitvectore operator "%a"|} Ty.Unop.pp op

  let binop op bv1 bv2 =
    let bv1 = of_value 1 (`Binop op) bv1 in
    let bv2 = of_value 2 (`Binop op) bv2 in
    to_value
    @@
    match op with
    | Ty.Binop.Add -> Bitvector.add bv1 bv2
    | Sub -> Bitvector.sub bv1 bv2
    | Mul -> Bitvector.mul bv1 bv2
    | Div -> Bitvector.div bv1 bv2
    | DivU -> Bitvector.div_u bv1 bv2
    | Rem -> Bitvector.rem bv1 bv2
    | RemU -> Bitvector.rem_u bv1 bv2
    | And -> Bitvector.logand bv1 bv2
    | Or -> Bitvector.logor bv1 bv2
    | Xor -> Bitvector.logxor bv1 bv2
    | Shl -> Bitvector.shl bv1 bv2
    | ShrL -> Bitvector.lshr bv1 bv2
    | ShrA -> Bitvector.ashr bv1 bv2
    | Rotl -> Bitvector.rotate_left bv1 bv2
    | Rotr -> Bitvector.rotate_right bv1 bv2
    | _ ->
      Fmt.failwith {|binop: unsupported bitvector operator "%a"|} Ty.Binop.pp op

  let relop op bv1 bv2 =
    let bv1 = of_value 1 (`Relop op) bv1 in
    let bv2 = of_value 2 (`Relop op) bv2 in
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
end

module F32 = struct
  let to_float (v : int32) : float = Int32.float_of_bits v [@@inline]

  let of_float (v : float) : int32 = Int32.bits_of_float v [@@inline]

  let to_value (f : int32) : Value.t = Num (F32 f) [@@inline]

  let to_value' (f : float) : Value.t = to_value @@ of_float f [@@inline]

  let of_value (i : int) (op : op_type) (v : Value.t) : int32 =
    of_arg
      (function Num (F32 f) -> f | _ -> raise_notrace (Value (Ty_fp 32)))
      i v op
      (err_str i op (Ty_fp 32) (Value.type_of v))
  [@@inline]

  let of_value' (i : int) (op : op_type) (v : Value.t) : float =
    of_value i op v |> to_float
  [@@inline]

  (* Stolen from Owi *)
  let abs x = Int32.logand x Int32.max_int

  let neg x = Int32.logxor x Int32.min_int

  let unop (op : Ty.Unop.t) (v : Value.t) : Value.t =
    let f = to_float @@ of_value 1 (`Unop op) v in
    match op with
    | Neg -> to_value @@ neg @@ of_value 1 (`Unop op) v
    | Abs -> to_value @@ abs @@ of_value 1 (`Unop op) v
    | Sqrt -> to_value' @@ Float.sqrt f
    | Nearest -> to_value' @@ Float.round f
    | Ceil -> to_value' @@ Float.ceil f
    | Floor -> to_value' @@ Float.floor f
    | Trunc -> to_value' @@ Float.trunc f
    | Is_nan -> if Float.is_nan f then Value.True else Value.False
    | _ -> Fmt.failwith {|unop: Unsupported f32 operator "%a"|} Ty.Unop.pp op

  (* Stolen from Owi *)
  let copy_sign x y = Int32.logor (abs x) (Int32.logand y Int32.min_int)

  let binop (op : Ty.Binop.t) (v1 : Value.t) (v2 : Value.t) : Value.t =
    let a = of_value' 1 (`Binop op) v1 in
    let b = of_value' 1 (`Binop op) v2 in
    match op with
    | Add -> to_value' @@ Float.add a b
    | Sub -> to_value' @@ Float.sub a b
    | Mul -> to_value' @@ Float.mul a b
    | Div -> to_value' @@ Float.div a b
    | Rem -> to_value' @@ Float.rem a b
    | Min -> to_value' @@ Float.min a b
    | Max -> to_value' @@ Float.max a b
    | Copysign ->
      let a = of_value 1 (`Binop op) v1 in
      let b = of_value 1 (`Binop op) v2 in
      to_value (copy_sign a b)
    | _ -> Fmt.failwith {|binop: Unsupported f32 operator "%a"|} Ty.Binop.pp op

  let relop (op : Ty.Relop.t) (v1 : Value.t) (v2 : Value.t) : bool =
    let f =
      match op with
      | Eq -> Float.Infix.( = )
      | Ne -> Float.Infix.( <> )
      | Lt -> Float.Infix.( < )
      | Le -> Float.Infix.( <= )
      | Gt -> Float.Infix.( > )
      | Ge -> Float.Infix.( >= )
      | _ ->
        Fmt.failwith {|relop: Unsupported f32 operator "%a"|} Ty.Relop.pp op
    in
    f (of_value' 1 (`Relop op) v1) (of_value' 2 (`Relop op) v2)
end

module F64 = struct
  let to_float (v : int64) : float = Int64.float_of_bits v [@@inline]

  let of_float (v : float) : int64 = Int64.bits_of_float v [@@inline]

  let to_value (f : int64) : Value.t = Num (F64 f) [@@inline]

  let to_value' (f : float) : Value.t = to_value @@ of_float f [@@inline]

  let of_value (i : int) (op : op_type) (v : Value.t) : int64 =
    of_arg
      (function Num (F64 f) -> f | _ -> raise_notrace (Value (Ty_fp 64)))
      i v op
      (err_str i op (Ty_fp 64) (Value.type_of v))
  [@@inline]

  let of_value' (i : int) (op : op_type) (v : Value.t) : float =
    of_value i op v |> to_float
  [@@inline]

  (* Stolen from owi *)
  let abs x = Int64.logand x Int64.max_int

  let neg x = Int64.logxor x Int64.min_int

  let unop (op : Ty.Unop.t) (v : Value.t) : Value.t =
    let f = of_value' 1 (`Unop op) v in
    match op with
    | Neg -> to_value @@ neg @@ of_value 1 (`Unop op) v
    | Abs -> to_value @@ abs @@ of_value 1 (`Unop op) v
    | Sqrt -> to_value' @@ Float.sqrt f
    | Nearest -> to_value' @@ Float.round f
    | Ceil -> to_value' @@ Float.ceil f
    | Floor -> to_value' @@ Float.floor f
    | Trunc -> to_value' @@ Float.trunc f
    | Is_nan -> if Float.is_nan f then Value.True else Value.False
    | _ -> Fmt.failwith {|unop: Unsupported f32 operator "%a"|} Ty.Unop.pp op

  let copy_sign x y = Int64.logor (abs x) (Int64.logand y Int64.min_int)

  let binop (op : Ty.Binop.t) (v1 : Value.t) (v2 : Value.t) : Value.t =
    let a = of_value' 1 (`Binop op) v1 in
    let b = of_value' 1 (`Binop op) v2 in
    match op with
    | Add -> to_value' @@ Float.add a b
    | Sub -> to_value' @@ Float.sub a b
    | Mul -> to_value' @@ Float.mul a b
    | Div -> to_value' @@ Float.div a b
    | Rem -> to_value' @@ Float.rem a b
    | Min -> to_value' @@ Float.min a b
    | Max -> to_value' @@ Float.max a b
    | Copysign ->
      let a = of_value 1 (`Binop op) v1 in
      let b = of_value 1 (`Binop op) v2 in
      to_value @@ copy_sign a b
    | _ -> Fmt.failwith {|binop: Unsupported f32 operator "%a"|} Ty.Binop.pp op

  let relop (op : Ty.Relop.t) (v1 : Value.t) (v2 : Value.t) : bool =
    let f =
      match op with
      | Eq -> Float.Infix.( = )
      | Ne -> Float.Infix.( <> )
      | Lt -> Float.Infix.( < )
      | Le -> Float.Infix.( <= )
      | Gt -> Float.Infix.( > )
      | Ge -> Float.Infix.( >= )
      | _ ->
        Fmt.failwith {|relop: Unsupported f32 operator "%a"|} Ty.Relop.pp op
    in
    f (of_value' 1 (`Relop op) v1) (of_value' 2 (`Relop op) v2)
end

module I32CvtOp = struct
  (* let extend_s (n : int) (x : int32) : int32 = *)
  (*   let shift = 32 - n in *)
  (*   Int32.(shift_right (shift_left x shift) shift) *)

  let trunc_f32_s (x : int32) =
    if Int32.Infix.(x <> x) then raise Conversion_to_integer
    else
      let xf = F32.to_float x in
      if
        Float.Infix.(
          xf >= -.Int32.(to_float min_int) || xf < Int32.(to_float min_int) )
      then raise Integer_overflow
      else Int32.of_float xf

  let trunc_f32_u (x : int32) =
    if Int32.Infix.(x <> x) then raise Conversion_to_integer
    else
      let xf = F32.to_float x in
      if Float.Infix.(xf >= -.Int32.(to_float min_int) *. 2.0 || xf <= -1.0)
      then raise Integer_overflow
      else Int32.of_float xf

  let trunc_f64_s (x : int64) =
    if Int64.Infix.(x <> x) then raise Conversion_to_integer
    else
      let xf = F64.to_float x in
      if
        Float.Infix.(
          xf >= -.Int64.(to_float min_int) || xf < Int64.(to_float min_int) )
      then raise Integer_overflow
      else Int32.of_float xf

  let trunc_f64_u (x : int64) =
    if Int64.Infix.(x <> x) then raise Conversion_to_integer
    else
      let xf = F64.to_float x in
      if Float.Infix.(xf >= -.Int64.(to_float min_int) *. 2.0 || xf <= -1.0)
      then raise Integer_overflow
      else Int32.of_float xf

  let trunc_sat_f32_s x =
    if Int32.Infix.(x <> x) then 0l
    else
      let xf = F32.to_float x in
      if Float.Infix.(xf < Int32.(to_float min_int)) then Int32.min_int
      else if Float.Infix.(xf >= -.Int32.(to_float min_int)) then Int32.max_int
      else Int32.of_float xf

  let trunc_sat_f32_u x =
    if Int32.Infix.(x <> x) then 0l
    else
      let xf = F32.to_float x in
      if Float.Infix.(xf <= -1.0) then 0l
      else if Float.Infix.(xf >= -.Int32.(to_float min_int) *. 2.0) then -1l
      else Int32.of_float xf

  let trunc_sat_f64_s x =
    if Int64.Infix.(x <> x) then 0l
    else
      let xf = F64.to_float x in
      if Float.Infix.(xf < Int64.(to_float min_int)) then Int32.min_int
      else if Float.Infix.(xf >= -.Int64.(to_float min_int)) then Int32.max_int
      else Int32.of_float xf

  let trunc_sat_f64_u x =
    if Int64.Infix.(x <> x) then 0l
    else
      let xf = F64.to_float x in
      if Float.Infix.(xf <= -1.0) then 0l
      else if Float.Infix.(xf >= -.Int64.(to_float min_int) *. 2.0) then -1l
      else Int32.of_float xf

  let cvtop op v =
    let op' = `Cvtop op in
    match op with
    | Ty.Cvtop.WrapI64 ->
      Bitv.i32_to_value (Int64.to_int32 (Bitv.i64_of_value 1 op' v))
    | TruncSF32 -> Bitv.i32_to_value (trunc_f32_s (F32.of_value 1 op' v))
    | TruncUF32 -> Bitv.i32_to_value (trunc_f32_u (F32.of_value 1 op' v))
    | TruncSF64 -> Bitv.i32_to_value (trunc_f64_s (F64.of_value 1 op' v))
    | TruncUF64 -> Bitv.i32_to_value (trunc_f64_u (F64.of_value 1 op' v))
    | Trunc_sat_f32_s ->
      Bitv.i32_to_value (trunc_sat_f32_s (F32.of_value 1 op' v))
    | Trunc_sat_f32_u ->
      Bitv.i32_to_value (trunc_sat_f32_u (F32.of_value 1 op' v))
    | Trunc_sat_f64_s ->
      Bitv.i32_to_value (trunc_sat_f64_s (F64.of_value 1 op' v))
    | Trunc_sat_f64_u ->
      Bitv.i32_to_value (trunc_sat_f64_u (F64.of_value 1 op' v))
    | Reinterpret_float -> Bitv.i32_to_value (F32.of_value 1 op' v)
    | Sign_extend n ->
      Bitv.to_value (Bitvector.sign_extend n (Bitv.of_value 1 op' v))
    | Zero_extend n ->
      Bitv.to_value (Bitvector.zero_extend n (Bitv.of_value 1 op' v))
    | OfBool -> v (* already a num here *)
    | ToBool | _ ->
      Fmt.failwith {|cvtop: Unsupported i32 operator "%a"|} Ty.Cvtop.pp op
end

module I64CvtOp = struct
  (* let extend_s n x = *)
  (*   let shift = 64 - n in *)
  (*   Int64.(shift_right (shift_left x shift) shift) *)

  let extend_i32_u (x : int32) =
    Int64.(logand (of_int32 x) 0x0000_0000_ffff_ffffL)

  let trunc_f32_s (x : int32) =
    if Int32.Infix.(x <> x) then raise Conversion_to_integer
    else
      let xf = F32.to_float x in
      if
        Float.Infix.(
          xf >= -.Int64.(to_float min_int) || xf < Int64.(to_float min_int) )
      then raise Integer_overflow
      else Int64.of_float xf

  let trunc_f32_u (x : int32) =
    if Int32.Infix.(x <> x) then raise Conversion_to_integer
    else
      let xf = F32.to_float x in
      if Float.Infix.(xf >= -.Int64.(to_float min_int) *. 2.0 || xf <= -1.0)
      then raise Integer_overflow
      else if Float.Infix.(xf >= -.Int64.(to_float min_int)) then
        Int64.(logxor (of_float (xf -. 0x1p63)) min_int)
      else Int64.of_float xf

  let trunc_f64_s (x : int64) =
    if Int64.Infix.(x <> x) then raise Conversion_to_integer
    else
      let xf = F64.to_float x in
      if
        Float.Infix.(
          xf >= -.Int64.(to_float min_int) || xf < Int64.(to_float min_int) )
      then raise Integer_overflow
      else Int64.of_float xf

  let trunc_f64_u (x : int64) =
    if Int64.Infix.(x <> x) then raise Conversion_to_integer
    else
      let xf = F64.to_float x in
      if Float.Infix.(xf >= -.Int64.(to_float min_int) *. 2.0 || xf <= -1.0)
      then raise Integer_overflow
      else if Float.Infix.(xf >= -.Int64.(to_float min_int)) then
        Int64.(logxor (of_float (xf -. 0x1p63)) min_int)
      else Int64.of_float xf

  let trunc_sat_f32_s (x : int32) =
    if Int32.Infix.(x <> x) then 0L
    else
      let xf = F32.to_float x in
      if Float.Infix.(xf < Int64.(to_float min_int)) then Int64.min_int
      else if Float.Infix.(xf >= -.Int64.(to_float min_int)) then Int64.max_int
      else Int64.of_float xf

  let trunc_sat_f32_u (x : int32) =
    if Int32.Infix.(x <> x) then 0L
    else
      let xf = F32.to_float x in
      if Float.Infix.(xf <= -1.0) then 0L
      else if Float.Infix.(xf >= -.Int64.(to_float min_int) *. 2.0) then -1L
      else if Float.Infix.(xf >= -.Int64.(to_float min_int)) then
        Int64.(logxor (of_float (xf -. 0x1p63)) min_int)
      else Int64.of_float xf

  let trunc_sat_f64_s (x : int64) =
    if Int64.Infix.(x <> x) then 0L
    else
      let xf = F64.to_float x in
      if Float.Infix.(xf < Int64.(to_float min_int)) then Int64.min_int
      else if Float.Infix.(xf >= -.Int64.(to_float min_int)) then Int64.max_int
      else Int64.of_float xf

  let trunc_sat_f64_u (x : int64) =
    if Int64.Infix.(x <> x) then 0L
    else
      let xf = F64.to_float x in
      if Float.Infix.(xf <= -1.0) then 0L
      else if Float.Infix.(xf >= -.Int64.(to_float min_int) *. 2.0) then -1L
      else if Float.Infix.(xf >= -.Int64.(to_float min_int)) then
        Int64.(logxor (of_float (xf -. 0x1p63)) min_int)
      else Int64.of_float xf

  let cvtop (op : Ty.Cvtop.t) (v : Value.t) : Value.t =
    let op' = `Cvtop op in
    match op with
    | Sign_extend n ->
      Bitv.to_value (Bitvector.sign_extend n (Bitv.of_value 1 op' v))
    | Zero_extend n ->
      Bitv.to_value (Bitvector.zero_extend n (Bitv.of_value 1 op' v))
    | TruncSF32 -> Bitv.i64_to_value (trunc_f32_s (F32.of_value 1 op' v))
    | TruncUF32 -> Bitv.i64_to_value (trunc_f32_u (F32.of_value 1 op' v))
    | TruncSF64 -> Bitv.i64_to_value (trunc_f64_s (F64.of_value 1 op' v))
    | TruncUF64 -> Bitv.i64_to_value (trunc_f64_u (F64.of_value 1 op' v))
    | Trunc_sat_f32_s ->
      Bitv.i64_to_value (trunc_sat_f32_s (F32.of_value 1 op' v))
    | Trunc_sat_f32_u ->
      Bitv.i64_to_value (trunc_sat_f32_u (F32.of_value 1 op' v))
    | Trunc_sat_f64_s ->
      Bitv.i64_to_value (trunc_sat_f64_s (F64.of_value 1 op' v))
    | Trunc_sat_f64_u ->
      Bitv.i64_to_value (trunc_sat_f64_u (F64.of_value 1 op' v))
    | Reinterpret_float -> Bitv.i64_to_value (F64.of_value 1 op' v)
    | WrapI64 ->
      raise
        (TypeError
           { index = 1
           ; value = v
           ; ty = Ty_bitv 64
           ; op = `Cvtop WrapI64
           ; msg = "Cannot wrapI64 on an I64"
           } )
    | ToBool | OfBool | _ ->
      Fmt.failwith {|cvtop: Unsupported i64 operator "%a"|} Ty.Cvtop.pp op
end

module F32CvtOp = struct
  let demote_f64 x =
    let xf = F64.to_float x in
    if Float.Infix.(xf = xf) then F32.of_float xf
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

  let convert_i32_s x = F32.of_float (Int32.to_float x)

  let convert_i32_u x =
    F32.of_float
      Int32.(
        Int32.Infix.(
          if x >= 0l then to_float x
          else to_float (logor (shift_right_logical x 1) (logand x 1l)) *. 2.0 ) )

  let convert_i64_s x =
    F32.of_float
      Int64.(
        Int64.Infix.(
          if abs x < 0x10_0000_0000_0000L then to_float x
          else
            let r = if logand x 0xfffL = 0L then 0L else 1L in
            to_float (logor (shift_right x 12) r) *. 0x1p12 ) )

  let convert_i64_u x =
    F32.of_float
      Int64.(
        Int64.Infix.(
          if I64.lt_u x 0x10_0000_0000_0000L then to_float x
          else
            let r = if logand x 0xfffL = 0L then 0L else 1L in
            to_float (logor (shift_right_logical x 12) r) *. 0x1p12 ) )

  let cvtop (op : Ty.Cvtop.t) (v : Value.t) : Value.t =
    let op' = `Cvtop op in
    match op with
    | DemoteF64 -> F32.to_value (demote_f64 (F64.of_value 1 op' v))
    | ConvertSI32 -> F32.to_value (convert_i32_s (Bitv.i32_of_value 1 op' v))
    | ConvertUI32 -> F32.to_value (convert_i32_u (Bitv.i32_of_value 1 op' v))
    | ConvertSI64 -> F32.to_value (convert_i64_s (Bitv.i64_of_value 1 op' v))
    | ConvertUI64 -> F32.to_value (convert_i64_u (Bitv.i64_of_value 1 op' v))
    | Reinterpret_int -> F32.to_value (Bitv.i32_of_value 1 op' v)
    | PromoteF32 ->
      raise
        (TypeError
           { index = 1
           ; value = v
           ; ty = Ty_fp 32
           ; op = `Cvtop PromoteF32
           ; msg = "F64 must promote a F32"
           } )
    | ToString | OfString | _ ->
      Fmt.failwith {|cvtop: Unsupported f32 operator "%a"|} Ty.Cvtop.pp op
end

module F64CvtOp = struct
  Float.is_nan

  let promote_f32 x =
    let xf = F32.to_float x in
    if Float.Infix.(xf = xf) then F64.of_float xf
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

  let convert_i32_s x = F64.of_float (Int32.to_float x)

  (*
   * Unlike the other convert_u functions, the high half of the i32 range is
   * within the range where f32 can represent odd numbers, so we can't do the
   * shift. Instead, we can use int64 signed arithmetic.
   *)
  let convert_i32_u x =
    F64.of_float Int64.(to_float (logand (of_int32 x) 0x0000_0000_ffff_ffffL))

  let convert_i64_s x = F64.of_float (Int64.to_float x)

  (*
   * Values in the low half of the int64 range can be converted with a signed
   * conversion. The high half is beyond the range where f64 can represent odd
   * numbers, so we can shift the value right, adjust the least significant
   * bit to round correctly, do a conversion, and then scale it back up.
   *)
  let convert_i64_u (x : int64) =
    F64.of_float
      Int64.(
        Int64.Infix.(
          if x >= 0L then to_float x
          else to_float (logor (shift_right_logical x 1) (logand x 1L)) *. 2.0 ) )

  let cvtop (op : Ty.Cvtop.t) v : Value.t =
    let op' = `Cvtop op in
    match op with
    | PromoteF32 -> F64.to_value (promote_f32 (F32.of_value 1 op' v))
    | ConvertSI32 -> F64.to_value (convert_i32_s (Bitv.i32_of_value 1 op' v))
    | ConvertUI32 -> F64.to_value (convert_i32_u (Bitv.i32_of_value 1 op' v))
    | ConvertSI64 -> F64.to_value (convert_i64_s (Bitv.i64_of_value 1 op' v))
    | ConvertUI64 -> F64.to_value (convert_i64_u (Bitv.i64_of_value 1 op' v))
    | Reinterpret_int -> F64.to_value (Bitv.i64_of_value 1 op' v)
    | DemoteF64 ->
      raise
        (TypeError
           { index = 1
           ; value = v
           ; ty = Ty_bitv 64
           ; op = `Cvtop DemoteF64
           ; msg = "F32 must demote a F64"
           } )
    | ToString | OfString | _ ->
      Fmt.failwith {|cvtop: Unsupported f64 operator "%a"|} Ty.Cvtop.pp op
end

(* Dispatch *)

let op int real bool str lst bv f32 f64 ty op =
  match ty with
  | Ty.Ty_int -> int op
  | Ty_real -> real op
  | Ty_bool -> bool op
  | Ty_str -> str op
  | Ty_list -> lst op
  | Ty_bitv _ -> bv op
  | Ty_fp 32 -> f32 op
  | Ty_fp 64 -> f64 op
  | Ty_fp _ | Ty_app | Ty_unit | Ty_none | Ty_regexp | Ty_roundingMode ->
    assert false
[@@inline]

let unop =
  op Int.unop Real.unop Bool.unop Str.unop Lst.unop Bitv.unop F32.unop F64.unop

let binop =
  op Int.binop Real.binop Bool.binop Str.binop Lst.binop Bitv.binop F32.binop
    F64.binop

let triop = function
  | Ty.Ty_bool -> Bool.triop
  | Ty_str -> Str.triop
  | Ty_list -> Lst.triop
  | _ -> assert false

let relop = function
  | Ty.Ty_int -> Int.relop
  | Ty_real -> Real.relop
  | Ty_bool -> Bool.relop
  | Ty_str -> Str.relop
  | Ty_bitv _ -> Bitv.relop
  | Ty_fp 32 -> F32.relop
  | Ty_fp 64 -> F64.relop
  | _ -> assert false

let cvtop = function
  | Ty.Ty_int -> Int.cvtop
  | Ty_real -> Real.cvtop
  | Ty_str -> Str.cvtop
  | Ty_bitv 32 -> I32CvtOp.cvtop
  | Ty_bitv 64 -> I64CvtOp.cvtop
  | Ty_fp 32 -> F32CvtOp.cvtop
  | Ty_fp 64 -> F64CvtOp.cvtop
  | _ -> assert false

let naryop = function
  | Ty.Ty_bool -> Bool.naryop
  | Ty_str -> Str.naryop
  | Ty_list -> Lst.naryop
  | _ -> assert false

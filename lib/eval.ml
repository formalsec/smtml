(***************************************************************************)
(* This file is part of the third-party OCaml library `smtml`.             *)
(* Copyright (C) 2023-2024 formalsec                                       *)
(*                                                                         *)
(* This program is free software: you can redistribute it and/or modify    *)
(* it under the terms of the GNU General Public License as published by    *)
(* the Free Software Foundation, either version 3 of the License, or       *)
(* (at your option) any later version.                                     *)
(*                                                                         *)
(* This program is distributed in the hope that it will be useful,         *)
(* but WITHOUT ANY WARRANTY; without even the implied warranty of          *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the           *)
(* GNU General Public License for more details.                            *)
(*                                                                         *)
(* You should have received a copy of the GNU General Public License       *)
(* along with this program.  If not, see <https://www.gnu.org/licenses/>.  *)
(***************************************************************************)

(* Adapted from: *)
(* - https://github.com/WebAssembly/spec/blob/main/interpreter/exec/ixx.ml, *)
(* - https://github.com/WebAssembly/spec/blob/main/interpreter/exec/fxx.ml, and *)
(* - https://github.com/WebAssembly/spec/blob/main/interpreter/exec *)

(* TODO: This module should be concrete or a part of the reducer *)

open Ty

exception Value of Ty.t

exception
  TypeError of
    { index : int
    ; value : Value.t
    ; ty : Ty.t (* ; op : [< `Unary | `Binary | `Ternary | `Nary ] Ty.op *)
    }

exception DivideByZero

exception ConversionToInteger

exception IntegerOverflow

exception IndexOutOfBounds

exception ParseNumError

let of_arg f n v _op =
  try f v with Value t -> raise (TypeError { index = n; value = v; ty = t })
[@@inline]

module Bool = struct
  let to_value (b : bool) : Value.t = if b then True else False [@@inline]

  let of_value (n : int) (op : 'a op) (v : Value.t) : bool =
    of_arg
      (function
        | True -> true | False -> false | _ -> raise_notrace (Value Ty_bool) )
      n v op
  [@@inline]

  let unop (op : [ `Unary ] op) (v : Value.t) : Value.t =
    let b = of_value 1 op v in
    match op with
    | Not -> to_value (not b)
    | _ -> Log.err {|unop: Unsupported bool operator "%a"|} Ty.pp_op op

  let xor b1 b2 =
    match (b1, b2) with
    | true, true -> false
    | true, false -> true
    | false, true -> true
    | false, false -> false

  let binop (op : [ `Binary ] op) (v1 : Value.t) (v2 : Value.t) : Value.t =
    to_value
    @@
    match op with
    | Eq -> Value.equal v1 v2
    | Ne -> not (Value.equal v1 v2)
    | And -> of_value 1 op v1 && of_value 2 op v2
    | Or -> of_value 1 op v1 || of_value 2 op v2
    | Xor -> xor (of_value 1 op v1) (of_value 2 op v2)
    | _ -> Log.err {|binop: Unsupported bool operator "%a"|} Ty.pp_op op

  let triop (op : [ `Ternary ] op) (c : Value.t) (v1 : Value.t) (v2 : Value.t) :
    Value.t =
    match op with
    | Ite -> if of_value 1 op c then v1 else v2
    | _ -> Log.err {|triop: Unsupported bool operator "%a"|} Ty.pp_op op

  let naryop (op : [ `Nary ] op) (vs : Value.t list) : Value.t =
    let b =
      match op with
      | Logand ->
        List.fold_left ( && ) true (List.mapi (fun i -> of_value i op) vs)
      | Logor ->
        List.fold_left ( || ) false (List.mapi (fun i -> of_value i op) vs)
      | _ -> Log.err {|naryop: Unsupported bool operator "%a"|} Ty.pp_op op
    in
    to_value b
end

module Int = struct
  let to_value (i : int) : Value.t = Int i [@@inline]

  let of_value (n : int) (op : 'a Ty.op) (v : Value.t) : int =
    of_arg (function Int i -> i | _ -> raise_notrace (Value Ty_int)) n v op
  [@@inline]

  let of_bool : Value.t -> int = function
    | True -> 1
    | False -> 0
    | _ -> assert false
  [@@inline]

  let unop (op : [ `Unary ] op) (v : Value.t) : Value.t =
    match op with
    | Neg -> to_value (Int.neg (of_value 1 op v))
    | Abs -> to_value (Int.abs (of_value 1 op v))
    | OfBool -> to_value (of_bool v)
    | Reinterpret_int -> Real (Float.of_int (of_value 1 op v))
    | ToString -> Str (string_of_int (of_value 1 op v))
    | _ -> Log.err {|unop: Unsupported int operator "%a"|} Ty.pp_op op

  let binop (op : [ `Binary ] op) (v1 : Value.t) (v2 : Value.t) : Value.t =
    let v1 = of_value 1 op v1 in
    let v2 = of_value 2 op v2 in
    match op with
    | Lt -> Bool.to_value (v1 < v2)
    | Le -> Bool.to_value (v1 <= v2)
    | Gt -> Bool.to_value (v1 > v2)
    | Ge -> Bool.to_value (v1 >= v2)
    | Eq -> Bool.to_value (Int.equal v1 v2)
    | Ne -> Bool.to_value (not (Int.equal v1 v2))
    | Add -> to_value (Int.add v1 v2)
    | Sub -> to_value (Int.sub v1 v2)
    | Mul -> to_value (Int.mul v1 v2)
    | Div -> to_value (Int.div v1 v2)
    | Rem -> to_value (Int.rem v1 v2)
    | Pow -> to_value (int_of_float (float_of_int v1 ** float_of_int v2))
    | Min -> to_value (Int.min v1 v2)
    | Max -> to_value (Int.max v1 v2)
    | And -> to_value (Int.logand v1 v2)
    | Or -> to_value (Int.logor v1 v2)
    | Xor -> to_value (Int.logxor v1 v2)
    | Shl -> to_value (Int.shift_left v1 v2)
    | ShrL -> to_value (Int.shift_right_logical v1 v2)
    | ShrA -> to_value (Int.shift_right v1 v2)
    | _ -> Log.err {|binop: Unsupported int operator "%a"|} Ty.pp_op op
end

module Real = struct
  let to_value (v : float) : Value.t = Real v [@@inline]

  let of_value (n : int) (op : 'a op) (v : Value.t) : float =
    of_arg (function Real v -> v | _ -> raise_notrace (Value Ty_int)) n v op
  [@@inline]

  let unop (op : [ `Unary ] op) (v : Value.t) : Value.t =
    match op with
    | Neg -> to_value @@ Float.neg (of_value 1 op v)
    | Abs -> to_value @@ Float.abs (of_value 1 op v)
    | Sqrt -> to_value @@ Float.sqrt (of_value 1 op v)
    | Nearest -> to_value @@ Float.round (of_value 1 op v)
    | Ceil -> to_value @@ Float.ceil (of_value 1 op v)
    | Floor -> to_value @@ Float.floor (of_value 1 op v)
    | Trunc -> to_value @@ Float.trunc (of_value 1 op v)
    | Is_nan -> Bool.to_value @@ Float.is_nan (of_value 1 op v)
    | ToString -> Str (Float.to_string (of_value 1 op v))
    | OfString ->
      let v = match v with Str v -> v | _ -> raise_notrace (Value Ty_str) in
      to_value (Float.of_string v)
    | Reinterpret_int ->
      let v = match v with Int v -> v | _ -> raise_notrace (Value Ty_int) in
      to_value (Float.of_int v)
    | Reinterpret_float -> Int (Float.to_int (of_value 1 op v))
    | _ -> Log.err {|unop: Unsupported real operator "%a"|} Ty.pp_op op

  let binop (op : [ `Binary ] op) (v1 : Value.t) (v2 : Value.t) : Value.t =
    let v1 = of_value 1 op v1 in
    let v2 = of_value 2 op v2 in
    match op with
    | Lt -> Bool.to_value (v1 < v2)
    | Le -> Bool.to_value (v1 <= v2)
    | Gt -> Bool.to_value (v1 > v2)
    | Ge -> Bool.to_value (v1 >= v2)
    | Add -> to_value (Float.add v1 v2)
    | Sub -> to_value (Float.sub v1 v2)
    | Mul -> to_value (Float.mul v1 v2)
    | Div -> to_value (Float.div v1 v2)
    | Rem -> to_value (Float.rem v1 v2)
    | Min -> to_value (Float.min v1 v2)
    | Max -> to_value (Float.max v1 v2)
    | Pow -> to_value (Float.pow v1 v2)
    | _ -> Log.err {|binop: Unsupported real operator "%a"|} Ty.pp_op op
end

module Str = struct
  let to_value (str : string) : Value.t = Str str [@@inline]

  let of_value (n : int) (op : 'a op) (v : Value.t) : string =
    of_arg
      (function Str str -> str | _ -> raise_notrace (Value Ty_str))
      n v op
  [@@inline]

  let replace s t t' =
    let len_s = String.length s in
    let len_t = String.length t in
    let rec loop i =
      if i >= len_s then s
      else if i + len_t > len_s then s
      else if String.sub s i len_t = t then
        let s' = String.sub s 0 i ^ t' in
        let s'' = String.sub s (i + len_t) (len_s - i - len_t) in
        s' ^ s''
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
      else if String.sub s i len_sub = sub then i
      else loop (i + 1)
    in
    if start <= 0 then loop 0 else loop start

  let contains s sub = if indexof s sub 0 < 0 then false else true

  let unop (op : [ `Unary ] op) (v : Value.t) : Value.t =
    match op with
    | Length -> Int.to_value (String.length (of_value 1 op v))
    | String_trim ->
      let str = of_value 1 op v in
      to_value (String.trim str)
    | String_to_code ->
      let str = of_value 1 op v in
      Int.to_value (Char.code str.[0])
    | String_from_code ->
      let code = Int.of_value 1 op v in
      to_value (String.make 1 (Char.chr code))
    | String_to_int ->
      let s = of_value 1 op v in
      let i = try int_of_string s with Failure _ -> raise ParseNumError in
      Int.to_value i
    | String_from_int -> to_value (string_of_int (Int.of_value 1 op v))
    | String_to_float ->
      let s = of_value 1 op v in
      let f = try float_of_string s with Failure _ -> raise ParseNumError in
      Real.to_value f
    | _ -> Log.err {|unop: Unsupported string operator "%a"|} Ty.pp_op op

  let binop (op : [ `Binary ] op) (v1 : Value.t) (v2 : Value.t) : Value.t =
    let str = of_value 1 op v1 in
    match op with
    | At -> (
      let i = Int.of_value 2 op v2 in
      try to_value (Format.sprintf "%c" (String.get str i))
      with Invalid_argument _ -> raise IndexOutOfBounds )
    | String_prefix ->
      Bool.to_value (String.starts_with ~prefix:str (of_value 2 op v2))
    | String_suffix ->
      Bool.to_value (String.ends_with ~suffix:str (of_value 2 op v2))
    | String_contains -> Bool.to_value (contains str (of_value 2 op v2))
    | _ -> Log.err {|binop: Unsupported string operator "%a"|} Ty.pp_op op

  let triop (op : [ `Ternary ] op) v1 v2 v3 =
    let str = of_value 1 op v1 in
    match op with
    | String_extract ->
      let i = Int.of_value 2 op v2 in
      let len = Int.of_value 3 op v3 in
      to_value (String.sub str i len)
    | String_replace ->
      let t = of_value 2 op v2 in
      let t' = of_value 2 op v3 in
      to_value (replace str t t')
    | String_index ->
      let t = of_value 2 op v2 in
      let i = Int.of_value 3 op v3 in
      Int.to_value (indexof str t i)
    | _ -> Log.err {|binop: Unsupported string operator "%a"|} Ty.pp_op op

  let naryop (op : [ `Nary ] op) (vs : Value.t list) : Value.t =
    match op with
    | Concat -> to_value (String.concat "" (List.map (of_value 0 op) vs))
    | _ -> Log.err {|naryop: Unsupported str operator "%a"|} Ty.pp_op op
end

module List = struct
  let of_value (n : int) (op : 'a op) (v : Value.t) : Value.t list =
    of_arg
      (function List lst -> lst | _ -> raise_notrace (Value Ty_list))
      n v op
  [@@inline]

  let unop (op : [ `Unary ] op) (v : Value.t) : Value.t =
    let lst = of_value 1 op v in
    match op with
    | Length -> Int.to_value (List.length lst)
    | List_head -> List.hd lst
    | List_tail -> List (List.tl lst)
    | List_reverse -> List (List.rev lst)
    | _ -> Log.err {|unop: Unsupported list operator "%a"|} Ty.pp_op op

  let binop (op : [ `Binary ] op) (v1 : Value.t) (v2 : Value.t) : Value.t =
    match op with
    | At -> (
      let lst = of_value 1 op v1 in
      let i = Int.of_value 2 op v2 in
      try List.nth lst i
      with Failure _ | Invalid_argument _ -> raise IndexOutOfBounds )
    | List_append_last -> List (of_value 1 op v1 @ [ v2 ])
    | List_append -> List (v2 :: of_value 2 op v1)
    | _ -> Log.err {|binop: Unsupported list operator "%a"|} Ty.pp_op op

  let triop (op : [ `Ternary ] op) (v1 : Value.t) (v2 : Value.t) (v3 : Value.t)
    : Value.t =
    match op with
    | List_set ->
      let lst = of_value 1 op v1 in
      let i = Int.of_value 2 op v2 in
      let rec set i lst v acc =
        match (i, lst) with
        | 0, _ :: tl -> List.rev_append acc (v :: tl)
        | i, hd :: tl -> set (i - 1) tl v (hd :: acc)
        | _, [] -> raise IndexOutOfBounds
      in
      List (set i lst v3 [])
    | _ -> Log.err {|triop: Unsupported list operator "%a"|} Ty.pp_op op

  let naryop (op : [ `Nary ] op) (vs : Value.t list) : Value.t =
    match op with
    | Concat -> List (List.concat_map (of_value 0 op) vs)
    | _ -> Log.err {|naryop: Unsupported list operator "%a"|} Ty.pp_op op
end

module I32 = struct
  let to_value (i : int32) : Value.t = Num (I32 i) [@@inline]

  let of_value (n : int) (op : 'a op) (v : Value.t) : int32 =
    of_arg
      (function Num (I32 i) -> i | _ -> raise_notrace (Value (Ty_bitv 32)))
      n v op
  [@@inline]

  let cmp_u x op y = op Int32.(add x min_int) Int32.(add y min_int) [@@inline]

  let lt_u x y = cmp_u x ( < ) y [@@inline]

  let le_u x y = cmp_u x ( <= ) y [@@inline]

  let gt_u x y = cmp_u x ( > ) y [@@inline]

  let ge_u x y = cmp_u x ( >= ) y [@@inline]

  let shift f x y = f x Int32.(to_int (logand y 31l)) [@@inline]

  let shl x y = shift Int32.shift_left x y [@@inline]

  let shr_s x y = shift Int32.shift_right x y [@@inline]

  let shr_u x y = shift Int32.shift_right_logical x y [@@inline]

  (* Stolen rotl and rotr from: *)
  (* https://github.com/OCamlPro/owi/blob/main/src/int32.ml *)
  (* We must mask the count to implement rotates via shifts. *)
  let clamp_rotate_count n = Int32.(to_int (logand n 31l)) [@@inline]

  let rotl x y =
    let n = clamp_rotate_count y in
    Int32.logor (shl x (Int32.of_int n)) (shr_u x (Int32.of_int (32 - n)))
  [@@inline]

  let rotr x y =
    let n = clamp_rotate_count y in
    Int32.logor (shr_u x (Int32.of_int n)) (shl x (Int32.of_int (32 - n)))
  [@@inline]

  let clz n =
    let n = Ocaml_intrinsics.Int32.count_leading_zeros n in
    Int32.of_int n

  let ctz n =
    let n = Ocaml_intrinsics.Int32.count_trailing_zeros n in
    Int32.of_int n

  let unop (op : [ `Unary ] op) (v : Value.t) : Value.t =
    let f =
      match op with
      | Neg -> Int32.neg
      | Not -> Int32.lognot
      | Clz -> clz
      | Ctz -> ctz
      | _ -> Log.err {|unop: Unsupported i32 operator "%a"|} Ty.pp_op op
    in
    to_value (f (of_value 1 op v))

  let binop (op : [ `Binary ] op) (v1 : Value.t) (v2 : Value.t) : Value.t =
    let v1 = of_value 1 op v1 in
    let v2 = of_value 2 op v2 in
    match op with
    | Lt -> Bool.to_value (v1 < v2)
    | LtU -> Bool.to_value (lt_u v1 v2)
    | Le -> Bool.to_value (v1 <= v2)
    | LeU -> Bool.to_value (le_u v1 v2)
    | Gt -> Bool.to_value (v1 > v2)
    | GtU -> Bool.to_value (gt_u v1 v2)
    | Ge -> Bool.to_value (v1 >= v2)
    | GeU -> Bool.to_value (ge_u v1 v2)
    | Add -> to_value (Int32.add v1 v2)
    | Sub -> to_value (Int32.sub v1 v2)
    | Mul -> to_value (Int32.mul v1 v2)
    | Div -> to_value (Int32.div v1 v2)
    | DivU -> to_value (Int32.unsigned_div v1 v2)
    | Rem -> to_value (Int32.rem v1 v2)
    | RemU -> to_value (Int32.unsigned_rem v1 v2)
    | And -> to_value (Int32.logand v1 v2)
    | Or -> to_value (Int32.logor v1 v2)
    | Xor -> to_value (Int32.logxor v1 v2)
    | Shl -> to_value (shl v1 v2)
    | ShrL -> to_value (shr_u v1 v2)
    | ShrA -> to_value (shr_s v1 v2)
    | Rotl -> to_value (rotl v1 v2)
    | Rotr -> to_value (rotr v1 v2)
    | _ -> Log.err {|binop: Unsupported i32 operator "%a"|} Ty.pp_op op
end

module I64 = struct
  let to_value (i : int64) : Value.t = Num (I64 i) [@@inline]

  let of_value (n : int) (op : 'a op) (v : Value.t) : int64 =
    of_arg
      (function Num (I64 i) -> i | _ -> raise_notrace (Value (Ty_bitv 64)))
      n v op
  [@@inline]

  let cmp_u x op y = op Int64.(add x min_int) Int64.(add y min_int) [@@inline]

  let lt_u x y = cmp_u x ( < ) y [@@inline]

  let le_u x y = cmp_u x ( <= ) y [@@inline]

  let gt_u x y = cmp_u x ( > ) y [@@inline]

  let ge_u x y = cmp_u x ( >= ) y [@@inline]

  let shift f x y = f x Int64.(to_int (logand y 63L)) [@@inline]

  let shl x y = shift Int64.shift_left x y [@@inline]

  let shr_s x y = shift Int64.shift_right x y [@@inline]

  let shr_u x y = shift Int64.shift_right_logical x y [@@inline]

  (* Stolen rotl and rotr from: *)
  (* https://github.com/OCamlPro/owi/blob/main/src/int64.ml *)
  (* We must mask the count to implement rotates via shifts. *)
  let clamp_rotate_count n = Int64.(to_int (logand n (of_int 63))) [@@inline]

  let rotl x y =
    let n = clamp_rotate_count y in
    Int64.logor (shl x (Int64.of_int n)) (shr_u x (Int64.of_int (64 - n)))
  [@@inline]

  let rotr x y =
    let n = clamp_rotate_count y in
    Int64.logor (shr_u x (Int64.of_int n)) (shl x (Int64.of_int (64 - n)))
  [@@inline]

  let clz n =
    let n = Ocaml_intrinsics.Int64.count_leading_zeros n in
    Int64.of_int n

  let ctz n =
    let n = Ocaml_intrinsics.Int64.count_trailing_zeros n in
    Int64.of_int n

  let unop (op : [ `Unary ] op) (v : Value.t) : Value.t =
    let f =
      match op with
      | Neg -> Int64.neg
      | Not -> Int64.lognot
      | Clz -> clz
      | Ctz -> ctz
      | _ -> Log.err {|unop: Unsupported i64 operator "%a"|} Ty.pp_op op
    in
    to_value (f (of_value 1 op v))

  let binop (op : [ `Binary ] op) (v1 : Value.t) (v2 : Value.t) : Value.t =
    let v1 = of_value 1 op v1 in
    let v2 = of_value 2 op v2 in
    match op with
    | Lt -> Bool.to_value (v1 < v2)
    | LtU -> Bool.to_value (lt_u v1 v2)
    | Le -> Bool.to_value (v1 <= v2)
    | LeU -> Bool.to_value (le_u v1 v2)
    | Gt -> Bool.to_value (v1 > v2)
    | GtU -> Bool.to_value (gt_u v1 v2)
    | Ge -> Bool.to_value (v1 >= v2)
    | GeU -> Bool.to_value (ge_u v1 v2)
    | Add -> to_value (Int64.add v1 v2)
    | Sub -> to_value (Int64.sub v1 v2)
    | Mul -> to_value (Int64.mul v1 v2)
    | Div -> to_value (Int64.div v1 v2)
    | DivU -> to_value (Int64.unsigned_div v1 v2)
    | Rem -> to_value (Int64.rem v1 v2)
    | RemU -> to_value (Int64.unsigned_rem v1 v2)
    | And -> to_value (Int64.logand v1 v2)
    | Or -> to_value (Int64.logor v1 v2)
    | Xor -> to_value (Int64.logxor v1 v2)
    | Shl -> to_value (shl v1 v2)
    | ShrL -> to_value (shr_u v1 v2)
    | ShrA -> to_value (shr_s v1 v2)
    | Rotl -> to_value (rotl v1 v2)
    | Rotr -> to_value (rotr v1 v2)
    | _ -> Log.err {|binop: Unsupported i64 operator "%a"|} Ty.pp_op op
end

module F32 = struct
  let to_float (v : int32) : float = Int32.float_of_bits v [@@inline]

  let of_float (v : float) : int32 = Int32.bits_of_float v [@@inline]

  let to_value (f : int32) : Value.t = Num (F32 f) [@@inline]

  let to_value' (f : float) : Value.t = to_value @@ of_float f [@@inline]

  let of_value (i : int) (op : 'a op) (v : Value.t) : int32 =
    of_arg
      (function Num (F32 f) -> f | _ -> raise_notrace (Value (Ty_fp 32)))
      i v op
  [@@inline]

  let of_value' (i : int) (op : 'a op) (v : Value.t) : float =
    of_value i op v |> to_float
  [@@inline]

  let unop (op : [ `Unary ] op) (v : Value.t) : Value.t =
    match op with
    | Neg -> to_value' @@ Float.neg (of_value' 1 op v)
    | Abs -> to_value' @@ Float.abs (of_value' 1 op v)
    | Sqrt -> to_value' @@ Float.sqrt (of_value' 1 op v)
    | Nearest -> to_value' @@ Float.round (of_value' 1 op v)
    | Ceil -> to_value' @@ Float.ceil (of_value' 1 op v)
    | Floor -> to_value' @@ Float.floor (of_value' 1 op v)
    | Trunc -> to_value' @@ Float.trunc (of_value' 1 op v)
    | Is_nan -> Bool.to_value (Float.is_nan (of_value' 1 op v))
    | _ -> Log.err {|unop: Unsupported f32 operator "%a"|} Ty.pp_op op

  let binop (op : [ `Binary ] op) (v1 : Value.t) (v2 : Value.t) : Value.t =
    let v1 = of_value' 1 op v1 in
    let v2 = of_value' 2 op v2 in
    match op with
    | Eq -> Bool.to_value (v1 = v2)
    | Ne -> Bool.to_value (v1 <> v2)
    | Lt -> Bool.to_value (v1 < v2)
    | Le -> Bool.to_value (v1 <= v2)
    | Gt -> Bool.to_value (v1 > v2)
    | Ge -> Bool.to_value (v1 >= v2)
    | Add -> to_value' (Float.add v1 v2)
    | Sub -> to_value' (Float.sub v1 v2)
    | Mul -> to_value' (Float.mul v1 v2)
    | Div -> to_value' (Float.div v1 v2)
    | Rem -> to_value' (Float.rem v1 v2)
    | Min -> to_value' (Float.min v1 v2)
    | Max -> to_value' (Float.max v1 v2)
    | _ -> Log.err {|binop: Unsupported f32 operator "%a"|} Ty.pp_op op
end

module F64 = struct
  let to_float (v : int64) : float = Int64.float_of_bits v [@@inline]

  let of_float (v : float) : int64 = Int64.bits_of_float v [@@inline]

  let to_value (f : int64) : Value.t = Num (F64 f) [@@inline]

  let to_value' (f : float) : Value.t = to_value @@ of_float f [@@inline]

  let of_value (i : int) (op : 'a op) (v : Value.t) : int64 =
    of_arg
      (function Num (F64 f) -> f | _ -> raise_notrace (Value (Ty_fp 64)))
      i v op
  [@@inline]

  let of_value' (i : int) (op : 'a op) (v : Value.t) : float =
    of_value i op v |> to_float
  [@@inline]

  let unop (op : [ `Unary ] op) (v : Value.t) : Value.t =
    match op with
    | Neg -> to_value' @@ Float.neg (of_value' 1 op v)
    | Abs -> to_value' @@ Float.abs (of_value' 1 op v)
    | Sqrt -> to_value' @@ Float.sqrt (of_value' 1 op v)
    | Nearest -> to_value' @@ Float.round (of_value' 1 op v)
    | Ceil -> to_value' @@ Float.ceil (of_value' 1 op v)
    | Floor -> to_value' @@ Float.floor (of_value' 1 op v)
    | Trunc -> to_value' @@ Float.trunc (of_value' 1 op v)
    | Is_nan -> Bool.to_value (Float.is_nan (of_value' 1 op v))
    | _ -> Log.err {|unop: Unsupported f32 operator "%a"|} Ty.pp_op op

  let binop (op : [ `Binary ] op) (v1 : Value.t) (v2 : Value.t) : Value.t =
    let v1 = of_value' 1 op v1 in
    let v2 = of_value' 2 op v2 in
    match op with
    | Eq -> Bool.to_value (v1 = v2)
    | Ne -> Bool.to_value (v1 <> v2)
    | Lt -> Bool.to_value (v1 < v2)
    | Le -> Bool.to_value (v1 <= v2)
    | Gt -> Bool.to_value (v1 > v2)
    | Ge -> Bool.to_value (v1 >= v2)
    | Add -> to_value' (Float.add v1 v2)
    | Sub -> to_value' (Float.sub v1 v2)
    | Mul -> to_value' (Float.mul v1 v2)
    | Div -> to_value' (Float.div v1 v2)
    | Rem -> to_value' (Float.rem v1 v2)
    | Min -> to_value' (Float.min v1 v2)
    | Max -> to_value' (Float.max v1 v2)
    | _ -> Log.err {|binop: Unsupported f32 operator "%a"|} Ty.pp_op op
end

(*module I32CvtOp = struct *)
(*  let extend_s (n : int) (x : int32) : int32 = *)
(*    let shift = 32 - n in *)
(*    Int32.(shift_right (shift_left x shift) shift) *)

(*  let trunc_f32_s (x : int32) = *)
(*    if x <> x then raise ConversionToInteger *)
(*    else *)
(*      let xf = F32.to_float x in *)
(*      if xf >= -.Int32.(to_float min_int) || xf < Int32.(to_float min_int) then *)
(*        raise IntegerOverflow *)
(*      else Int32.of_float xf *)

(*  let trunc_f32_u (x : int32) = *)
(*    if x <> x then raise ConversionToInteger *)
(*    else *)
(*      let xf = F32.to_float x in *)
(*      if xf >= -.Int32.(to_float min_int) *. 2.0 || xf <= -1.0 then *)
(*        raise IntegerOverflow *)
(*      else Int32.of_float xf *)

(*  let trunc_f64_s (x : int64) = *)
(*    if x <> x then raise ConversionToInteger *)
(*    else *)
(*      let xf = F64.to_float x in *)
(*      if xf >= -.Int64.(to_float min_int) || xf < Int64.(to_float min_int) then *)
(*        raise IntegerOverflow *)
(*      else Int32.of_float xf *)

(*  let trunc_f64_u (x : int64) = *)
(*    if x <> x then raise ConversionToInteger *)
(*    else *)
(*      let xf = F64.to_float x in *)
(*      if xf >= -.Int64.(to_float min_int) *. 2.0 || xf <= -1.0 then *)
(*        raise IntegerOverflow *)
(*      else Int32.of_float xf *)

(*  let cvtop (op : cvtop) (v : Value.t) : Value.t = *)
(*    let op' = `Cvtop op in *)
(*    match op with *)
(*    | WrapI64 -> I32.to_value (Int64.to_int32 (I64.of_value 1 op' v)) *)
(*    | TruncSF32 -> I32.to_value (trunc_f32_s (F32.of_value 1 op' v)) *)
(*    | TruncUF32 -> I32.to_value (trunc_f32_u (F32.of_value 1 op' v)) *)
(*    | TruncSF64 -> I32.to_value (trunc_f64_s (F64.of_value 1 op' v)) *)
(*    | TruncUF64 -> I32.to_value (trunc_f64_u (F64.of_value 1 op' v)) *)
(*    | Reinterpret_float -> I32.to_value (F32.of_value 1 op' v) *)
(*    | Sign_extend n -> I32.to_value (extend_s n (I32.of_value 1 op' v)) *)
(*    | Zero_extend _n -> I32.to_value (I32.of_value 1 op' v) *)
(*    | OfBool -> v (1* already a num here *1) *)
(*    | ToBool | _ -> *)
(*      Log.err {|cvtop: Unsupported i32 operator "%a"|} Ty.pp_cvtop op *)
(*end *)

(*module I64CvtOp = struct *)
(*  (1* let extend_s n x = *1) *)
(*  (1*   let shift = 64 - n in *1) *)
(*  (1*   Int64.(shift_right (shift_left x shift) shift) *1) *)

(*  let extend_i32_u (x : int32) = *)
(*    Int64.(logand (of_int32 x) 0x0000_0000_ffff_ffffL) *)

(*  let trunc_f32_s (x : int32) = *)
(*    if x <> x then raise ConversionToInteger *)
(*    else *)
(*      let xf = F32.to_float x in *)
(*      if xf >= -.Int64.(to_float min_int) || xf < Int64.(to_float min_int) then *)
(*        raise IntegerOverflow *)
(*      else Int64.of_float xf *)

(*  let trunc_f32_u x = *)
(*    if x <> x then raise ConversionToInteger *)
(*    else *)
(*      let xf = F32.to_float x in *)
(*      if xf >= -.Int64.(to_float min_int) *. 2.0 || xf <= -1.0 then *)
(*        raise IntegerOverflow *)
(*      else if xf >= -.Int64.(to_float min_int) then *)
(*        Int64.(logxor (of_float (xf -. 0x1p63)) min_int) *)
(*      else Int64.of_float xf *)

(*  let trunc_f64_s (x : int64) = *)
(*    if x <> x then raise ConversionToInteger *)
(*    else *)
(*      let xf = F64.to_float x in *)
(*      if xf >= -.Int64.(to_float min_int) || xf < Int64.(to_float min_int) then *)
(*        raise IntegerOverflow *)
(*      else Int64.of_float xf *)

(*  let trunc_f64_u x = *)
(*    if x <> x then raise ConversionToInteger *)
(*    else *)
(*      let xf = F64.to_float x in *)
(*      if xf >= -.Int64.(to_float min_int) *. 2.0 || xf <= -1.0 then *)
(*        raise IntegerOverflow *)
(*      else if xf >= -.Int64.(to_float min_int) then *)
(*        Int64.(logxor (of_float (xf -. 0x1p63)) min_int) *)
(*      else Int64.of_float xf *)

(*  let cvtop (op : cvtop) (v : Value.t) : Value.t = *)
(*    let op' = `Cvtop op in *)
(*    match op with *)
(*    | Sign_extend 32 -> I64.to_value (Int64.of_int32 (I32.of_value 1 op' v)) *)
(*    | Zero_extend 32 -> I64.to_value (extend_i32_u (I32.of_value 1 op' v)) *)
(*    | TruncSF32 -> I64.to_value (trunc_f32_s (F32.of_value 1 op' v)) *)
(*    | TruncUF32 -> I64.to_value (trunc_f32_u (F32.of_value 1 op' v)) *)
(*    | TruncSF64 -> I64.to_value (trunc_f64_s (F64.of_value 1 op' v)) *)
(*    | TruncUF64 -> I64.to_value (trunc_f64_u (F64.of_value 1 op' v)) *)
(*    | Reinterpret_float -> I64.to_value (F64.of_value 1 op' v) *)
(*    | WrapI64 -> *)
(*      raise *)
(*        (TypeError *)
(*           { index = 1; value = v; ty = Ty_bitv 64; op = `Cvtop WrapI64 } ) *)
(*    | ToBool | OfBool | _ -> *)
(*      Log.err {|cvtop: Unsupported i64 operator "%a"|} Ty.pp_cvtop op *)
(*end *)

(*module F32CvtOp = struct *)
(*  let demote_f64 x = *)
(*    let xf = F64.to_float x in *)
(*    if xf = xf then F32.of_float xf *)
(*    else *)
(*      let nan64bits = x in *)
(*      let sign_field = *)
(*        Int64.(shift_left (shift_right_logical nan64bits 63) 31) *)
(*      in *)
(*      let significand_field = *)
(*        Int64.(shift_right_logical (shift_left nan64bits 12) 41) *)
(*      in *)
(*      let fields = Int64.logor sign_field significand_field in *)
(*      Int32.logor 0x7fc0_0000l (Int64.to_int32 fields) *)

(*  let convert_i32_s x = F32.of_float (Int32.to_float x) *)

(*  let convert_i32_u x = *)
(*    F32.of_float *)
(*      Int32.( *)
(*        if x >= 0l then to_float x *)
(*        else to_float (logor (shift_right_logical x 1) (logand x 1l)) *. 2.0 ) *)

(*  let convert_i64_s x = *)
(*    F32.of_float *)
(*      Int64.( *)
(*        if abs x < 0x10_0000_0000_0000L then to_float x *)
(*        else *)
(*          let r = if logand x 0xfffL = 0L then 0L else 1L in *)
(*          to_float (logor (shift_right x 12) r) *. 0x1p12 ) *)

(*  let convert_i64_u x = *)
(*    F32.of_float *)
(*      Int64.( *)
(*        if I64.lt_u x 0x10_0000_0000_0000L then to_float x *)
(*        else *)
(*          let r = if logand x 0xfffL = 0L then 0L else 1L in *)
(*          to_float (logor (shift_right_logical x 12) r) *. 0x1p12 ) *)

(*  let cvtop (op : cvtop) (v : Value.t) : Value.t = *)
(*    let op' = `Cvtop op in *)
(*    match op with *)
(*    | DemoteF64 -> F32.to_value (demote_f64 (F64.of_value 1 op' v)) *)
(*    | ConvertSI32 -> F32.to_value (convert_i32_s (I32.of_value 1 op' v)) *)
(*    | ConvertUI32 -> F32.to_value (convert_i32_u (I32.of_value 1 op' v)) *)
(*    | ConvertSI64 -> F32.to_value (convert_i64_s (I64.of_value 1 op' v)) *)
(*    | ConvertUI64 -> F32.to_value (convert_i64_u (I64.of_value 1 op' v)) *)
(*    | Reinterpret_int -> F32.to_value (I32.of_value 1 op' v) *)
(*    | PromoteF32 -> *)
(*      raise *)
(*        (TypeError *)
(*           { index = 1; value = v; ty = Ty_fp 32; op = `Cvtop PromoteF32 } ) *)
(*    | ToString | OfString | _ -> *)
(*      Log.err {|cvtop: Unsupported f32 operator "%a"|} Ty.pp_cvtop op *)
(*end *)

(*module F64CvtOp = struct *)
(*  let promote_f32 x = *)
(*    let xf = F32.to_float x in *)
(*    if xf = xf then F64.of_float xf *)
(*    else *)
(*      let nan32bits = I64CvtOp.extend_i32_u x in *)
(*      let sign_field = *)
(*        Int64.(shift_left (shift_right_logical nan32bits 31) 63) *)
(*      in *)
(*      let significand_field = *)
(*        Int64.(shift_right_logical (shift_left nan32bits 41) 12) *)
(*      in *)
(*      let fields = Int64.logor sign_field significand_field in *)
(*      Int64.logor 0x7ff8_0000_0000_0000L fields *)

(*  let convert_i32_s x = F64.of_float (Int32.to_float x) *)

(*  (1* *)
(*   * Unlike the other convert_u functions, the high half of the i32 range is *)
(*   * within the range where f32 can represent odd numbers, so we can't do the *)
(*   * shift. Instead, we can use int64 signed arithmetic. *)
(*   *1) *)
(*  let convert_i32_u x = *)
(*    F64.of_float Int64.(to_float (logand (of_int32 x) 0x0000_0000_ffff_ffffL)) *)

(*  let convert_i64_s x = F64.of_float (Int64.to_float x) *)

(*  (1* *)
(*   * Values in the low half of the int64 range can be converted with a signed *)
(*   * conversion. The high half is beyond the range where f64 can represent odd *)
(*   * numbers, so we can shift the value right, adjust the least significant *)
(*   * bit to round correctly, do a conversion, and then scale it back up. *)
(*   *1) *)
(*  let convert_i64_u (x : int64) = *)
(*    F64.of_float *)
(*      Int64.( *)
(*        if x >= 0L then to_float x *)
(*        else to_float (logor (shift_right_logical x 1) (logand x 1L)) *. 2.0 ) *)

(*  let cvtop (op : cvtop) v : Value.t = *)
(*    let op' = `Cvtop op in *)
(*    match op with *)
(*    | PromoteF32 -> F64.to_value (promote_f32 (F32.of_value 1 op' v)) *)
(*    | ConvertSI32 -> F64.to_value (convert_i32_s (I32.of_value 1 op' v)) *)
(*    | ConvertUI32 -> F64.to_value (convert_i32_u (I32.of_value 1 op' v)) *)
(*    | ConvertSI64 -> F64.to_value (convert_i64_s (I64.of_value 1 op' v)) *)
(*    | ConvertUI64 -> F64.to_value (convert_i64_u (I64.of_value 1 op' v)) *)
(*    | Reinterpret_int -> F64.to_value (I64.of_value 1 op' v) *)
(*    | DemoteF64 -> *)
(*      raise *)
(*        (TypeError *)
(*           { index = 1; value = v; ty = Ty_bitv 64; op = `Cvtop DemoteF64 } ) *)
(*    | ToString | OfString | _ -> *)
(*      Log.err {|cvtop: Unsupported f64 operator "%a"|} Ty.pp_cvtop op *)
(*end *)

(* Dispatch *)

let op int real bool str lst i32 i64 f32 f64 ty op =
  match ty with
  | Ty_int -> int op
  | Ty_real -> real op
  | Ty_bool -> bool op
  | Ty_str -> str op
  | Ty_list -> lst op
  | Ty_bitv 32 -> i32 op
  | Ty_bitv 64 -> i64 op
  | Ty_fp 32 -> f32 op
  | Ty_fp 64 -> f64 op
  | Ty_bitv _ | Ty_fp _ | Ty_app | Ty_unit -> assert false
[@@inline]

let unop =
  op Int.unop Real.unop Bool.unop Str.unop List.unop I32.unop I64.unop F32.unop
    F64.unop

let binop =
  op Int.binop Real.binop Bool.binop Str.binop List.binop I32.binop I64.binop
    F32.binop F64.binop

let triop = function
  | Ty_bool -> Bool.triop
  | Ty_str -> Str.triop
  | Ty_list -> List.triop
  | _ -> assert false

let naryop = function
  | Ty_bool -> Bool.naryop
  | Ty_str -> Str.naryop
  | Ty_list -> List.naryop
  | _ -> assert false

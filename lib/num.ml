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

type t =
  | I8 of int
  | I32 of int32
  | I64 of int64
  | F32 of int32
  | F64 of int64

let compare n1 n2 =
  match (n1, n2) with
  | I8 i1, I8 i2 -> Int.compare i1 i2
  | I32 i1, I32 i2 -> Int32.compare i1 i2
  | I64 i1, I64 i2 -> Int64.compare i1 i2
  | F32 i1, F32 i2 ->
    Float.compare (Int32.float_of_bits i1) (Int32.float_of_bits i2)
  | F64 i1, F64 i2 ->
    Float.compare (Int64.float_of_bits i1) (Int64.float_of_bits i2)
  | I8 _, _ -> -1
  | I32 _, I8 _ -> 1
  | I32 _, _ -> -1
  | I64 _, (I8 _ | I32 _) -> 1
  | I64 _, _ -> -1
  | F32 _, (I8 _ | I32 _ | I64 _) -> 1
  | F32 _, F64 _ -> -1
  | F64 _, _ -> 1

let equal (n1 : t) (n2 : t) : bool = compare n1 n2 = 0

let num_of_bool (b : bool) : t = I32 (if b then 1l else 0l)

let pp fmt (n : t) =
  match n with
  | I8 i -> Fmt.pf fmt "0x%02x" (i land 0xff)
  | I32 i -> Fmt.pf fmt "0x%08lx" i
  | I64 i -> Fmt.pf fmt "0x%016Lx" i
  | F32 f -> Fmt.pf fmt "(fp 0x%08lx)" f
  | F64 f -> Fmt.pf fmt "(fp 0x%016Lx)" f

let to_string (n : t) : string = Fmt.str "%a" pp n

let to_json (n : t) : Yojson.Basic.t = `String (to_string n)

let type_of (n : t) =
  match n with
  | I8 _ -> Ty.(Ty_bitv 8)
  | I32 _ -> Ty.(Ty_bitv 32)
  | I64 _ -> Ty.(Ty_bitv 64)
  | F32 _ -> Ty.(Ty_fp 32)
  | F64 _ -> Ty.(Ty_fp 64)

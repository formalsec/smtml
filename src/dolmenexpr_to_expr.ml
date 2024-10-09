(***************************************************************************)
(* This file is part of the third-party OCaml library `smtml`.             *)
(* Copyright (C) 2023-2024 formalsec                                       *)
(* Written by Hichem Rami Ait El Hara                                      *)
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

module DExpr = Dolmen_std.Expr
module DTy = DExpr.Ty
module DTerm = DExpr.Term
module DBuiltin = Dolmen_std.Builtin

module Builtin = struct
  (* additional builtins *)

  let string_ty_cst : DExpr.Term.ty_const =
    DExpr.Id.mk ~builtin:DBuiltin.Base
      (Dolmen_std.Path.global "StringTy")
      DExpr.{ arity = 0; alias = No_alias }

  let string_ty = DTy.apply string_ty_cst []

  let float32_ty = DTy.float 8 24

  let float64_ty = DTy.float 11 53

  let int_to_string : DExpr.term_cst =
    DExpr.Id.mk ~name:"IntToString" ~builtin:DBuiltin.Base
      (Dolmen_std.Path.global "IntToString")
      (DTy.arrow [ DTy.int ] string_ty)

  let string_to_int : DExpr.term_cst =
    DExpr.Id.mk ~name:"StringToInt" ~builtin:DBuiltin.Base
      (Dolmen_std.Path.global "StringToInt")
      (DTy.arrow [ string_ty ] DTy.int)

  let real_to_string : DExpr.term_cst =
    DExpr.Id.mk ~name:"RealToString" ~builtin:DBuiltin.Base
      (Dolmen_std.Path.global "RealToString")
      (DTy.arrow [ DTy.real ] string_ty)

  let string_to_real : DExpr.term_cst =
    DExpr.Id.mk ~name:"StringToReal" ~builtin:DBuiltin.Base
      (Dolmen_std.Path.global "StringToReal")
      (DTy.arrow [ string_ty ] DTy.real)

  let real_to_uint32 : DExpr.term_cst =
    DExpr.Id.mk ~name:"RealToUInt32" ~builtin:DBuiltin.Base
      (Dolmen_std.Path.global "RealToUInt32")
      (DTy.arrow [ DTy.real ] DTy.real)

  let trim_string : DExpr.term_cst =
    DExpr.Id.mk ~name:"TrimString" ~builtin:DBuiltin.Base
      (Dolmen_std.Path.global "TrimString")
      (DTy.arrow [ string_ty ] string_ty)

  let f32_to_string : DExpr.term_cst =
    DExpr.Id.mk ~name:"F32ToString" ~builtin:DBuiltin.Base
      (Dolmen_std.Path.global "F32ToString")
      (DTy.arrow [ float32_ty ] string_ty)

  let string_to_f32 : DExpr.term_cst =
    DExpr.Id.mk ~name:"StringToF32" ~builtin:DBuiltin.Base
      (Dolmen_std.Path.global "StringToF32")
      (DTy.arrow [ string_ty ] float32_ty)

  let f64_to_string : DExpr.term_cst =
    DExpr.Id.mk ~name:"F64ToString" ~builtin:DBuiltin.Base
      (Dolmen_std.Path.global "F64ToString")
      (DTy.arrow [ float64_ty ] string_ty)

  let string_to_f64 : DExpr.term_cst =
    DExpr.Id.mk ~name:"StringToF64" ~builtin:DBuiltin.Base
      (Dolmen_std.Path.global "StringToF64")
      (DTy.arrow [ string_ty ] float64_ty)
end

let tty_of_etype (e : Ty.t) : DTy.t =
  match e with
  | Ty_int -> DTy.int
  | Ty_real -> DTy.real
  | Ty_bool -> DTy.bool
  | Ty_str -> Builtin.string_ty
  | Ty_bitv 8 -> DTy.bitv 8
  | Ty_bitv 32 -> DTy.bitv 32
  | Ty_bitv 64 -> DTy.bitv 64
  | Ty_fp 32 -> Builtin.float32_ty
  | Ty_fp 64 -> Builtin.float64_ty
  | Ty_fp _ | Ty_bitv _ | Ty_list | Ty_app | Ty_unit | Ty_none | Ty_regexp ->
    Fmt.failwith {|Unsupported type "%a"|} Ty.pp e

let tty_to_etype (ty : DTy.t) : Ty.t =
  match ty with
  | { ty_descr = TyApp ({ builtin = DBuiltin.Int; _ }, _); _ } -> Ty_int
  | { ty_descr = TyApp ({ builtin = DBuiltin.Real; _ }, _); _ } -> Ty_real
  | { ty_descr = TyApp ({ builtin = DBuiltin.Prop; _ }, _); _ } -> Ty_bool
  | { ty_descr =
        TyApp
          ( { builtin = DBuiltin.Base
            ; path = Absolute { name = "StringTy"; _ }
            ; _
            }
          , _ )
    ; _
    } ->
    Ty_str
  | { ty_descr = TyApp ({ builtin = DBuiltin.Bitv n; _ }, _); _ } -> Ty_bitv n
  | { ty_descr = TyApp ({ builtin = DBuiltin.Float (8, 24); _ }, _); _ } ->
    Ty_fp 32
  | { ty_descr = TyApp ({ builtin = DBuiltin.Float (11, 53); _ }, _); _ } ->
    Ty_fp 64
  | _ -> Fmt.failwith {|Unsupported dolmen type "%a"|} DTy.print ty

module SHT = Hashtbl.Make (struct
  include Symbol

  let hash = Hashtbl.hash
end)

let sym_cache = SHT.create 17

let tcst_of_symbol (s : Symbol.t) =
  match SHT.find_opt sym_cache s with
  | None ->
    let x = Symbol.to_string s
    and t = Symbol.type_of s in
    let cst = DTerm.Const.mk (Dolmen_std.Path.global x) (tty_of_etype t) in
    SHT.add sym_cache s cst;
    cst
  | Some c -> c

let tcst_to_symbol (c : DExpr.term_cst) : Symbol.t =
  match c with
  | { builtin = DBuiltin.Base
    ; path = Local { name } | Absolute { name; _ }
    ; id_ty
    ; _
    } ->
    Symbol.make (tty_to_etype id_ty) name
  | _ -> Fmt.failwith {|Unsupported constant term "%a"|} DExpr.Print.term_cst c

type expr = DTerm.t

module I :
  Op_intf.S
    with type v := int
     and type t := expr
     and type unop := Ty.unop
     and type binop := Ty.binop
     and type relop := Ty.relop
     and type cvtop := Ty.cvtop
     and type triop := Ty.triop = struct
  open Ty

  let encode_val i = DTerm.Int.mk (Int.to_string i)

  let encode_unop op e =
    let op' =
      match op with
      | Neg -> DTerm.Int.minus
      | _ -> Fmt.failwith {|Int: Unsupported unop operator "%a"|} Ty.pp_unop op
    in
    op' e

  let encode_binop op e1 e2 =
    let op' =
      match op with
      | Add -> DTerm.Int.add
      | Sub -> DTerm.Int.sub
      | Mul -> DTerm.Int.mul
      | Div -> DTerm.Int.div
      | Rem -> DTerm.Int.rem
      | Pow ->
        fun _e1 _e2 -> assert false
        (* DTerm.apply_cst
           Colibri2_theories_LRA.RealValue.Builtin.colibri_pow_int_int []
           [ e1; e2 ] *)
      | _ -> Fmt.failwith "{|Unsupported binop operation %a|}" Ty.pp_binop op
    in
    op' e1 e2

  let encode_relop op e1 e2 =
    let op' =
      match op with
      | Eq -> DTerm.eq
      | Ne -> DTerm.neq
      | Lt -> DTerm.Int.lt
      | Gt -> DTerm.Int.gt
      | Le -> DTerm.Int.le
      | Ge -> DTerm.Int.ge
      | _ ->
        Fmt.failwith {|Arith: Unsupported relop operator "%a"|} Ty.pp_relop op
    in
    op' e1 e2

  let encode_cvtop op e =
    let op' =
      match op with
      | ToString -> fun v -> DTerm.apply_cst Builtin.int_to_string [] [ v ]
      | OfString -> fun v -> DTerm.apply_cst Builtin.string_to_int [] [ v ]
      | _ ->
        Fmt.failwith {|Int: Unsupported cvtop operator "%a"|} Ty.pp_cvtop op
    in
    op' e

  let encode_triop op _ _ _ =
    Fmt.failwith {|Arith: Unsupported triop operator "%a"|} Ty.pp_triop op
end

module Real :
  Op_intf.S
    with type v := float
     and type t := expr
     and type unop := Ty.unop
     and type binop := Ty.binop
     and type relop := Ty.relop
     and type cvtop := Ty.cvtop
     and type triop := Ty.triop = struct
  open Ty

  let encode_val f = DTerm.Real.mk (Float.to_string f)

  let encode_unop op e =
    let op' =
      match op with
      | Neg -> DTerm.Real.minus
      | Abs -> assert false
      | Sqrt -> assert false
      | Ceil ->
        fun _e -> assert false
        (* DTerm.apply_cst Colibri2_theories_LRA.RealValue.Builtin.colibri_ceil
           [] [ e ] *)
      | Floor -> DTerm.Real.floor
      | Nearest | Is_nan | _ ->
        Fmt.failwith {|Real: Unsupported cvtop operator "%a"|} Ty.pp_unop op
    in
    op' e

  let encode_binop op e1 e2 =
    let op' =
      match op with
      | Add -> DTerm.Real.add
      | Sub -> DTerm.Real.sub
      | Mul -> DTerm.Real.mul
      | Div -> DTerm.Real.div
      | Min -> fun e1 e2 -> DTerm.ite (DTerm.Real.le e1 e2) e1 e2
      | Max -> fun e1 e2 -> DTerm.ite (DTerm.Real.le e1 e2) e2 e1
      | _ ->
        Fmt.failwith {|Real: Unsupported binop operator "%a"|} Ty.pp_binop op
    in
    op' e1 e2

  let encode_relop op e1 e2 =
    let op' =
      match op with
      | Eq -> DTerm.eq
      | Ne -> DTerm.neq
      | Lt -> DTerm.Real.lt
      | Gt -> DTerm.Real.gt
      | Le -> DTerm.Real.le
      | Ge -> DTerm.Real.ge
      | _ ->
        Fmt.failwith {|Arith: Unsupported relop operator "%a"|} Ty.pp_relop op
    in
    op' e1 e2

  let encode_cvtop op e =
    let op' =
      match op with
      | ToString -> fun v -> DTerm.apply_cst Builtin.real_to_string [] [ v ]
      | OfString -> fun v -> DTerm.apply_cst Builtin.string_to_real [] [ v ]
      | ConvertUI32 -> fun t -> DTerm.apply_cst Builtin.real_to_uint32 [] [ t ]
      | Reinterpret_int -> DTerm.Int.to_real
      | _ ->
        Fmt.failwith {|Real: Unsupported cvtop operator "%a"|} Ty.pp_cvtop op
    in
    op' e

  let encode_triop op _ _ _ =
    Fmt.failwith {|Arith: Unsupported triop operator "%a"|} Ty.pp_triop op
end

module Boolean = struct
  open Ty

  let encode_unop op e =
    let op' =
      match op with
      | Not -> DTerm.neg
      | _ -> Fmt.failwith {|Bool: Unsupported unop operator "%a"|} Ty.pp_unop op
    in
    op' e

  let encode_binop op e1 e2 =
    let op' =
      match op with
      | And -> fun a b -> DTerm._and [ a; b ]
      | Or -> fun a b -> DTerm._or [ a; b ]
      | Xor -> DTerm.xor
      | _ ->
        Fmt.failwith {|Bool: Unsupported binop operator "%a"|} Ty.pp_binop op
    in
    op' e1 e2

  let encode_relop op e1 e2 =
    let op' =
      match op with
      | Eq -> DTerm.eq
      | Ne -> DTerm.neq
      | _ ->
        Fmt.failwith {|Bool: Unsupported relop operator "%a"|} Ty.pp_relop op
    in
    op' e1 e2

  let encode_cvtop op _ =
    Fmt.failwith {|Bool: Unsupported cvtop operator "%a"|} Ty.pp_cvtop op

  let encode_triop op e1 e2 e3 =
    let op' =
      match op with
      | Ite -> DTerm.ite
      | _ ->
        Fmt.failwith {|Bool: Unsupported triop operator "%a"|} Ty.pp_triop op
    in
    op' e1 e2 e3
end

module Str = struct
  open Ty

  let encode_unop op _ =
    Fmt.failwith {|Str: Unsupported unop operator "%a"|} Ty.pp_unop op

  let encode_binop op _ _ =
    Fmt.failwith {|Str: Unsupported binop operator "%a"|} Ty.pp_binop op

  let encode_relop op =
    let op' =
      match op with
      | Eq -> DTerm.eq
      | Ne -> DTerm.neq
      | _ ->
        Fmt.failwith {|Str: Unsupported relop operator "%a"|} Ty.pp_relop op
    in
    op'

  let encode_triop op _ _ _ =
    Fmt.failwith {|Str: Unsupported triop operator "%a"|} Ty.pp_triop op

  let encode_cvtop op _ =
    Fmt.failwith {|Str: Unsupported cvtop operator "%a"|} Ty.pp_cvtop op
end

module Bv = struct
  open Ty

  let encode_val (type a) (cast : a Ty.cast) (i : a) =
    match cast with
    | C8 ->
      let n = if i >= 0 then i else i land ((1 lsl 8) - 1) in
      (* necessary to have the same behaviour as Z3 *)
      DTerm.Bitv.mk
        (Dolmen_type.Misc.Bitv.parse_decimal
           (String.cat "bv" (Int.to_string n))
           8 )
    | C32 ->
      let iint = Int32.to_int i in
      let n = if iint >= 0 then iint else iint land ((1 lsl 32) - 1) in
      (* necessary to have the same behaviour as Z3 *)
      DTerm.Bitv.mk
        (Dolmen_type.Misc.Bitv.parse_decimal
           (String.cat "bv" (Int.to_string n))
           32 )
    | C64 ->
      let n =
        if Int64.compare i Int64.zero >= 0 then Z.of_int64 i
        else Z.logand (Z.of_int64 i) (Z.sub (Z.( lsl ) Z.one 64) Z.one)
      in

      (* necessary to have the same behaviour as Z3 *)
      DTerm.Bitv.mk
        (Dolmen_type.Misc.Bitv.parse_decimal
           (String.cat "bv" (Z.to_string n))
           64 )

  let encode_unop op e =
    let op' =
      match op with
      | Not -> DTerm.Bitv.not
      | Neg -> DTerm.Bitv.neg
      | _ -> Fmt.failwith {|Bv: Unsupported unary operator "%a"|} Ty.pp_unop op
    in
    op' e

  let encode_binop op e1 e2 =
    let op' =
      match op with
      | Add -> DTerm.Bitv.add
      | Sub -> DTerm.Bitv.sub
      | Mul -> DTerm.Bitv.mul
      | Div -> DTerm.Bitv.sdiv
      | DivU -> DTerm.Bitv.udiv
      | And -> DTerm.Bitv.and_
      | Xor -> DTerm.Bitv.xor
      | Or -> DTerm.Bitv.or_
      | ShrA -> DTerm.Bitv.ashr
      | ShrL -> DTerm.Bitv.lshr
      | Shl -> DTerm.Bitv.shl
      | Rem -> DTerm.Bitv.srem
      | RemU -> DTerm.Bitv.urem
      | _ ->
        Fmt.failwith {|Bv: Unsupported binary operator "%a"|} Ty.pp_binop op
    in
    op' e1 e2

  let encode_triop op _ =
    Fmt.failwith {|Bv: Unsupported triop operator "%a"|} Ty.pp_triop op

  let encode_relop op e1 e2 =
    let op' =
      match op with
      | Eq -> DTerm.eq
      | Ne -> DTerm.neq
      | Lt -> DTerm.Bitv.slt
      | LtU -> DTerm.Bitv.ult
      | Le -> DTerm.Bitv.sle
      | LeU -> DTerm.Bitv.ule
      | Gt -> DTerm.Bitv.sgt
      | GtU -> DTerm.Bitv.ugt
      | Ge -> DTerm.Bitv.sge
      | GeU -> DTerm.Bitv.uge
    in
    op' e1 e2

  let encode_cvtop sz op e =
    let op' =
      match sz with
      | 32 -> (
        match op with
        | Sign_extend n -> DTerm.Bitv.sign_extend n
        | Zero_extend n -> DTerm.Bitv.zero_extend n
        | TruncSF32 | TruncSF64 ->
          DTerm.Float.to_sbv 32 DTerm.Float.roundTowardZero
        | TruncUF32 | TruncUF64 ->
          DTerm.Float.to_ubv 32 DTerm.Float.roundTowardZero
        | Reinterpret_float -> DTerm.Float.ieee_format_to_fp 8 24
        | ToBool -> encode_relop Ne (encode_val C32 0l)
        | OfBool -> fun e -> DTerm.ite e (encode_val C32 1l) (encode_val C32 0l)
        | _ ->
          Fmt.failwith {|Bv: Unsupported bv(32) operator "%a"|} Ty.pp_cvtop op )
      | 64 -> (
        match op with
        | Sign_extend n -> DTerm.Bitv.sign_extend n
        | Zero_extend n -> DTerm.Bitv.zero_extend n
        | TruncSF32 | TruncSF64 ->
          DTerm.Float.to_sbv 64 DTerm.Float.roundTowardZero
        | TruncUF32 | TruncUF64 ->
          DTerm.Float.to_ubv 64 DTerm.Float.roundTowardZero
        | Reinterpret_float -> DTerm.Float.ieee_format_to_fp 11 51
        | ToBool -> encode_relop Ne (encode_val C64 0L)
        | OfBool -> fun e -> DTerm.ite e (encode_val C64 1L) (encode_val C64 0L)
        | _ ->
          Fmt.failwith {|Bv: Unsupported bv(64) operator "%a"|} Ty.pp_cvtop op )
      | _ -> assert false
    in
    op' e
end

module Fp = struct
  open Ty

  let encode_val (type a) (sz : a Ty.cast) (f : a) =
    match sz with
    | C8 -> Fmt.failwith "Unable to create FP numeral using 8 bits"
    | C32 -> DTerm.Float.ieee_format_to_fp 8 24 (Bv.encode_val C32 f)
    | C64 -> DTerm.Float.ieee_format_to_fp 11 53 (Bv.encode_val C64 f)

  let encode_unop op e =
    let op' =
      match op with
      | Neg -> DTerm.Float.neg
      | Abs -> DTerm.Float.abs
      | Sqrt -> DTerm.Float.sqrt DTerm.Float.roundNearestTiesToEven
      | Is_nan -> DTerm.Float.isNaN
      | Ceil -> DTerm.Float.roundToIntegral DTerm.Float.roundTowardPositive
      | Floor -> DTerm.Float.roundToIntegral DTerm.Float.roundTowardNegative
      | Trunc -> DTerm.Float.roundToIntegral DTerm.Float.roundTowardZero
      | Nearest ->
        DTerm.Float.roundToIntegral DTerm.Float.roundNearestTiesToEven
      | _ -> Fmt.failwith {|Fp: Unsupported unary operator "%a"|} Ty.pp_unop op
    in
    op' e

  let encode_binop op e1 e2 =
    let op' =
      match op with
      | Add -> DTerm.Float.add DTerm.Float.roundNearestTiesToEven
      | Sub -> DTerm.Float.sub DTerm.Float.roundNearestTiesToEven
      | Mul -> DTerm.Float.mul DTerm.Float.roundNearestTiesToEven
      | Div -> DTerm.Float.div DTerm.Float.roundNearestTiesToEven
      | Min -> DTerm.Float.min
      | Max -> DTerm.Float.max
      | Rem -> DTerm.Float.rem
      | _ -> Fmt.failwith {|Fp: Unsupported binop operator "%a"|} Ty.pp_binop op
    in
    op' e1 e2

  let encode_triop op _ =
    Fmt.failwith {|Fp: Unsupported triop operator "%a"|} Ty.pp_triop op

  let encode_relop op e1 e2 =
    let op' =
      match op with
      | Eq -> DTerm.Float.eq
      | Ne -> fun e1 e2 -> DTerm.Float.eq e1 e2 |> DTerm.neg
      | Lt -> DTerm.Float.lt
      | Le -> DTerm.Float.leq
      | Gt -> DTerm.Float.gt
      | Ge -> DTerm.Float.geq
      | _ -> Fmt.failwith {|Fp: Unsupported relop operator "%a"|} Ty.pp_relop op
    in
    op' e1 e2

  let encode_cvtop sz op e =
    let op' =
      match sz with
      | 32 -> (
        match op with
        | DemoteF64 -> DTerm.Float.to_fp 8 24 DTerm.Float.roundNearestTiesToEven
        | ConvertSI32 | ConvertSI64 ->
          DTerm.Float.sbv_to_fp 8 24 DTerm.Float.roundNearestTiesToEven
        | ConvertUI32 | ConvertUI64 ->
          DTerm.Float.ubv_to_fp 8 24 DTerm.Float.roundNearestTiesToEven
        | Reinterpret_int -> DTerm.Float.ieee_format_to_fp 8 24
        | ToString -> fun v -> DTerm.apply_cst Builtin.f32_to_string [] [ v ]
        | OfString -> fun v -> DTerm.apply_cst Builtin.string_to_f32 [] [ v ]
        | _ ->
          Fmt.failwith {|Fp: Unsupported fp(32) operator "%a"|} Ty.pp_cvtop op )
      | 64 -> (
        match op with
        | PromoteF32 ->
          DTerm.Float.to_fp 11 53 DTerm.Float.roundNearestTiesToEven
        | ConvertSI32 | ConvertSI64 ->
          DTerm.Float.sbv_to_fp 11 53 DTerm.Float.roundNearestTiesToEven
        | ConvertUI32 | ConvertUI64 ->
          DTerm.Float.ubv_to_fp 11 53 DTerm.Float.roundNearestTiesToEven
        | Reinterpret_int -> DTerm.Float.ieee_format_to_fp 11 53
        | ToString -> fun v -> DTerm.apply_cst Builtin.f64_to_string [] [ v ]
        | OfString -> fun v -> DTerm.apply_cst Builtin.string_to_f64 [] [ v ]
        | _ ->
          Fmt.failwith {|Fp: Unsupported fp(64) operator "%a"|} Ty.pp_cvtop op )
      | _ -> assert false
    in
    op' e
end

let encode_val : Value.t -> expr = function
  | True -> DTerm.of_cst DTerm.Const._true
  | False -> DTerm.of_cst DTerm.Const._false
  | Int v -> I.encode_val v
  | Real v -> Real.encode_val v
  | Str _ -> assert false
  | Num (I8 x) -> Bv.encode_val C8 x
  | Num (I32 x) -> Bv.encode_val C32 x
  | Num (I64 x) -> Bv.encode_val C64 x
  | Num (F32 x) -> Fp.encode_val C32 x
  | Num (F64 x) -> Fp.encode_val C64 x
  | v -> Fmt.failwith {|Unsupported value "%a"|} Value.pp v

let encode_unop (ty : Ty.t) =
  match ty with
  | Ty_int -> I.encode_unop
  | Ty_real -> Real.encode_unop
  | Ty_bool -> Boolean.encode_unop
  | Ty_str -> Str.encode_unop
  | Ty_bitv _ -> Bv.encode_unop
  | Ty_fp _ -> Fp.encode_unop
  | (Ty_list | Ty_app | Ty_unit | Ty_none | Ty_regexp) as op ->
    Fmt.failwith {|Trying to encode unsupported op of type %a|} Ty.pp op

let encode_binop (ty : Ty.t) =
  match ty with
  | Ty_int -> I.encode_binop
  | Ty_real -> Real.encode_binop
  | Ty_bool -> Boolean.encode_binop
  | Ty_str -> Str.encode_binop
  | Ty_bitv _ -> Bv.encode_binop
  | Ty_fp _ -> Fp.encode_binop
  | (Ty_list | Ty_app | Ty_unit | Ty_none | Ty_regexp) as op ->
    Fmt.failwith "Trying to encode unsupported op of type %a" Ty.pp op

let encode_triop (ty : Ty.t) =
  match ty with
  | Ty_int -> I.encode_triop
  | Ty_real -> Real.encode_triop
  | Ty_bool -> Boolean.encode_triop
  | Ty_str -> Str.encode_triop
  | Ty_bitv _ -> Bv.encode_triop
  | Ty_fp _ -> Fp.encode_triop
  | (Ty_list | Ty_app | Ty_unit | Ty_none | Ty_regexp) as op ->
    Fmt.failwith "Trying to encode unsupported op of type %a" Ty.pp op

let encode_relop (ty : Ty.t) =
  match ty with
  | Ty_int -> I.encode_relop
  | Ty_real -> Real.encode_relop
  | Ty_bool -> Boolean.encode_relop
  | Ty_str -> Str.encode_relop
  | Ty_bitv _ -> Bv.encode_relop
  | Ty_fp _ -> Fp.encode_relop
  | (Ty_list | Ty_app | Ty_unit | Ty_none | Ty_regexp) as op ->
    Fmt.failwith "Trying to encode unsupported op of type %a" Ty.pp op

let encode_cvtop (ty : Ty.t) =
  match ty with
  | Ty_int -> I.encode_cvtop
  | Ty_real -> Real.encode_cvtop
  | Ty_bool -> Boolean.encode_cvtop
  | Ty_str -> Str.encode_cvtop
  | Ty_bitv sz -> Bv.encode_cvtop sz
  | Ty_fp sz -> Fp.encode_cvtop sz
  | (Ty_list | Ty_app | Ty_unit | Ty_none | Ty_regexp) as op ->
    Fmt.failwith "Trying to encode unsupported op of type %a" Ty.pp op

(*let symbol_to_var v =
  DExpr.Term.Var.mk (Symbol.to_string v) (tty_of_etype (Symbol.type_of v))*)

(* let encode_unviversal_quantifier (vars_list : Symbol.t list) (body : expr)
     (_patterns : expr list) : expr =
     (* TODO: support triggers *)
     let vars = List.map symbol_to_var vars_list in
     DTerm.all ([], vars) body

   let encore_existential_quantifier (vars_list : Symbol.t list) (body : expr)
     (_patterns : expr list) : expr =
     (* TODO: support triggers *)
     let vars = List.map symbol_to_var vars_list in
     DTerm.ex ([], vars) body
*)
let encode_expr_aux ?(record_sym = fun _ -> ()) (e : Expr.t) : expr =
  let open Expr in
  let rec aux (hte : t) =
    match view hte with
    | Val v -> encode_val v
    | Ptr { base; offset } ->
      let base' = encode_val (Num (I32 base)) in
      let offset' = aux offset in
      DTerm.Bitv.add base' offset'
    | Symbol s ->
      let cst = tcst_of_symbol s in
      record_sym cst;
      DTerm.of_cst cst
    | Unop (ty, op, e) ->
      let e' = aux e in
      encode_unop ty op e'
    | Binop (ty, op, e1, e2) ->
      let e1' = aux e1 in
      let e2' = aux e2 in
      encode_binop ty op e1' e2'
    | Triop (ty, op, e1, e2, e3) ->
      let e1' = aux e1
      and e2' = aux e2
      and e3' = aux e3 in
      encode_triop ty op e1' e2' e3'
    | Relop (ty, op, e1, e2) ->
      let e1' = aux e1
      and e2' = aux e2 in
      encode_relop ty op e1' e2'
    | Cvtop (ty, op, e) ->
      let e' = aux e in
      encode_cvtop ty op e'
    | Extract (e, h, l) ->
      let e' = aux e in
      DTerm.Bitv.extract ((h * 8) - 1) (l * 8) e'
    | Concat (e1, e2) ->
      let e1' = aux e1
      and e2' = aux e2 in
      DTerm.Bitv.concat e1' e2'
    | Naryop _ | List _ | App _ | Binder _ ->
      Fmt.failwith {|Unsupported expr %a|} Expr.pp hte
    (* | Quantifier (t, vars, body, patterns) -> (
       let body' = aux body in
       let encode_pattern (p : t list) =
         DTerm.multi_trigger (List.map aux p)
       in
       let patterns' = List.map encode_pattern patterns in
       match t with
       | Forall -> encode_unviversal_quantifier vars body' patterns'
       | Exists -> encore_existential_quantifier vars body' patterns' ) *)
  in
  aux e

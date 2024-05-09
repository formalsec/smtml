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

open Smtml

module Fresh = struct
  module DExpr = Colibri2_core.Expr
  module DTy = DExpr.Ty
  module DTerm = DExpr.Term
  module A = Colibri2_stdlib.Std.A
  module LRA = Colibri2_theories_LRA
  module Scheduler = Colibri2_solver.Scheduler
  module Context = Colibri2_stdlib.Context
  module Interp = Colibri2_core.Interp
  module Uninterp = Colibri2_theories_quantifiers.Uninterp
  module Ground = Colibri2_core.Ground
  module IArray = Colibri2_popop_lib.IArray
  module Egraph = Colibri2_core.Egraph

  module Var = struct
    include DTerm.Var

    let is_int _ = false

    let print = pp
  end

  module Ex = struct
    type t = unit

    let print fmt () = Fmt.pf fmt "()"

    let empty = ()

    let union () () = ()
  end

  module Rat = struct
    include A

    let m_one = A.minus_one

    let print = A.pp

    let is_int = A.is_integer

    let is_zero v = A.equal v A.zero

    let is_one v = A.equal v A.one

    let mult = A.mul

    let minus = A.neg

    let is_m_one v = A.equal v m_one

    let ceiling = ceil
  end

  let err = Log.err

  module Make () = struct
    exception Error of string

    type expr = DTerm.t

    type model =
      Colibri2_core.Egraph.wt * (DTerm.Const.t * Colibri2_core.Value.t) list

    module Sim = OcplibSimplex.Basic.Make (Var) (Rat) (Ex)

    type optimize = Sim.Core.t

    type handle = optimize * (Sim.Core.P.t * bool) option

    type status =
      [ `Sat of Colibri2_core.Egraph.wt
      | `Unknown of Colibri2_core.Egraph.wt
      | `Search
      | `Unsat
      | `StepLimitReached
      ]

    type solver =
      { mutable scheduler : Scheduler.t
      ; mutable pushpop : Scheduler.bp list
      ; mutable state : status
      ; mutable status_colibri :
          [ `No | `Sat | `Unsat | `Unknown | `StepLimitReached ] Context.Ref.t
      ; mutable decls : DTerm.Const.S.t
      }

    (* additional builtins *)

    let string_ty_cst : DExpr.Term.ty_const =
      DExpr.Id.mk ~builtin:DExpr.Base
        (Dolmen_std.Path.global "StringTy")
        DExpr.{ arity = 0; alias = No_alias }

    let string_ty = DTy.apply string_ty_cst []

    let float32_ty = DTy.float 8 24

    let float64_ty = DTy.float 11 53

    let int_to_string : DExpr.term_cst =
      DExpr.Id.mk ~name:"IntToString" ~builtin:DExpr.Base
        (Dolmen_std.Path.global "IntToString")
        (DTy.arrow [ DTy.int ] string_ty)

    let string_to_int : DExpr.term_cst =
      DExpr.Id.mk ~name:"StringToInt" ~builtin:DExpr.Base
        (Dolmen_std.Path.global "StringToInt")
        (DTy.arrow [ string_ty ] DTy.int)

    let real_to_string : DExpr.term_cst =
      DExpr.Id.mk ~name:"RealToString" ~builtin:DExpr.Base
        (Dolmen_std.Path.global "RealToString")
        (DTy.arrow [ DTy.real ] string_ty)

    let string_to_real : DExpr.term_cst =
      DExpr.Id.mk ~name:"StringToReal" ~builtin:DExpr.Base
        (Dolmen_std.Path.global "StringToReal")
        (DTy.arrow [ string_ty ] DTy.real)

    let real_to_uint32 : DExpr.term_cst =
      DExpr.Id.mk ~name:"RealToUInt32" ~builtin:DExpr.Base
        (Dolmen_std.Path.global "RealToUInt32")
        (DTy.arrow [ DTy.real ] DTy.real)

    let trim_string : DExpr.term_cst =
      DExpr.Id.mk ~name:"TrimString" ~builtin:DExpr.Base
        (Dolmen_std.Path.global "TrimString")
        (DTy.arrow [ string_ty ] string_ty)

    let f32_to_string : DExpr.term_cst =
      DExpr.Id.mk ~name:"F32ToString" ~builtin:DExpr.Base
        (Dolmen_std.Path.global "F32ToString")
        (DTy.arrow [ float32_ty ] string_ty)

    let string_to_f32 : DExpr.term_cst =
      DExpr.Id.mk ~name:"StringToF32" ~builtin:DExpr.Base
        (Dolmen_std.Path.global "StringToF32")
        (DTy.arrow [ string_ty ] float32_ty)

    let f64_to_string : DExpr.term_cst =
      DExpr.Id.mk ~name:"F64ToString" ~builtin:DExpr.Base
        (Dolmen_std.Path.global "F64ToString")
        (DTy.arrow [ float64_ty ] string_ty)

    let string_to_f64 : DExpr.term_cst =
      DExpr.Id.mk ~name:"StringToF64" ~builtin:DExpr.Base
        (Dolmen_std.Path.global "StringToF64")
        (DTy.arrow [ string_ty ] float64_ty)

    module SHT = Hashtbl.Make (struct
      include Symbol

      let hash = Hashtbl.hash
    end)

    let tty_of_etype (e : Ty.t) : DTerm.ty =
      match e with
      | Ty_int -> DTy.int
      | Ty_real -> DTy.real
      | Ty_bool -> DTy.bool
      | Ty_str -> string_ty
      | Ty_bitv 8 -> DTy.bitv 8
      | Ty_bitv 32 -> DTy.bitv 32
      | Ty_bitv 64 -> DTy.bitv 64
      | Ty_fp 32 -> float32_ty
      | Ty_fp 64 -> float64_ty
      | Ty_fp _ | Ty_bitv _ | Ty_list | Ty_array | Ty_tuple -> assert false

    let tty_to_etype (ty : DTerm.ty) : Ty.t =
      match ty with
      | { ty_descr = TyApp ({ builtin = DExpr.Int; _ }, _); _ } -> Ty_int
      | { ty_descr = TyApp ({ builtin = DExpr.Real; _ }, _); _ } -> Ty_real
      | { ty_descr = TyApp ({ builtin = DExpr.Prop; _ }, _); _ } -> Ty_bool
      | { ty_descr =
            TyApp
              ( { builtin = DExpr.Base
                ; path = Absolute { name = "StringTy"; _ }
                ; _
                }
              , _ )
        ; _
        } ->
        Ty_str
      | { ty_descr = TyApp ({ builtin = DExpr.Bitv 32; _ }, _); _ } ->
        Ty_bitv 32
      | { ty_descr = TyApp ({ builtin = DExpr.Bitv 64; _ }, _); _ } ->
        Ty_bitv 64
      | { ty_descr = TyApp ({ builtin = DExpr.Float (8, 24); _ }, _); _ } ->
        Ty_fp 32
      | { ty_descr = TyApp ({ builtin = DExpr.Float (11, 53); _ }, _); _ } ->
        Ty_fp 64
      | _ -> assert false

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
      | { builtin = DExpr.Base
        ; path = Local { name } | Absolute { name; _ }
        ; id_ty
        ; _
        } ->
        Symbol.make (tty_to_etype id_ty) name
      | _ -> assert false

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
          | _ -> err {|Int: Unsupported Z3 unop operator "%a"|} Ty.pp_unop op
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
            fun e1 e2 ->
              DTerm.apply_cst
                Colibri2_theories_LRA.RealValue.Builtin.colibri_pow_int_int []
                [ e1; e2 ]
          | _ -> raise (Error "Unsupported integer operations")
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
            err {|Arith: Unsupported Z3 relop operator "%a"|} Ty.pp_relop op
        in
        op' e1 e2

      let encode_cvtop op e =
        let op' =
          match op with
          | ToString -> fun v -> DTerm.apply_cst int_to_string [] [ v ]
          | OfString -> fun v -> DTerm.apply_cst string_to_int [] [ v ]
          | _ -> err {|Int: Unsupported Z3 cvtop operator "%a"|} Ty.pp_cvtop op
        in
        op' e

      let encode_triop op _ _ _ =
        err {|Arith: Unsupported Z3 triop operator "%a"|} Ty.pp_triop op
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
            fun e ->
              DTerm.apply_cst
                Colibri2_theories_LRA.RealValue.Builtin.colibri_ceil [] [ e ]
          | Floor -> DTerm.Real.floor
          | Nearest | Is_nan | _ ->
            err {|Real: Unsupported Z3 cvtop operator "%a"|} Ty.pp_unop op
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
          | _ -> err {|Real: Unsupported Z3 binop operator "%a"|} Ty.pp_binop op
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
            err {|Arith: Unsupported Z3 relop operator "%a"|} Ty.pp_relop op
        in
        op' e1 e2

      let encode_cvtop op e =
        let op' =
          match op with
          | ToString -> fun v -> DTerm.apply_cst real_to_string [] [ v ]
          | OfString -> fun v -> DTerm.apply_cst string_to_real [] [ v ]
          | ConvertUI32 -> fun t -> DTerm.apply_cst real_to_uint32 [] [ t ]
          | Reinterpret_int -> DTerm.Int.to_real
          | _ -> err {|Real: Unsupported Z3 cvtop operator "%a"|} Ty.pp_cvtop op
        in
        op' e

      let encode_triop op _ _ _ =
        err {|Arith: Unsupported Z3 triop operator "%a"|} Ty.pp_triop op
    end

    module Boolean = struct
      open Ty

      let encode_unop op e =
        let op' = match op with Not -> DTerm.neg | _ -> assert false in
        op' e

      let encode_binop op e1 e2 =
        let op' =
          match op with
          | And -> fun a b -> DTerm._and [ a; b ]
          | Or -> fun a b -> DTerm._or [ a; b ]
          | Xor -> DTerm.xor
          | _ -> assert false
        in
        op' e1 e2

      let encode_relop op e1 e2 =
        let op' =
          match op with Eq -> DTerm.eq | Ne -> DTerm.neq | _ -> assert false
        in
        op' e1 e2

      let encode_cvtop _op _e = assert false

      let encode_triop op e1 e2 e3 =
        let op' = match op with Ite -> DTerm.ite | _ -> assert false in
        op' e1 e2 e3
    end

    module Str = struct
      open Ty

      let encode_unop op e =
        let op' =
          match op with
          | Seq_length -> assert false
          | Trim -> fun _v -> assert false
          | _ -> assert false
        in
        op' e

      let encode_binop op _e1 _e2 =
        let op' =
          match op with
          | Seq_at -> assert false
          | Seq_concat -> assert false
          | Seq_prefix -> assert false
          | Seq_suffix -> assert false
          | Seq_contains -> assert false
          | _ -> assert false
        in
        op'

      let encode_relop op =
        let op' =
          match op with
          | Eq -> DTerm.eq
          | Ne -> DTerm.neq
          | _ -> err {|Str: Unsupported Z3 relop operator "%a"|} Ty.pp_relop op
        in
        op'

      let encode_triop op _e1 _e2 _e3 =
        let op' =
          match op with
          | Seq_extract -> assert false
          | Seq_replace -> assert false
          | Seq_index -> assert false
          | _ -> err {|Str: Unsupported Z3 triop operator "%a"|} Ty.pp_triop op
        in
        op'

      let encode_cvtop _op _e = assert false
    end

    module Bv = struct
      open Ty

      let encode_val (type a) (cast : a Ty.cast) (i : a) =
        match cast with
        | C8 ->
          let n = if i >= 0 then i else i land ((1 lsl 8) - 1) in
          (* necessary to have the same behaviour as Z3 *)
          DTerm.Bitv.mk
            (Dolmen_type.Misc.Bitv.parse_decimal ("bv" ^ Int.to_string n) 8)
        | C32 ->
          let iint = Int32.to_int i in
          let n = if iint >= 0 then iint else iint land ((1 lsl 32) - 1) in
          (* necessary to have the same behaviour as Z3 *)
          DTerm.Bitv.mk
            (Dolmen_type.Misc.Bitv.parse_decimal ("bv" ^ Int.to_string n) 32)
        | C64 ->
          let n =
            if Int64.compare i Int64.zero >= 0 then Z.of_int64 i
            else Z.logand (Z.of_int64 i) (Z.sub (Z.( lsl ) Z.one 64) Z.one)
          in
          (* necessary to have the same behaviour as Z3 *)
          DTerm.Bitv.mk
            (Dolmen_type.Misc.Bitv.parse_decimal ("bv" ^ Z.to_string n) 64)

      let encode_unop op e =
        let op' =
          match op with
          | Not -> DTerm.Bitv.not
          | Neg -> DTerm.Bitv.neg
          | _ -> err {|Bv: Unsupported Z3 unary operator "%a"|} Ty.pp_unop op
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
          | _ -> err {|Bv: Unsupported Z3 binary operator "%a"|} Ty.pp_binop op
        in
        op' e1 e2

      let encode_triop op _ =
        err {|Bv: Unsupported Z3 triop operator "%a"|} Ty.pp_triop op

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
            | WrapI64 -> assert false
            | TruncSF32 | TruncSF64 ->
              DTerm.Float.to_sbv 32 DTerm.Float.roundTowardZero
            | TruncUF32 | TruncUF64 ->
              DTerm.Float.to_ubv 32 DTerm.Float.roundTowardZero
            | Reinterpret_float -> DTerm.Float.ieee_format_to_fp 8 24
            | ToBool -> encode_relop Ne (encode_val C32 0l)
            | OfBool ->
              fun e -> DTerm.ite e (encode_val C32 1l) (encode_val C32 0l)
            | _ -> assert false )
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
            | OfBool ->
              fun e -> DTerm.ite e (encode_val C64 1L) (encode_val C64 0L)
            | _ -> assert false )
          | _ -> assert false
        in
        op' e
    end

    module Fp = struct
      open Ty

      let encode_val (type a) (sz : a Ty.cast) (f : a) =
        match sz with
        | C8 -> err "Unable to create FP numeral using 8 bits"
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
          | _ -> err {|Fp: Unsupported Z3 unary operator "%a"|} Ty.pp_unop op
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
          | _ -> err {|Fp: Unsupported Z3 binop operator "%a"|} Ty.pp_binop op
        in
        op' e1 e2

      let encode_triop op _ =
        err {|Fp: Unsupported Z3 triop operator "%a"|} Ty.pp_triop op

      let encode_relop op e1 e2 =
        let op' =
          match op with
          | Eq -> DTerm.Float.eq
          | Ne -> fun e1 e2 -> DTerm.Float.eq e1 e2 |> DTerm.neg
          | Lt -> DTerm.Float.lt
          | Le -> DTerm.Float.leq
          | Gt -> DTerm.Float.gt
          | Ge -> DTerm.Float.geq
          | _ -> err {|Fp: Unsupported Z3 relop operator "%a"|} Ty.pp_relop op
        in
        op' e1 e2

      let encode_cvtop sz op e =
        let op' =
          match sz with
          | 32 -> (
            match op with
            | DemoteF64 ->
              DTerm.Float.to_fp 8 24 DTerm.Float.roundNearestTiesToEven
            | ConvertSI32 | ConvertSI64 ->
              DTerm.Float.sbv_to_fp 8 24 DTerm.Float.roundNearestTiesToEven
            | ConvertUI32 | ConvertUI64 ->
              DTerm.Float.ubv_to_fp 8 24 DTerm.Float.roundNearestTiesToEven
            | Reinterpret_int -> DTerm.Float.ieee_format_to_fp 8 24
            | ToString -> fun v -> DTerm.apply_cst f32_to_string [] [ v ]
            | OfString -> fun v -> DTerm.apply_cst string_to_f32 [] [ v ]
            | _ -> assert false )
          | 64 -> (
            match op with
            | PromoteF32 ->
              DTerm.Float.to_fp 11 53 DTerm.Float.roundNearestTiesToEven
            | ConvertSI32 | ConvertSI64 ->
              DTerm.Float.sbv_to_fp 11 53 DTerm.Float.roundNearestTiesToEven
            | ConvertUI32 | ConvertUI64 ->
              DTerm.Float.ubv_to_fp 11 53 DTerm.Float.roundNearestTiesToEven
            | Reinterpret_int -> DTerm.Float.ieee_format_to_fp 11 53
            | ToString -> fun v -> DTerm.apply_cst f64_to_string [] [ v ]
            | OfString -> fun v -> DTerm.apply_cst string_to_f64 [] [ v ]
            | _ -> assert false )
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

    let encode_unop = function
      | Ty.Ty_int -> I.encode_unop
      | Ty.Ty_real -> Real.encode_unop
      | Ty.Ty_bool -> Boolean.encode_unop
      | Ty.Ty_str -> Str.encode_unop
      | Ty.Ty_bitv _ -> Bv.encode_unop
      | Ty.Ty_fp _ -> Fp.encode_unop
      | Ty.Ty_list | Ty_array | Ty_tuple -> assert false

    let encode_binop = function
      | Ty.Ty_int -> I.encode_binop
      | Ty.Ty_real -> Real.encode_binop
      | Ty.Ty_bool -> Boolean.encode_binop
      | Ty.Ty_str -> Str.encode_binop
      | Ty.Ty_bitv _ -> Bv.encode_binop
      | Ty.Ty_fp _ -> Fp.encode_binop
      | Ty.Ty_list | Ty_array | Ty_tuple -> assert false

    let encode_triop = function
      | Ty.Ty_int -> I.encode_triop
      | Ty.Ty_real -> Real.encode_triop
      | Ty.Ty_bool -> Boolean.encode_triop
      | Ty.Ty_str -> Str.encode_triop
      | Ty.Ty_bitv _ -> Bv.encode_triop
      | Ty.Ty_fp _ -> Fp.encode_triop
      | Ty.Ty_list | Ty_array | Ty_tuple -> assert false

    let encode_relop = function
      | Ty.Ty_int -> I.encode_relop
      | Ty.Ty_real -> Real.encode_relop
      | Ty.Ty_bool -> Boolean.encode_relop
      | Ty.Ty_str -> Str.encode_relop
      | Ty.Ty_bitv _ -> Bv.encode_relop
      | Ty.Ty_fp _ -> Fp.encode_relop
      | Ty.Ty_list | Ty_array | Ty_tuple -> assert false

    let encode_cvtop = function
      | Ty.Ty_int -> I.encode_cvtop
      | Ty.Ty_real -> Real.encode_cvtop
      | Ty.Ty_bool -> Boolean.encode_cvtop
      | Ty.Ty_str -> Str.encode_cvtop
      | Ty.Ty_bitv sz -> Bv.encode_cvtop sz
      | Ty.Ty_fp sz -> Fp.encode_cvtop sz
      | Ty.Ty_list | Ty_array | Ty_tuple -> assert false

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
    let encore_expr_aux ?(record_sym = fun _ -> ()) (e : Expr.t) : expr =
      let open Expr in
      let rec aux (hte : t) =
        match view hte with
        | Val v -> encode_val v
        | Ptr (base, offset) ->
          let base' = encode_val (Num (I32 base)) in
          let offset' = aux offset in
          DTerm.Bitv.add base' offset'
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
        | Symbol s ->
          let cst = tcst_of_symbol s in
          record_sym cst;
          DTerm.of_cst cst
        | Extract (e, h, l) ->
          let e' = aux e in
          DTerm.Bitv.extract ((h * 8) - 1) (l * 8) e'
        | Concat (e1, e2) ->
          let e1' = aux e1
          and e2' = aux e2 in
          DTerm.Bitv.concat e1' e2'
        | List _ | Array _ | Tuple _ | App _ -> assert false
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

    let () =
      let term_app1 env s f =
        Dolmen_loop.Typer.T.builtin_term
          (Dolmen_type.Base.term_app1
             (module Dolmen_loop.Typer.T)
             env s
             (fun a -> DExpr.Term.apply_cst f [ a.DExpr.term_ty ] [ a ]) )
      in
      DExpr.add_builtins (fun env s ->
          match s with
          | Dolmen_loop.Typer.T.Id { ns = Term; name = Simple "StringToInt" } ->
            term_app1 env s string_to_int
          | Dolmen_loop.Typer.T.Id { ns = Term; name = Simple "RealToString" }
            ->
            term_app1 env s real_to_string
          | Dolmen_loop.Typer.T.Id { ns = Term; name = Simple "StringToReal" }
            ->
            term_app1 env s string_to_real
          | Dolmen_loop.Typer.T.Id { ns = Term; name = Simple "TrimString" } ->
            term_app1 env s trim_string
          | Dolmen_loop.Typer.T.Id { ns = Term; name = Simple "F32ToString" } ->
            term_app1 env s f32_to_string
          | Dolmen_loop.Typer.T.Id { ns = Term; name = Simple "StringToF32" } ->
            term_app1 env s string_to_f32
          | Dolmen_loop.Typer.T.Id { ns = Term; name = Simple "F64ToString" } ->
            term_app1 env s f64_to_string
          | Dolmen_loop.Typer.T.Id { ns = Term; name = Simple "StringToF64" } ->
            term_app1 env s string_to_f64
          | _ -> `Not_found )

    (* let add_default_axioms env =
       (* string_to_alpha (alpha_to_string x) = x
          alpha_to_string (string_to_alpha x) = x *)
       let add_string_axiom =
         let convert ~subst env =
           Colibri2_theories_quantifiers.Subst.convert ~subst_new:subst env
         in
         let mk_eq env subst t1 t2 =
           let n1 = convert ~subst env t1 in
           let n2 = convert ~subst env t2 in
           Egraph.register env n1;
           Egraph.register env n2;
           let eq = Colibri2_theories_bool.Equality.equality env [ n1; n2 ] in
           Egraph.register env eq;
           Colibri2_theories_bool.Boolean.set_true env eq
         in
         fun env to_string of_string ty ->
           let x_str = DTerm.Var.mk "x_str" string_ty in
           let xt_str = DTerm.of_var x_str in
           let term_str =
             DTerm.apply_cst to_string []
               [ DTerm.apply_cst of_string [] [ xt_str ] ]
           in
           let x = DTerm.Var.mk "x" ty in
           let xt = DTerm.of_var x in
           let term =
             DTerm.apply_cst of_string [] [ DTerm.apply_cst to_string [] [ xt ] ]
           in
           let pattern_1 =
             Colibri2_theories_quantifiers.Pattern.of_term_exn term_str
           in
           let pattern_2 =
             Colibri2_theories_quantifiers.Pattern.of_term_exn term
           in
           let run_1 env subst = mk_eq env subst xt_str term_str in
           let run_2 env subst = mk_eq env subst xt term in
           Colibri2_theories_quantifiers.InvertedPath.add_callback env pattern_1
             run_1;
           Colibri2_theories_quantifiers.InvertedPath.add_callback env pattern_2
             run_2
       in
       add_string_axiom env int_to_string string_to_int DTy.int;
       add_string_axiom env real_to_string string_to_real DTy.real;
       add_string_axiom env f32_to_string string_to_f32 float32_ty;
       add_string_axiom env f64_to_string string_to_f64 float64_ty *)

    let encode_expr e = encore_expr_aux e

    let pp_smt ?status:_ _ _ = ()

    let satisfiability = function
      | `Sat _ -> `Sat
      | `Unknown _ -> `Unknown
      | `Unsat -> `Unsat
      | `Search -> assert false
      | `StepLimitReached -> assert false

    module Solver = struct
      let mk_scheduler () =
        let scheduler = Scheduler.new_solver ~learning:false () in
        Scheduler.init_theories
          ~theories:
            ( Colibri2_theories_bool.Ite.th_register :: LRA.LRA.th_register
            :: Colibri2_theories_fp.Fp.th_register
            :: Colibri2_core.ForSchedulers.default_theories () )
          scheduler;
        scheduler

      let make ?params:_ ?logic:_ () =
        let scheduler = mk_scheduler () in
        let ctx = Scheduler.get_context scheduler in
        { scheduler
        ; pushpop = []
        ; state = `Search
        ; status_colibri = Context.Ref.create ctx `No
        ; decls = DTerm.Const.S.empty
        }

      let add_simplifier s = s

      let clone { pushpop; state; status_colibri; decls; _ } =
        let scheduler = mk_scheduler () in
        { scheduler; pushpop; state; status_colibri; decls }

      let push st = st.pushpop <- Scheduler.push st.scheduler :: st.pushpop

      let rec pop st i =
        assert (0 <= i);
        match (i, st.pushpop) with
        | 0, _ -> ()
        | _, [] -> assert false
        | 1, bp :: l ->
          st.pushpop <- l;
          Scheduler.pop_to st.scheduler bp
        | n, _ :: l ->
          st.pushpop <- l;
          pop st (n - 1)

      let reset s =
        let scheduler = mk_scheduler () in
        let ctx = Scheduler.get_context scheduler in
        s.scheduler <- scheduler;
        s.pushpop <- [];
        s.state <- `Search;
        s.status_colibri <- Context.Ref.create ctx `No;
        s.decls <- DTerm.Const.S.empty

      let add s es =
        Scheduler.add_assertion s.scheduler (fun d ->
            let es' =
              List.map
                (encore_expr_aux ~record_sym:(fun c ->
                     s.decls <- DTerm.Const.S.add c s.decls ) )
                es
            in
            List.iter
              (fun e ->
                let n = Colibri2_core.Ground.convert d e in
                Colibri2_core.Egraph.register d n;
                Colibri2_theories_bool.Boolean.set_true d n )
              es' )

      let check s ~assumptions =
        add s assumptions;
        satisfiability @@ Scheduler.check_sat s.scheduler

      let model s : model option =
        match Scheduler.check_sat s.scheduler with
        | `Sat d | `Unknown d ->
          let l =
            DTerm.Const.S.fold_left
              (fun acc c ->
                let e = DExpr.Term.of_cst c in
                let v = Interp.interp d e in
                (c, v) :: acc )
              [] s.decls
          in
          Some (d, l)
        | `Unsat -> assert false
        | `StepLimitReached -> assert false
        | `Search -> assert false

      let interrupt _ = ()

      let pp_statistics _ _ = ()
    end

    module Optimizer = struct
      let make () : optimize = Sim.Core.empty ~is_int:false ~check_invs:false

      let push _ = ()

      let pop _ = ()

      let add _ _ = assert false

      let check _ = assert false

      let model o =
        match Sim.Result.get None o with
        | Sim.Core.Sat s ->
          let _model = (Lazy.force s).Sim.Core.main_vars in
          (* let l = List.map (fun (n, av) -> (n, LRA.RealValue.of_value av)) model in
             Some l *)
          None
        | Sim.Core.Unknown | Sim.Core.Unsat _ | Sim.Core.Unbounded _
        | Sim.Core.Max (_, _) ->
          None

      let maximize _ _ = assert false

      let minimize _ _ = assert false

      let interrupt _ = ()

      let pp_statistics _ _ = ()
    end

    let c2value_to_value (ty : Ty.t) (v : Colibri2_core.Value.t) =
      match ty with
      | Ty_bool -> (
        match
          Colibri2_core.Value.value Colibri2_theories_bool.Boolean.BoolValue.key
            v
        with
        | Some true -> Some Value.True
        | Some false -> Some Value.False
        | None -> None )
      | Ty_int | Ty_real -> (
        match
          Colibri2_core.Value.value Colibri2_theories_LRA.RealValue.key v
        with
        | Some a when A.is_integer a -> Some (Value.Int (A.to_int a))
        | Some a when A.is_real a ->
          Some (Value.Real (Stdlib.Float.of_string (A.to_string a)))
        | Some _ | None -> None )
      | Ty_str | Ty_bitv _ | Ty_fp _ | Ty_list | Ty_array | Ty_tuple ->
        assert false

    (* let value_of_const ((d, _l) : model) (e : Expr.t) : Value.t option =
       let e' = encore_expr_aux e in
       let v = Colibri2_core.Interp.interp d e' in
       c2value_to_value e.ty v *)

    let value (e, _) (c : Expr.t) : Value.t =
      let c2v = Interp.interp e (encode_expr c) in
      match c2value_to_value (Expr.ty c) c2v with
      | None -> assert false
      | Some v -> v

    let values_of_model ?(symbols : Symbol.t list option) ((_, model) : model) :
      Model.t =
      let m = Hashtbl.create 512 in
      match symbols with
      | Some symbols ->
        List.iter
          (fun sy ->
            let c = tcst_of_symbol sy in
            match List.assoc_opt c model with
            | Some v -> (
              match c2value_to_value (tty_to_etype c.DExpr.id_ty) v with
              | Some data -> Hashtbl.add m (tcst_to_symbol c) data
              | None -> () )
            | _ -> () )
          symbols;
        m
      | None ->
        List.iter
          (fun (c, v) ->
            match c2value_to_value (tty_to_etype c.DExpr.id_ty) v with
            | Some data -> Hashtbl.add m (tcst_to_symbol c) data
            | None -> () )
          model;
        m

    let set_debug = Colibri2_stdlib.Debug.set_info_flags
  end
end

include Fresh.Make ()

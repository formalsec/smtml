(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2024 formalsec *)
(* Written by Hichem Rami Ait El Hara *)

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

  (* let f32_to_string : DExpr.term_cst = *)
  (*   DExpr.Id.mk ~name:"F32ToString" ~builtin:DBuiltin.Base *)
  (*     (Dolmen_std.Path.global "F32ToString") *)
  (*     (DTy.arrow [ float32_ty ] string_ty) *)

  (* let string_to_f32 : DExpr.term_cst = *)
  (*   DExpr.Id.mk ~name:"StringToF32" ~builtin:DBuiltin.Base *)
  (*     (Dolmen_std.Path.global "StringToF32") *)
  (*     (DTy.arrow [ string_ty ] float32_ty) *)

  (* let f64_to_string : DExpr.term_cst = *)
  (*   DExpr.Id.mk ~name:"F64ToString" ~builtin:DBuiltin.Base *)
  (*     (Dolmen_std.Path.global "F64ToString") *)
  (*     (DTy.arrow [ float64_ty ] string_ty) *)

  (* let string_to_f64 : DExpr.term_cst = *)
  (*   DExpr.Id.mk ~name:"StringToF64" ~builtin:DBuiltin.Base *)
  (*     (Dolmen_std.Path.global "StringToF64") *)
  (*     (DTy.arrow [ string_ty ] float64_ty) *)
end

(* let tcst_to_symbol (c : DExpr.term_cst) : Symbol.t = *)
(*   match c with *)
(*   | { builtin = DBuiltin.Base *)
(*     ; path = Local { name } | Absolute { name; _ } *)
(*     ; id_ty *)
(*     ; _ *)
(*     } -> *)
(*     Symbol.make (tty_to_etype id_ty) name *)
(*   | _ -> Fmt.failwith {|Unsupported constant term "%a"|} DExpr.Print.term_cst c *)

type ty = DTy.t

type term = DTerm.t

let true_ = DTerm.of_cst DTerm.Const._true

let false_ = DTerm.of_cst DTerm.Const._false

let int x = DTerm.Int.mk (Int.to_string x)

let real x = DTerm.Real.mk (Float.to_string x)

let const sym ty = DTerm.Const.mk (Dolmen_std.Path.global sym) ty

let not_ t = DTerm.neg t

let and_ t1 t2 = DTerm._and [ t1; t2 ]

let or_ t1 t2 = DTerm._or [ t1; t2 ]

let logand ts = DTerm._and ts

let logor ts = DTerm._or ts

let xor t1 t2 = DTerm.xor t1 t2

let eq t1 t2 = DTerm.eq t1 t2

let distinct ts = DTerm.distinct ts

let ite c r1 r2 = DTerm.ite c r1 r2

module Types = struct
  let int = DTy.int

  let real = DTy.real

  let bool = DTy.bool

  let string = Builtin.string_ty

  let bitv m = DTy.bitv m

  let float ebits sbits = DTy.float ebits sbits

  let to_ety (ty : ty) : Ty.t =
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
end

module Int = struct
  let neg t = DTerm.Int.minus t

  let to_real t = DTerm.Int.to_real t

  let add t1 t2 = DTerm.Int.add t1 t2

  let sub t1 t2 = DTerm.Int.sub t1 t2

  let mul t1 t2 = DTerm.Int.mul t1 t2

  let div t1 t2 = DTerm.Int.div t1 t2

  let rem t1 t2 = DTerm.Int.rem t1 t2

  let pow _ _ = assert false

  let lt t1 t2 = DTerm.Int.lt t1 t2

  let gt t1 t2 = DTerm.Int.gt t1 t2

  let le t1 t2 = DTerm.Int.le t1 t2

  let ge t1 t2 = DTerm.Int.ge t1 t2
end

module Real = struct
  let neg t = DTerm.Real.minus t

  let to_int t = DTerm.Real.to_int t

  let add t1 t2 = DTerm.Real.add t1 t2

  let sub t1 t2 = DTerm.Real.sub t1 t2

  let mul t1 t2 = DTerm.Real.mul t1 t2

  let div t1 t2 = DTerm.Real.div t1 t2

  let pow t1 t2 = DTerm.Real.pow t1 t2

  let lt t1 t2 = DTerm.Real.lt t1 t2

  let gt t1 t2 = DTerm.Real.gt t1 t2

  let le t1 t2 = DTerm.Real.le t1 t2

  let ge t1 t2 = DTerm.Real.ge t1 t2
end

module String = struct
  let v _x = assert false

  let length t = DTerm.String.length t

  let to_code t = DTerm.String.to_code t

  let of_code t = DTerm.String.of_code t

  let to_int t = DTerm.String.to_int t

  let of_int t = DTerm.String.of_int t

  let to_re t = DTerm.String.RegLan.of_string t

  let at t ~pos = DTerm.String.at t pos

  let concat _ts = assert false

  let contains t ~sub = DTerm.String.contains t sub

  let is_prefix t ~prefix = DTerm.String.is_prefix t prefix

  let is_suffix t ~suffix = DTerm.String.is_prefix t suffix

  let in_re _ _ = assert false

  let lt t1 t2 = DTerm.String.lt t1 t2

  let le t1 t2 = DTerm.String.leq t1 t2

  let sub t ~pos ~len = DTerm.String.sub t pos len

  let index_of t ~sub ~pos = DTerm.String.index_of t sub pos

  let replace t ~pattern ~with_ = DTerm.String.replace t pattern with_
end

module Re = struct
  let star t = DTerm.String.RegLan.star t

  let plus _t = assert false

  let opt _t = assert false

  let comp _t = assert false

  let range t1 t2 = DTerm.String.RegLan.range t1 t2

  let loop t1 t2 = DTerm.String.RegLan.loop t1 t2

  let union _ts = assert false

  let concat _ts = assert false
end

module Bitv = struct
  (* let encode_val (type a) (cast : a Ty.cast) (i : a) = *)
  (*   match cast with *)
  (*   | C8 -> *)
  (*     let n = if i >= 0 then i else i land ((1 lsl 8) - 1) in *)
  (*     (1* necessary to have the same behaviour as Z3 *1) *)
  (*     DTerm.Bitv.mk *)
  (*       (Dolmen_type.Misc.Bitv.parse_decimal *)
  (*          (String.cat "bv" (Int.to_string n)) *)
  (*          8 ) *)
  (*   | C32 -> *)
  (*     let iint = Int32.to_int i in *)
  (*     let n = if iint >= 0 then iint else iint land ((1 lsl 32) - 1) in *)
  (*     (1* necessary to have the same behaviour as Z3 *1) *)
  (*     DTerm.Bitv.mk *)
  (*       (Dolmen_type.Misc.Bitv.parse_decimal *)
  (*          (String.cat "bv" (Int.to_string n)) *)
  (*          32 ) *)
  (*   | C64 -> *)
  (*     let n = *)
  (*       if Int64.compare i Int64.zero >= 0 then Z.of_int64 i *)
  (*       else Z.logand (Z.of_int64 i) (Z.sub (Z.( lsl ) Z.one 64) Z.one) *)
  (*     in *)

  (*     (1* necessary to have the same behaviour as Z3 *1) *)
  (*     DTerm.Bitv.mk *)
  (*       (Dolmen_type.Misc.Bitv.parse_decimal *)
  (*          (String.cat "bv" (Z.to_string n)) *)
  (*          64 ) *)

  let neg t = DTerm.Bitv.neg t

  let lognot t = DTerm.Bitv.not t

  let add t1 t2 = DTerm.Bitv.add t1 t2

  let sub t1 t2 = DTerm.Bitv.sub t1 t2

  let mul t1 t2 = DTerm.Bitv.mul t1 t2

  let div t1 t2 = DTerm.Bitv.sdiv t1 t2

  let div_u t1 t2 = DTerm.Bitv.udiv t1 t2

  let logor t1 t2 = DTerm.Bitv.or_ t1 t2

  let logand t1 t2 = DTerm.Bitv.and_ t1 t2

  let logxor t1 t2 = DTerm.Bitv.xor t1 t2

  let shl t1 t2 = DTerm.Bitv.shl t1 t2

  let ashr t1 t2 = DTerm.Bitv.ashr t1 t2

  let lshr t1 t2 = DTerm.Bitv.lshr t1 t2

  let rem t1 t2 = DTerm.Bitv.srem t1 t2

  let rem_u t1 t2 = DTerm.Bitv.urem t1 t2

  let rotate_left _t1 _t2 = assert false

  let rotate_right _t1 _t2 = assert false

  let lt t1 t2 = DTerm.Bitv.slt t1 t2

  let lt_u t1 t2 = DTerm.Bitv.ult t1 t2

  let le t1 t2 = DTerm.Bitv.sle t1 t2

  let le_u t1 t2 = DTerm.Bitv.ule t1 t2

  let gt t1 t2 = DTerm.Bitv.sgt t1 t2

  let gt_u t1 t2 = DTerm.Bitv.ugt t1 t2

  let ge t1 t2 = DTerm.Bitv.sge t1 t2

  let ge_u t1 t2 = DTerm.Bitv.uge t1 t2

  let concat t1 t2 = DTerm.Bitv.concat t1 t2

  let extract t ~high ~low = DTerm.Bitv.extract high low t

  let zero_extend n t = DTerm.Bitv.zero_extend n t

  let sign_extend n t = DTerm.Bitv.sign_extend n t
end

module Float = struct
  (* let encode_val (type a) (sz : a Ty.cast) (f : a) = *)
  (*   match sz with *)
  (*   | C8 -> Fmt.failwith "Unable to create FP numeral using 8 bits" *)
  (*   | C32 -> DTerm.Float.ieee_format_to_fp 8 24 (Bv.encode_val C32 f) *)
  (*   | C64 -> DTerm.Float.ieee_format_to_fp 11 53 (Bv.encode_val C64 f) *)

  module Rouding_mode = struct
    let rne = DTerm.Float.roundNearestTiesToEven

    let rna = DTerm.Float.roundNearestTiesToAway

    let rtp = DTerm.Float.roundTowardPositive

    let rtn = DTerm.Float.roundTowardNegative

    let rtz = DTerm.Float.roundTowardZero
  end

  let neg t = DTerm.Float.neg t

  let abs t = DTerm.Float.abs t

  let sqrt ~rm t = DTerm.Float.sqrt rm t

  let is_nan t = DTerm.Float.isNaN t

  let round_to_integral ~rm t = DTerm.Float.roundToIntegral rm t

  let add ~rm t1 t2 = DTerm.Float.add rm t1 t2

  let sub ~rm t1 t2 = DTerm.Float.sub rm t1 t2

  let mul ~rm t1 t2 = DTerm.Float.mul rm t1 t2

  let div ~rm t1 t2 = DTerm.Float.div rm t1 t2

  let min t1 t2 = DTerm.Float.min t1 t2

  let max t1 t2 = DTerm.Float.max t1 t2

  let rem t1 t2 = DTerm.Float.rem t1 t2

  let eq t1 t2 = DTerm.Float.eq t1 t2

  let lt t1 t2 = DTerm.Float.lt t1 t2

  let le t1 t2 = DTerm.Float.leq t1 t2

  let gt t1 t2 = DTerm.Float.gt t1 t2

  let ge t1 t2 = DTerm.Float.geq t1 t2

  let to_fp ebits sbits ~rm t = DTerm.Float.to_fp ebits sbits rm t

  let sbv_to_fp ebits sbits ~rm t = DTerm.Float.sbv_to_fp ebits sbits rm t

  let ubv_to_fp ebits sbits ~rm t = DTerm.Float.ubv_to_fp ebits sbits rm t

  let to_ubv m ~rm t = DTerm.Float.to_ubv m rm t

  let to_sbv m ~rm t = DTerm.Float.to_sbv m rm t

  let of_ieee_bv ebits sbits t = DTerm.Float.ieee_format_to_fp ebits sbits t

  let to_ieee_bv _t = assert false
end

module Func = struct
  let make _name _args _ret = assert false

  let apply _func _args = assert false
end

(* let encode_val : Value.t -> expr = function *)
(*   | True -> DTerm.of_cst DTerm.Const._true *)
(*   | False -> DTerm.of_cst DTerm.Const._false *)
(*   | Int v -> I.encode_val v *)
(*   | Real v -> Real.encode_val v *)
(*   | Str _ -> assert false *)
(*   | Num (I8 x) -> Bv.encode_val C8 x *)
(*   | Num (I32 x) -> Bv.encode_val C32 x *)
(*   | Num (I64 x) -> Bv.encode_val C64 x *)
(*   | Num (F32 x) -> Fp.encode_val C32 x *)
(*   | Num (F64 x) -> Fp.encode_val C64 x *)
(*   | v -> Fmt.failwith {|Unsupported value "%a"|} Value.pp v *)

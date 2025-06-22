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

module DolmenIntf = struct
  include DTerm

  type ty = DTy.t

  type term = DTerm.t

  type func_decl = DTerm.Const.t

  let true_ = DTerm._true

  let false_ = DTerm._false

  let int i = DTerm.Int.mk (string_of_int i)

  let real r = DTerm.Real.mk (string_of_float r)

  let const s ty = DTerm.of_cst (DTerm.Const.mk (Dolmen_std.Path.global s) ty)

  let not_ = DTerm.neg

  let and_ a b = DTerm._and [ a; b ]

  let or_ a b = DTerm._or [ a; b ]

  let implies = DTerm.imply

  let logand = DTerm._and

  let logor = DTerm._or

  let get_vars_from_terms tl =
    List.map
      (fun (t : DTerm.t) ->
        match t.term_descr with
        | Var v -> v
        | _ ->
          Fmt.failwith {|Can't quantify non-variable term "%a"|} DTerm.print t )
      tl

  let forall tl t =
    let tvl = get_vars_from_terms tl in
    DTerm.all ([], tvl) t

  let exists (tl : term list) t =
    let tvl = get_vars_from_terms tl in
    DTerm.ex ([], tvl) t

  let nary_to_binary f tl =
    let rec aux acc = function
      | [] -> acc
      | h :: t -> aux (DTerm.apply_cst f [] [ acc; h ]) t
    in
    match tl with
    | h1 :: h2 :: t -> aux (DTerm.apply_cst f [] [ h1; h2 ]) t
    | _ ->
      Fmt.failwith {|%a applied to less than two terms|} DTerm.Const.print f

  let int_of_term (t : DTerm.t) =
    match t.term_descr with
    | Cst { builtin = DBuiltin.Bitvec i; _ } ->
      (* There may be a proper alternative to int_of_string somewhere,
         since its hidden by prelude. *)
      Z.to_int (Z.of_string i)
    | _ ->
      Fmt.failwith
        {|int_of_term: expected a term that is an integer constant, instead got: %a|}
        DTerm.print t

  module Types = struct
    include DTy

    let regexp = DTy.string_reg_lang

    let ty = DTerm.ty

    let to_ety (ty : DTy.t) : Ty.t =
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
      | { ty_descr = TyApp ({ builtin = DBuiltin.Bitv n; _ }, _); _ } ->
        Ty_bitv n
      | { ty_descr = TyApp ({ builtin = DBuiltin.Float (8, 24); _ }, _); _ } ->
        Ty_fp 32
      | { ty_descr = TyApp ({ builtin = DBuiltin.Float (11, 53); _ }, _); _ } ->
        Ty_fp 64
      | _ -> Fmt.failwith {|Unsupported dolmen type "%a"|} DTy.print ty
  end

  module Int = struct
    include DTerm.Int

    let neg = DTerm.Int.minus
  end

  module Real = struct
    include DTerm.Real

    let neg = DTerm.Real.minus
  end

  module String = struct
    include DTerm.String

    let v = DTerm.String.of_ustring

    let to_re = DTerm.String.RegLan.of_string

    let at t ~pos = DTerm.String.at t pos

    let concat = nary_to_binary DTerm.Const.String.concat

    let contains t ~sub = DTerm.String.contains t sub

    let is_prefix t ~prefix = DTerm.String.is_prefix t prefix

    let is_suffix t ~suffix = DTerm.String.is_suffix t suffix

    let le = DTerm.String.leq

    let sub t ~pos ~len = DTerm.String.sub t pos len

    let index_of t ~sub ~pos = DTerm.String.index_of t sub pos

    let replace t ~pattern ~with_ = DTerm.String.replace t pattern with_

    let replace_all t ~pattern ~with_ = DTerm.String.replace_all t pattern with_
  end

  module Re = struct
    let all () = DTerm.String.RegLan.all

    let allchar () = DTerm.String.RegLan.allchar

    let none () = DTerm.String.RegLan.empty

    let star = DTerm.String.RegLan.star

    let plus = DTerm.String.RegLan.cross

    let opt = DTerm.String.RegLan.option

    let comp = DTerm.String.RegLan.complement

    let range = DTerm.String.RegLan.range

    let inter = DTerm.String.RegLan.inter

    let loop t i1 i2 = DTerm.String.RegLan.loop i1 i2 t

    let union = nary_to_binary DTerm.Const.String.Reg_Lang.union

    let concat = nary_to_binary DTerm.Const.String.Reg_Lang.concat
  end

  module Bitv = struct
    include DTerm.Bitv

    let int_to_bitvector n bits =
      let two_pow_n = 1 lsl bits in
      let unsigned_bv = if n < 0 then two_pow_n + n else n in
      let rec to_bitlist acc n bits =
        if bits = 0 then acc
        else
          to_bitlist
            (Prelude.String.cat (string_of_int (n land 1)) acc)
            (n lsr 1) (bits - 1)
      in
      (* let bitlist = to_bitlist [] unsigned_bv bits in
      Fmt.str "%a" (Fmt.list Fmt.int) bitlist *)
      to_bitlist "" unsigned_bv bits

    let v (i : string) (n : int) =
      let bv = int_to_bitvector (Z.to_int (Z.of_string i)) n in
      DTerm.Bitv.mk bv

    let lognot = DTerm.Bitv.not

    let div = DTerm.Bitv.sdiv

    let div_u = DTerm.Bitv.udiv

    let logor = DTerm.Bitv.or_

    let logand = DTerm.Bitv.and_

    let logxor = DTerm.Bitv.xor

    let shl = DTerm.Bitv.shl

    let ashr = DTerm.Bitv.ashr

    let lshr = DTerm.Bitv.lshr

    let rem = DTerm.Bitv.srem

    let rem_u = DTerm.Bitv.urem

    let rotate_left t1 t2 = DTerm.Bitv.rotate_left (int_of_term t2) t1

    let rotate_right t1 t2 = DTerm.Bitv.rotate_right (int_of_term t2) t1

    let lt t1 t2 = DTerm.Bitv.slt t1 t2

    let lt_u t1 t2 = DTerm.Bitv.ult t1 t2

    let le = DTerm.Bitv.sle

    let le_u = DTerm.Bitv.ule

    let gt t1 t2 = DTerm.Bitv.sgt t1 t2

    let gt_u t1 t2 = DTerm.Bitv.ugt t1 t2

    let ge = DTerm.Bitv.sge

    let ge_u = DTerm.Bitv.uge

    let extract t ~high ~low = DTerm.Bitv.extract high low t
  end

  module Float = struct
    include DTerm.Float

    module Rounding_mode = struct
      let rne = DTerm.Float.roundNearestTiesToEven

      let rna = DTerm.Float.roundNearestTiesToAway

      let rtp = DTerm.Float.roundTowardPositive

      let rtn = DTerm.Float.roundTowardNegative

      let rtz = DTerm.Float.roundTowardZero
    end

    let v f e s =
      DTerm.Float.real_to_fp e s DTerm.Float.roundTowardZero
        (DTerm.Real.mk (Prelude.Float.to_string f))

    let sqrt ~rm t = DTerm.Float.sqrt rm t

    let is_normal = DTerm.Float.isNormal

    let is_subnormal = DTerm.Float.isSubnormal

    let is_negative = DTerm.Float.isNegative

    let is_positive = DTerm.Float.isPositive

    let is_infinite = DTerm.Float.isInfinite

    let is_nan = DTerm.Float.isNaN

    let is_zero = DTerm.Float.isZero

    let round_to_integral ~rm t = DTerm.Float.roundToIntegral rm t

    let add ~rm t1 t2 = DTerm.Float.add rm t1 t2

    let sub ~rm t1 t2 = DTerm.Float.sub rm t1 t2

    let mul ~rm t1 t2 = DTerm.Float.mul rm t1 t2

    let div ~rm t1 t2 = DTerm.Float.div rm t1 t2

    let fma ~rm a b c = DTerm.Float.fma rm a b c

    let le = DTerm.Float.leq

    let ge = DTerm.Float.geq

    let to_fp e s ~rm fp = DTerm.Float.to_fp e s rm fp

    let sbv_to_fp e s ~rm bv = DTerm.Float.sbv_to_fp e s rm bv

    let ubv_to_fp e s ~rm bv = DTerm.Float.ubv_to_fp e s rm bv

    let to_ubv n ~rm fp = DTerm.Float.to_ubv n rm fp

    let to_sbv n ~rm fp = DTerm.Float.to_sbv n rm fp

    let of_ieee_bv eb sb bv = DTerm.Float.ieee_format_to_fp eb sb bv

    let to_ieee_bv = None
  end

  module Func = struct
    let make name tyl ty =
      DTerm.Const.mk (Dolmen_std.Path.global name) (DTy.arrow tyl ty)

    let apply f tl = DTerm.apply_cst f [] tl
  end

  module Smtlib = struct
    let pp ?name:_ ?logic:_ ?status:_ = Fmt.list DTerm.print
  end
end

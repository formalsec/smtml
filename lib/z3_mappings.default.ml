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

include Mappings_intf

module Fresh = struct
  module Make () = struct
    type expr = Z3.Expr.expr

    type model = Z3.Model.model

    type solver = Z3.Solver.solver

    type optimize = Z3.Optimize.optimize

    type handle = Z3.Optimize.handle

    let ctx = Z3.mk_context []

    let int_sort = Z3.Arithmetic.Integer.mk_sort ctx

    let real_sort = Z3.Arithmetic.Real.mk_sort ctx

    let bool_sort = Z3.Boolean.mk_sort ctx

    let str_sort = Z3.Seq.mk_string_sort ctx

    let bv8_sort = Z3.BitVector.mk_sort ctx 8

    let bv32_sort = Z3.BitVector.mk_sort ctx 32

    let bv64_sort = Z3.BitVector.mk_sort ctx 64

    let fp32_sort = Z3.FloatingPoint.mk_sort_single ctx

    let fp64_sort = Z3.FloatingPoint.mk_sort_double ctx

    let rne = Z3.FloatingPoint.RoundingMode.mk_rne ctx

    let rtz = Z3.FloatingPoint.RoundingMode.mk_rtz ctx

    let rtp = Z3.FloatingPoint.RoundingMode.mk_rtp ctx

    let rtn = Z3.FloatingPoint.RoundingMode.mk_rtn ctx

    let get_sort (e : Ty.t) : Z3.Sort.sort =
      match e with
      | Ty_int -> int_sort
      | Ty_real -> real_sort
      | Ty_bool -> bool_sort
      | Ty_str -> str_sort
      | Ty_bitv 8 -> bv8_sort
      | Ty_bitv 32 -> bv32_sort
      | Ty_bitv 64 -> bv64_sort
      | Ty_bitv n -> Z3.BitVector.mk_sort ctx n
      | Ty_fp 32 -> fp32_sort
      | Ty_fp 64 -> fp64_sort
      | Ty_fp _ | Ty_list | Ty_app | Ty_unit -> assert false

    module Arithmetic = struct
      open Ty
      open Z3

      let int i = Arithmetic.Integer.mk_numeral_i ctx i

      let float f = Arithmetic.Real.mk_numeral_s ctx (Float.to_string f)

      let encode_unop op e =
        match op with
        | Neg -> Arithmetic.mk_unary_minus ctx e
        | Abs ->
          Boolean.mk_ite ctx
            (Arithmetic.mk_gt ctx e (float 0.))
            e
            (Arithmetic.mk_unary_minus ctx e)
        | Sqrt -> Arithmetic.mk_power ctx e (float 0.5)
        | Ceil ->
          let x_int = Arithmetic.Real.mk_real2int ctx e in
          Boolean.mk_ite ctx
            (Boolean.mk_eq ctx (Arithmetic.Integer.mk_int2real ctx x_int) e)
            x_int
            Arithmetic.(mk_add ctx [ x_int; Integer.mk_numeral_i ctx 1 ])
        | Floor -> Arithmetic.Real.mk_real2int ctx e
        | Trunc -> Arithmetic.Real.mk_real2int ctx e
        | Nearest | Is_nan | _ ->
          Fmt.failwith {|Arith: Unsupported Z3 unop operator "%a"|} Ty.pp_unop
            op

      let encode_binop op e1 e2 =
        match op with
        | Add -> Arithmetic.mk_add ctx [ e1; e2 ]
        | Sub -> Arithmetic.mk_sub ctx [ e1; e2 ]
        | Mul -> Arithmetic.mk_mul ctx [ e1; e2 ]
        | Div -> Arithmetic.mk_div ctx e1 e2
        | Rem -> Arithmetic.Integer.mk_rem ctx e1 e2
        | Pow -> Arithmetic.mk_power ctx e1 e2
        | Min -> Boolean.mk_ite ctx (Arithmetic.mk_le ctx e1 e2) e1 e2
        | Max -> Boolean.mk_ite ctx (Arithmetic.mk_ge ctx e1 e2) e1 e2
        | _ ->
          Fmt.failwith {|Real: Unsupported Z3 binop operator "%a"|} Ty.pp_binop
            op

      let encode_triop op _ =
        Fmt.failwith {|Arith: Unsupported Z3 triop operator "%a"|} Ty.pp_triop
          op

      let encode_relop op e1 e2 =
        match op with
        | Lt -> Arithmetic.mk_lt ctx e1 e2
        | Gt -> Arithmetic.mk_gt ctx e1 e2
        | Le -> Arithmetic.mk_le ctx e1 e2
        | Ge -> Arithmetic.mk_ge ctx e1 e2
        | op ->
          Fmt.failwith {|Arith: Unsupported Z3 relop operator "%a"|} Ty.pp_relop
            op

      module Integer = struct
        let v i = int i

        let int2str =
          FuncDecl.mk_func_decl_s ctx "IntToString" [ int_sort ] str_sort

        let str2int =
          FuncDecl.mk_func_decl_s ctx "StringToInt" [ str_sort ] int_sort

        let encode_cvtop op e =
          match op with
          | ToString -> FuncDecl.apply int2str [ e ]
          | OfString -> FuncDecl.apply str2int [ e ]
          | Reinterpret_float -> Arithmetic.Real.mk_real2int ctx e
          | op ->
            Fmt.failwith {|Int: Unsupported Z3 cvtop operator "%a"|} Ty.pp_cvtop
              op
      end

      module Real = struct
        let v f = float f

        let real2str =
          FuncDecl.mk_func_decl_s ctx "RealToString" [ real_sort ] str_sort

        let str2real =
          FuncDecl.mk_func_decl_s ctx "StringToReal" [ str_sort ] real_sort

        let to_uint32 =
          FuncDecl.mk_func_decl_s ctx "ToUInt32" [ real_sort ] real_sort

        let encode_cvtop op e =
          match op with
          | ToString -> FuncDecl.apply real2str [ e ]
          | OfString -> FuncDecl.apply str2real [ e ]
          | ConvertUI32 -> FuncDecl.apply to_uint32 [ e ]
          | Reinterpret_int -> Arithmetic.Integer.mk_int2real ctx e
          | _ ->
            Fmt.failwith {|Real: Unsupported Z3 cvtop operator "%a"|}
              Ty.pp_cvtop op
      end
    end

    module Boolean = struct
      open Z3
      open Ty

      let true_ = Boolean.mk_true ctx

      let false_ = Boolean.mk_false ctx

      let encode_unop = function
        | Not -> Boolean.mk_not ctx
        | op ->
          Fmt.failwith {|Bool: Unsupported Z3 unop operator "%a"|} Ty.pp_unop op

      let encode_binop op e1 e2 =
        match op with
        | And -> Boolean.mk_and ctx [ e1; e2 ]
        | Or -> Boolean.mk_or ctx [ e1; e2 ]
        | Xor -> Boolean.mk_xor ctx e1 e2
        | _ ->
          Fmt.failwith {|Bool: Unsupported Z3 binop operator "%a"|} Ty.pp_binop
            op

      let encode_triop = function
        | Ite -> Boolean.mk_ite ctx
        | op ->
          Fmt.failwith {|Bool: Unsupported Z3 triop operator "%a"|} Ty.pp_triop
            op

      let encode_relop op e1 e2 =
        match op with
        | Eq -> Boolean.mk_eq ctx e1 e2
        | Ne -> Boolean.mk_distinct ctx [ e1; e2 ]
        | _ ->
          Fmt.failwith {|Bool: Unsupported Z3 relop operator "%a"|} Ty.pp_relop
            op

      let encode_cvtop _op _e = assert false

      let encode_naryop op es =
        match op with
        | Logand -> Boolean.mk_and ctx es
        | Logor -> Boolean.mk_or ctx es
        | _ ->
          Fmt.failwith {|Bool: Unsupported Z3 naryop operator "%a"|}
            Ty.pp_naryop op
    end

    module Str = struct
      open Ty
      module Seq = Z3.Seq
      module FuncDecl = Z3.FuncDecl

      let v s = Seq.mk_string ctx s

      let trim = FuncDecl.mk_func_decl_s ctx "Trim" [ str_sort ] str_sort

      let encode_unop op e =
        match op with
        | Length -> Seq.mk_seq_length ctx e
        | Trim -> FuncDecl.apply trim [ e ]
        | _ ->
          Fmt.failwith {|Str: Unsupported Z3 unop operator "%a"|} Ty.pp_unop op

      let encode_binop op e1 e2 =
        match op with
        | At -> Seq.mk_seq_at ctx e1 e2
        | String_prefix -> Seq.mk_seq_prefix ctx e1 e2
        | String_suffix -> Seq.mk_seq_suffix ctx e1 e2
        | String_contains -> Seq.mk_seq_contains ctx e1 e2
        | String_last_index -> Seq.mk_seq_last_index ctx e1 e2
        | _ ->
          Fmt.failwith {|Str: Unsupported Z3 binop operator "%a"|} Ty.pp_binop
            op

      let encode_triop = function
        | String_extract -> Seq.mk_seq_extract ctx
        | String_replace -> Seq.mk_seq_replace ctx
        | String_index -> Seq.mk_seq_index ctx
        | op ->
          Fmt.failwith {|Str: Unsupported Z3 triop operator "%a"|} Ty.pp_triop
            op

      let encode_relop _ = assert false

      let encode_cvtop = function
        | String_to_code -> Seq.mk_string_to_code ctx
        | String_from_code -> Seq.mk_string_from_code ctx
        | String_to_int -> Seq.mk_str_to_int ctx
        | String_from_int -> Seq.mk_int_to_str ctx
        | op ->
          Fmt.failwith {|Str: Unsupported Z3 cvtop operator "%a"|} Ty.pp_cvtop
            op

      let encode_naryop op es =
        match op with
        | Concat -> Seq.mk_seq_concat ctx es
        | _ ->
          Fmt.failwith {|Str: Unsupported Z3 naryop operator "%a"|} Ty.pp_naryop
            op
    end

    module type Bv_sig = sig
      type t

      val v : t -> Z3.Expr.expr

      val bitwidth : int

      module Ixx : sig
        val of_int : int -> t

        val shift_left : t -> int -> t
      end
    end

    module Make_bv (Bv : Bv_sig) = struct
      include Bv
      open Ty
      open Z3

      (* Stolen from @krtab in OCamlPro/owi #195 *)
      let clz n =
        let rec loop (lb : int) (ub : int) =
          if ub = lb + 1 then v @@ Ixx.of_int (bitwidth - ub)
          else
            let mid = (lb + ub) / 2 in
            let pow_two_mid = Ixx.(shift_left (of_int 1) mid) in
            let pow_two_mid = v pow_two_mid in
            Boolean.mk_ite ctx
              (BitVector.mk_ult ctx n pow_two_mid)
              (loop lb mid) (loop mid ub)
        in
        Boolean.mk_ite ctx
          (Boolean.mk_eq ctx n (v (Ixx.of_int 0)))
          (v (Ixx.of_int bitwidth))
          (loop 0 bitwidth)

      (* Stolen from @krtab in OCamlPro/owi #195 *)
      let ctz n =
        let rec loop (lb : int) (ub : int) =
          if ub = lb + 1 then v (Ixx.of_int lb)
          else
            let mid = (lb + ub) / 2 in
            let pow_two_mid = Ixx.(shift_left (of_int 1) mid) in
            let pow_two_mid = v pow_two_mid in
            let is_div_pow_two = BitVector.mk_srem ctx n pow_two_mid in
            Boolean.mk_ite ctx
              (Boolean.mk_eq ctx is_div_pow_two (v (Ixx.of_int 0)))
              (loop mid ub) (loop lb mid)
        in
        Boolean.mk_ite ctx
          (Boolean.mk_eq ctx n (v (Ixx.of_int 0)))
          (v (Ixx.of_int bitwidth))
          (loop 0 bitwidth)

      let encode_unop = function
        | Not -> BitVector.mk_not ctx
        | Neg -> BitVector.mk_neg ctx
        | Clz -> clz
        | Ctz -> ctz
        | op ->
          Fmt.failwith {|Bv: Unsupported Z3 unary operator "%a"|} Ty.pp_unop op

      let encode_binop = function
        | Add -> BitVector.mk_add ctx
        | Sub -> BitVector.mk_sub ctx
        | Mul -> BitVector.mk_mul ctx
        | Div -> BitVector.mk_sdiv ctx
        | DivU -> BitVector.mk_udiv ctx
        | And -> BitVector.mk_and ctx
        | Xor -> BitVector.mk_xor ctx
        | Or -> BitVector.mk_or ctx
        | Shl -> BitVector.mk_shl ctx
        | ShrA -> BitVector.mk_ashr ctx
        | ShrL -> BitVector.mk_lshr ctx
        | Rem -> BitVector.mk_srem ctx
        | RemU -> BitVector.mk_urem ctx
        | Rotl -> BitVector.mk_ext_rotate_left ctx
        | Rotr -> BitVector.mk_ext_rotate_right ctx
        | op ->
          Fmt.failwith {|Bv: Unsupported Z3 binary operator "%a"|} Ty.pp_binop
            op

      let encode_triop op _ =
        Fmt.failwith {|Bv: Unsupported Z3 triop operator "%a"|} Ty.pp_triop op

      let encode_relop op e1 e2 =
        match op with
        | Lt -> BitVector.mk_slt ctx e1 e2
        | LtU -> BitVector.mk_ult ctx e1 e2
        | Le -> BitVector.mk_sle ctx e1 e2
        | LeU -> BitVector.mk_ule ctx e1 e2
        | Gt -> BitVector.mk_sgt ctx e1 e2
        | GtU -> BitVector.mk_ugt ctx e1 e2
        | Ge -> BitVector.mk_sge ctx e1 e2
        | GeU -> BitVector.mk_uge ctx e1 e2
        | Eq | Ne -> assert false

      let encode_cvtop op e =
        match op with
        | WrapI64 -> BitVector.mk_extract ctx (bitwidth - 1) 0 e
        | Sign_extend n -> BitVector.mk_sign_ext ctx n e
        | Zero_extend n -> BitVector.mk_zero_ext ctx n e
        | TruncSF32 | TruncSF64 -> FloatingPoint.mk_to_sbv ctx rtz e bitwidth
        | TruncUF32 | TruncUF64 -> FloatingPoint.mk_to_ubv ctx rtz e bitwidth
        | Reinterpret_float -> FloatingPoint.mk_to_ieee_bv ctx e
        | ToBool -> Boolean.mk_eq ctx e (v (Ixx.of_int 0)) |> Boolean.mk_not ctx
        | OfBool -> Boolean.mk_ite ctx e (v (Ixx.of_int 1)) (v (Ixx.of_int 0))
        | _ -> assert false
    end

    module I8 = Make_bv (struct
      type t = int

      let v i = Z3.BitVector.mk_numeral ctx (string_of_int i) 8

      let bitwidth = 8

      module Ixx = struct
        let of_int i = i [@@inline]

        let shift_left v i = v lsl i [@@inline]
      end
    end)

    module I32 = Make_bv (struct
      type t = int32

      let v i = Z3.BitVector.mk_numeral ctx (Int32.to_string i) 32

      let bitwidth = 32

      module Ixx = Int32
    end)

    module I64 = Make_bv (struct
      type t = int64

      let v i = Z3.BitVector.mk_numeral ctx (Int64.to_string i) 64

      let bitwidth = 64

      module Ixx = Int64
    end)

    module type Fp_sig = sig
      type t

      val v : t -> Z3.Expr.expr

      val sort : Z3.Sort.sort

      val to_string : Z3.FuncDecl.func_decl

      val of_string : Z3.FuncDecl.func_decl
    end

    module Make_fp (Fp : Fp_sig) = struct
      include Fp
      open Z3
      open Ty

      let encode_unop = function
        | Neg -> FloatingPoint.mk_neg ctx
        | Abs -> FloatingPoint.mk_abs ctx
        | Sqrt -> FloatingPoint.mk_sqrt ctx rne
        | Is_nan -> FloatingPoint.mk_is_nan ctx
        | Ceil -> FloatingPoint.mk_round_to_integral ctx rtp
        | Floor -> FloatingPoint.mk_round_to_integral ctx rtn
        | Trunc -> FloatingPoint.mk_round_to_integral ctx rtz
        | Nearest -> FloatingPoint.mk_round_to_integral ctx rne
        | op ->
          Fmt.failwith {|Fp: Unsupported Z3 unary operator "%a"|} Ty.pp_unop op

      let encode_binop = function
        | Add -> FloatingPoint.mk_add ctx rne
        | Sub -> FloatingPoint.mk_sub ctx rne
        | Mul -> FloatingPoint.mk_mul ctx rne
        | Div -> FloatingPoint.mk_div ctx rne
        | Min -> FloatingPoint.mk_min ctx
        | Max -> FloatingPoint.mk_max ctx
        | Rem -> FloatingPoint.mk_rem ctx
        | op ->
          Fmt.failwith {|Fp: Unsupported Z3 binop operator "%a"|} Ty.pp_binop op

      let encode_triop op _ =
        Fmt.failwith {|Fp: Unsupported Z3 triop operator "%a"|} Ty.pp_triop op

      let encode_relop op e1 e2 =
        match op with
        | Eq -> FloatingPoint.mk_eq ctx e1 e2
        | Ne -> FloatingPoint.mk_eq ctx e1 e2 |> Boolean.mk_not ctx
        | Lt -> FloatingPoint.mk_lt ctx e1 e2
        | Le -> FloatingPoint.mk_leq ctx e1 e2
        | Gt -> FloatingPoint.mk_gt ctx e1 e2
        | Ge -> FloatingPoint.mk_geq ctx e1 e2
        | _ ->
          Fmt.failwith {|Fp: Unsupported Z3 relop operator "%a"|} Ty.pp_relop op

      let encode_cvtop op e =
        match op with
        | PromoteF32 | DemoteF64 -> FloatingPoint.mk_to_fp_float ctx rne e sort
        | ConvertSI32 | ConvertSI64 ->
          FloatingPoint.mk_to_fp_signed ctx rne e sort
        | ConvertUI32 | ConvertUI64 ->
          FloatingPoint.mk_to_fp_unsigned ctx rne e sort
        | Reinterpret_int -> FloatingPoint.mk_to_fp_bv ctx e sort
        | ToString -> FuncDecl.apply to_string [ e ]
        | OfString -> FuncDecl.apply of_string [ e ]
        | _ ->
          Fmt.failwith {|Fp: Unsupported Z3 cvtop operator "%a"|} Ty.pp_cvtop op
    end

    module F32 = Make_fp (struct
      type t = int32

      let v f =
        Z3.FloatingPoint.mk_numeral_f ctx (Int32.float_of_bits f) fp32_sort

      let sort = fp32_sort

      let to_string =
        Z3.FuncDecl.mk_func_decl_s ctx "F32ToString" [ fp32_sort ] str_sort

      let of_string =
        Z3.FuncDecl.mk_func_decl_s ctx "StringToF32" [ str_sort ] fp32_sort
    end)

    module F64 = Make_fp (struct
      type t = int64

      let v f =
        Z3.FloatingPoint.mk_numeral_f ctx (Int64.float_of_bits f) fp64_sort

      let sort = fp64_sort

      let to_string =
        Z3.FuncDecl.mk_func_decl_s ctx "F64ToString" [ fp64_sort ] str_sort

      let of_string =
        Z3.FuncDecl.mk_func_decl_s ctx "StringToF64" [ str_sort ] fp64_sort
    end)

    let encode_val : Value.t -> Z3.Expr.expr = function
      | True -> Boolean.true_
      | False -> Boolean.false_
      | Int v -> Arithmetic.Integer.v v
      | Real v -> Arithmetic.Real.v v
      | Str v -> Str.v v
      | Num (I8 x) -> I8.v x
      | Num (I32 x) -> I32.v x
      | Num (I64 x) -> I64.v x
      | Num (F32 x) -> F32.v x
      | Num (F64 x) -> F64.v x
      | List _ | App _ | Unit -> assert false

    let encode_unop = function
      | Ty.Ty_int | Ty.Ty_real -> Arithmetic.encode_unop
      | Ty.Ty_bool -> Boolean.encode_unop
      | Ty.Ty_str -> Str.encode_unop
      | Ty.Ty_bitv 8 -> I8.encode_unop
      | Ty.Ty_bitv 32 -> I32.encode_unop
      | Ty.Ty_bitv 64 -> I64.encode_unop
      | Ty.Ty_fp 32 -> F32.encode_unop
      | Ty.Ty_fp 64 -> F64.encode_unop
      | Ty.Ty_bitv _ | Ty_fp _ | Ty_list | Ty_app | Ty_unit -> assert false

    let encode_binop = function
      | Ty.Ty_int | Ty.Ty_real -> Arithmetic.encode_binop
      | Ty.Ty_bool -> Boolean.encode_binop
      | Ty.Ty_str -> Str.encode_binop
      | Ty.Ty_bitv 8 -> I8.encode_binop
      | Ty.Ty_bitv 32 -> I32.encode_binop
      | Ty.Ty_bitv 64 -> I64.encode_binop
      | Ty.Ty_fp 32 -> F32.encode_binop
      | Ty.Ty_fp 64 -> F64.encode_binop
      | Ty.Ty_bitv _ | Ty_fp _ | Ty_list | Ty_app | Ty_unit -> assert false

    let encode_triop = function
      | Ty.Ty_int | Ty_real -> Arithmetic.encode_triop
      | Ty.Ty_bool -> Boolean.encode_triop
      | Ty.Ty_str -> Str.encode_triop
      | Ty.Ty_bitv 8 -> I8.encode_triop
      | Ty.Ty_bitv 32 -> I32.encode_triop
      | Ty.Ty_bitv 64 -> I64.encode_triop
      | Ty.Ty_fp 32 -> F32.encode_triop
      | Ty.Ty_fp 64 -> F64.encode_triop
      | Ty.Ty_bitv _ | Ty_fp _ | Ty_list | Ty_app | Ty_unit -> assert false

    let encode_relop = function
      | Ty.Ty_int | Ty.Ty_real -> Arithmetic.encode_relop
      | Ty.Ty_bool -> Boolean.encode_relop
      | Ty.Ty_str -> Str.encode_relop
      | Ty.Ty_bitv 8 -> I8.encode_relop
      | Ty.Ty_bitv 32 -> I32.encode_relop
      | Ty.Ty_bitv 64 -> I64.encode_relop
      | Ty.Ty_fp 32 -> F32.encode_relop
      | Ty.Ty_fp 64 -> F64.encode_relop
      | Ty.Ty_bitv _ | Ty_fp _ | Ty_list | Ty_app | Ty_unit -> assert false

    let encode_cvtop = function
      | Ty.Ty_int -> Arithmetic.Integer.encode_cvtop
      | Ty.Ty_real -> Arithmetic.Real.encode_cvtop
      | Ty.Ty_bool -> Boolean.encode_cvtop
      | Ty.Ty_str -> Str.encode_cvtop
      | Ty.Ty_bitv 8 -> I8.encode_cvtop
      | Ty.Ty_bitv 32 -> I32.encode_cvtop
      | Ty.Ty_bitv 64 -> I64.encode_cvtop
      | Ty.Ty_fp 32 -> F32.encode_cvtop
      | Ty.Ty_fp 64 -> F64.encode_cvtop
      | Ty.Ty_bitv _ | Ty_fp _ | Ty_list | Ty_app | Ty_unit -> assert false

    let encode_naryop = function
      | Ty.Ty_bool -> Boolean.encode_naryop
      | Ty.Ty_str -> Str.encode_naryop
      | _ -> assert false

    (* let encode_quantifier (t : bool) (vars_list : Symbol.t list) *)
    (*   (body : Z3.Expr.expr) (patterns : Z3.Quantifier.Pattern.pattern list) : *)
    (*   Z3.Expr.expr = *)
    (*   if List.length vars_list > 0 then *)
    (*     let quantified_assertion = *)
    (*       Z3.Quantifier.mk_quantifier_const ctx t *)
    (*         (List.map *)
    (*            (fun s -> *)
    (*              Z3.Expr.mk_const_s ctx (Symbol.to_string s) *)
    (*                (get_sort (Symbol.type_of s)) ) *)
    (*            vars_list ) *)
    (*         body None patterns [] None None *)
    (*     in *)
    (*     let quantified_assertion = *)
    (*       Z3.Quantifier.expr_of_quantifier quantified_assertion *)
    (*     in *)
    (*     let quantified_assertion = Z3.Expr.simplify quantified_assertion None in *)
    (*     quantified_assertion *)
    (*   else body *)

    let rec encode_expr (hte : Expr.t) : expr =
      let open Expr in
      match view hte with
      | Val v -> encode_val v
      | Ptr { base; offset } ->
        let base' = encode_val (Num (I32 base)) in
        let offset' = encode_expr offset in
        I32.encode_binop Add base' offset'
      | Unop (ty, op, e) ->
        let e' = encode_expr e in
        encode_unop ty op e'
      | Binop (ty, op, e1, e2) ->
        let e1' = encode_expr e1 in
        let e2' = encode_expr e2 in
        encode_binop ty op e1' e2'
      | Triop (ty, op, e1, e2, e3) ->
        let e1' = encode_expr e1
        and e2' = encode_expr e2
        and e3' = encode_expr e3 in
        encode_triop ty op e1' e2' e3'
      | Relop (ty, op, e1, e2) ->
        let e1' = encode_expr e1
        and e2' = encode_expr e2 in
        encode_relop ty op e1' e2'
      | Cvtop (ty, op, e) ->
        let e' = encode_expr e in
        encode_cvtop ty op e'
      | Naryop (ty, op, es) ->
        let es' = List.map encode_expr es in
        encode_naryop ty op es'
      | Symbol { name; ty } -> Z3.Expr.mk_const_s ctx name (get_sort ty)
      | Extract (e, h, l) ->
        let e' = encode_expr e in
        Z3.BitVector.mk_extract ctx ((h * 8) - 1) (l * 8) e'
      | Concat (e1, e2) ->
        let e1' = encode_expr e1
        and e2' = encode_expr e2 in
        Z3.BitVector.mk_concat ctx e1' e2'
      | List _ | App _ -> assert false
    (* | Quantifier (t, vars, body, patterns) -> *)
    (*   let body' = encode_expr body in *)
    (*   let encode_pattern p = *)
    (*     Z3.Quantifier.mk_pattern ctx (List.map encode_expr p) *)
    (*   in *)
    (*   let patterns' = List.map encode_pattern patterns in *)
    (*   let t' = match t with Forall -> true | Exists -> false in *)
    (*   encode_quantifier t' vars body' patterns' *)

    let set_params (params : Params.t) =
      let module P = Z3.Params in
      Z3.set_global_param "smt.ematching"
        (string_of_bool @@ Params.get params Ematching);
      P.update_param_value ctx "timeout"
        (string_of_int @@ Params.get params Timeout);
      P.update_param_value ctx "model"
        (string_of_bool @@ Params.get params Model);
      P.update_param_value ctx "unsat_core"
        (string_of_bool @@ Params.get params Unsat_core)

    let pp_smt ?status fmt (es : Expr.t list) =
      let st = match status with Some b -> string_of_bool b | None -> "" in
      let es' = List.map encode_expr es in
      Z3.Params.set_print_mode ctx Z3enums.PRINT_SMTLIB2_COMPLIANT;
      Fmt.pf fmt "%s"
        (Z3.SMT.benchmark_to_smtstring ctx "" "" st "" (List.tl es')
           (List.hd es') )

    let get_statistics stats =
      let statistics = Z3.Statistics.get_entries stats in
      let add_entry map entry =
        let key = Z3.Statistics.Entry.get_key entry in
        let value =
          if Z3.Statistics.Entry.is_int entry then
            `Int (Z3.Statistics.Entry.get_int entry)
          else begin
            assert (Z3.Statistics.Entry.is_float entry);
            `Float (Z3.Statistics.Entry.get_float entry)
          end
        in
        Statistics.Map.add key value map
      in
      List.fold_left add_entry Statistics.Map.empty statistics

    module Solver = struct
      let make ?params ?logic () : solver =
        Option.iter set_params params;
        let logic =
          Option.map
            (fun l -> Fmt.kstr (Z3.Symbol.mk_string ctx) "%a" Ty.pp_logic l)
            logic
        in
        Z3.Solver.mk_solver ctx logic

      let add_simplifier solver =
        let simplify = Z3.Simplifier.mk_simplifier ctx "simplify" in
        let solve_eqs = Z3.Simplifier.mk_simplifier ctx "solve-eqs" in
        let then_ =
          List.map
            (Z3.Simplifier.mk_simplifier ctx)
            [ "elim-unconstrained"; "propagate-values"; "simplify" ]
        in
        let simplifier = Z3.Simplifier.and_then ctx simplify solve_eqs then_ in
        Z3.Solver.add_simplifier ctx solver simplifier

      let clone s = Z3.Solver.translate s ctx

      let push s = Z3.Solver.push s

      let pop s lvl = Z3.Solver.pop s lvl

      let reset s = Z3.Solver.reset s [@@inline]

      let add s es = Z3.Solver.add s (List.map encode_expr es)

      let check s ~assumptions =
        match Z3.Solver.check s (List.map encode_expr assumptions) with
        | Z3.Solver.UNKNOWN -> `Unknown
        | Z3.Solver.SATISFIABLE -> `Sat
        | Z3.Solver.UNSATISFIABLE -> `Unsat

      let model s = Z3.Solver.get_model s

      let interrupt _ = Z3.Tactic.interrupt ctx

      let get_statistics solver =
        get_statistics (Z3.Solver.get_statistics solver)
    end

    module Optimizer = struct
      let make () = Z3.Optimize.mk_opt ctx

      let push o = Z3.Optimize.push o

      let pop o = Z3.Optimize.pop o

      let add o es = Z3.Optimize.add o (List.map encode_expr es)

      let check o =
        match Z3.Optimize.check o with
        | Z3.Solver.UNKNOWN -> `Unknown
        | Z3.Solver.SATISFIABLE -> `Sat
        | Z3.Solver.UNSATISFIABLE -> `Unsat

      let model o = Z3.Optimize.get_model o

      let maximize o e = Z3.Optimize.maximize o (encode_expr e)

      let minimize o e = Z3.Optimize.minimize o (encode_expr e)

      let interrupt _ = Z3.Tactic.interrupt ctx

      let get_statistics solver =
        get_statistics (Z3.Optimize.get_statistics solver)
    end

    let set (s : string) (i : int) (n : char) =
      let bs = Bytes.of_string s in
      Bytes.set bs i n;
      Bytes.to_string bs

    let int64_of_bv (bv : Z3.Expr.expr) : int64 =
      assert (Z3.Expr.is_numeral bv);
      Int64.of_string (set (Z3.Expr.to_string bv) 0 '0')

    let float_of_numeral (fp : Z3.Expr.expr) : float =
      assert (Z3.Expr.is_numeral fp);
      let module Fp = Z3.FloatingPoint in
      if Fp.is_numeral_nan ctx fp then Float.nan
      else if Fp.is_numeral_inf ctx fp then
        if Fp.is_numeral_negative ctx fp then Float.neg_infinity
        else Float.infinity
      else if Fp.is_numeral_zero ctx fp then
        if Fp.is_numeral_negative ctx fp then Float.neg Float.zero
        else Float.zero
      else
        let sort = Z3.Expr.get_sort fp in
        let ebits = Fp.get_ebits ctx sort in
        let sbits = Fp.get_sbits ctx sort in
        let _, sign = Fp.get_numeral_sign ctx fp in
        (* true => biased exponent *)
        let _, exponent = Fp.get_numeral_exponent_int ctx fp true in
        let _, significand = Fp.get_numeral_significand_uint ctx fp in
        let fp_bits =
          Int64.(
            logor
              (logor
                 (shift_left (of_int sign) (ebits + sbits - 1))
                 (shift_left exponent (sbits - 1)) )
              significand )
        in
        match ebits + sbits with
        | 32 -> Int32.float_of_bits @@ Int64.to_int32 fp_bits
        | 64 -> Int64.float_of_bits fp_bits
        | _ -> assert false

    let value (model : Z3.Model.model) (c : Expr.t) : Value.t =
      let open Value in
      (* we have a model with completion => should never be None *)
      let e = Z3.Model.eval model (encode_expr c) true |> Option.get in
      match (Expr.ty c, Z3.Sort.get_sort_kind @@ Z3.Expr.get_sort e) with
      | Ty_int, Z3enums.INT_SORT ->
        Int (Z.to_int @@ Z3.Arithmetic.Integer.get_big_int e)
      | Ty_real, Z3enums.REAL_SORT ->
        Real (Q.to_float @@ Z3.Arithmetic.Real.get_ratio e)
      | Ty_bool, Z3enums.BOOL_SORT -> (
        match Z3.Boolean.get_bool_value e with
        | Z3enums.L_TRUE -> True
        | Z3enums.L_FALSE -> False
        | Z3enums.L_UNDEF ->
          (* It can never be something else *)
          assert false )
      | Ty_str, Z3enums.SEQ_SORT -> Str (Z3.Seq.get_string ctx e)
      | Ty_bitv 1, Z3enums.BV_SORT ->
        (* Hack: properly represent values with bitwidth = 1 *)
        let bit = int64_of_bv e in
        if Int64.equal bit 1L then True
        else (
          assert (Int64.equal bit 0L);
          False )
      | Ty_bitv 8, Z3enums.BV_SORT -> Num (I8 (Int64.to_int (int64_of_bv e)))
      | Ty_bitv 32, Z3enums.BV_SORT -> Num (I32 (Int64.to_int32 (int64_of_bv e)))
      | Ty_bitv 64, Z3enums.BV_SORT -> Num (I64 (int64_of_bv e))
      | Ty_fp 32, Z3enums.FLOATING_POINT_SORT ->
        Num (F32 (Int32.bits_of_float @@ float_of_numeral e))
      | Ty_fp 64, Z3enums.FLOATING_POINT_SORT ->
        Num (F64 (Int64.bits_of_float @@ float_of_numeral e))
      | _ -> assert false

    let type_of_sort (sort : Z3.Sort.sort) : Ty.t =
      match Z3.Sort.get_sort_kind sort with
      | Z3enums.INT_SORT -> Ty.Ty_int
      | Z3enums.REAL_SORT -> Ty.Ty_real
      | Z3enums.BOOL_SORT -> Ty.Ty_bool
      | Z3enums.SEQ_SORT -> Ty.Ty_str
      | Z3enums.BV_SORT -> Ty.Ty_bitv (Z3.BitVector.get_size sort)
      | Z3enums.FLOATING_POINT_SORT ->
        let ebits = Z3.FloatingPoint.get_ebits ctx sort in
        let sbits = Z3.FloatingPoint.get_sbits ctx sort in
        Ty_fp (ebits + sbits)
      | _ -> assert false

    let symbols_of_model (model : Z3.Model.model) : Symbol.t list =
      List.map
        (fun const ->
          let x = Z3.Symbol.to_string (Z3.FuncDecl.get_name const) in
          let t = type_of_sort (Z3.FuncDecl.get_range const) in
          Symbol.make t x )
        (Z3.Model.get_const_decls model)

    let values_of_model ?(symbols : Symbol.t list option)
      (model : Z3.Model.model) : Model.t =
      let m = Hashtbl.create 512 in
      let symbols' = Option.value symbols ~default:(symbols_of_model model) in
      List.iter
        (fun sym ->
          let v = value model (Expr.mk_symbol sym) in
          Hashtbl.replace m sym v )
        symbols';
      m

    let set_debug _ = ()
  end
end

let is_available = true

include Fresh.Make ()

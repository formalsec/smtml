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
module P = Params
module Sym = Symbol

let err = Log.err

module M = struct
  module Make () = struct
    open Z3

    type ty = Sort.sort

    type term = Expr.expr

    type interp = term

    type model = Model.model

    type solver = Solver.solver

    type handle = Optimize.handle

    type optimizer = Optimize.optimize

    let ctx = mk_context []

    let true_ = Boolean.mk_true ctx

    let false_ = Boolean.mk_false ctx

    let int i = Arithmetic.Integer.mk_numeral_i ctx i

    let real f = Arithmetic.Real.mk_numeral_s ctx (Float.to_string f)

    let const sym ty = Expr.mk_const_s ctx sym ty

    let not_ e = Boolean.mk_not ctx e

    let and_ e1 e2 = Boolean.mk_and ctx [ e1; e2 ]

    let or_ e1 e2 = Boolean.mk_or ctx [ e1; e2 ]

    let xor e1 e2 = Boolean.mk_xor ctx e1 e2

    let eq e1 e2 = Boolean.mk_eq ctx e1 e2

    let distinct es = Boolean.mk_distinct ctx es

    let ite cond e1 e2 = Boolean.mk_ite ctx cond e1 e2

    module Types = struct
      let int = Arithmetic.Integer.mk_sort ctx

      let real = Arithmetic.Real.mk_sort ctx

      let bool = Boolean.mk_sort ctx

      let string = Seq.mk_string_sort ctx

      let bitv n = BitVector.mk_sort ctx n

      let float eb sb = FloatingPoint.mk_sort ctx eb sb

      let ty term = Expr.get_sort term

      let to_ety sort =
        match Sort.get_sort_kind sort with
        | Z3enums.INT_SORT -> Ty.Ty_int
        | Z3enums.REAL_SORT -> Ty.Ty_real
        | Z3enums.BOOL_SORT -> Ty.Ty_bool
        | Z3enums.SEQ_SORT -> Ty.Ty_str
        | Z3enums.BV_SORT -> Ty.Ty_bitv (Z3.BitVector.get_size sort)
        | Z3enums.FLOATING_POINT_SORT ->
          let ebits = Z3.FloatingPoint.get_ebits ctx sort in
          let sbits = Z3.FloatingPoint.get_sbits ctx sort in
          Ty.Ty_fp (ebits + sbits)
        | _ -> assert false
    end

    module Interp = struct
      let to_int interp = Z.to_int @@ Arithmetic.Integer.get_big_int interp

      let to_real interp = Q.to_float @@ Arithmetic.Real.get_ratio interp

      let to_bool interp =
        match Boolean.get_bool_value interp with
        | Z3enums.L_TRUE -> true
        | Z3enums.L_FALSE -> false
        | Z3enums.L_UNDEF ->
          err "Z3_mappings2: to_bool: something went terribly wrong!"

      let to_string interp = Seq.get_string ctx interp

      let to_bitv interp _n =
        assert (Expr.is_numeral interp);
        let set (s : string) (i : int) (n : char) =
          let bs = Bytes.of_string s in
          Bytes.set bs i n;
          Bytes.to_string bs
        in
        Int64.of_string (set (Expr.to_string interp) 0 '0')

      let to_float fp _eb _sb =
        assert (Expr.is_numeral fp);
        if FloatingPoint.is_numeral_nan ctx fp then Float.nan
        else if FloatingPoint.is_numeral_inf ctx fp then
          if FloatingPoint.is_numeral_negative ctx fp then Float.neg_infinity
          else Float.infinity
        else if FloatingPoint.is_numeral_zero ctx fp then
          if FloatingPoint.is_numeral_negative ctx fp then Float.neg Float.zero
          else Float.zero
        else
          let sort = Expr.get_sort fp in
          let ebits = FloatingPoint.get_ebits ctx sort in
          let sbits = FloatingPoint.get_sbits ctx sort in
          let _, sign = FloatingPoint.get_numeral_sign ctx fp in
          (* true => biased exponent *)
          let _, exponent =
            FloatingPoint.get_numeral_exponent_int ctx fp true
          in
          let _, significand =
            FloatingPoint.get_numeral_significand_uint ctx fp
          in
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
    end

    module Int = struct
      let neg e = Arithmetic.mk_unary_minus ctx e

      let to_real e = Arithmetic.Integer.mk_int2real ctx e

      let add e1 e2 = Arithmetic.mk_add ctx [ e1; e2 ]

      let sub e1 e2 = Arithmetic.mk_sub ctx [ e1; e2 ]

      let mul e1 e2 = Arithmetic.mk_mul ctx [ e1; e2 ]

      let div e1 e2 = Arithmetic.mk_div ctx e1 e2

      let rem e1 e2 = Arithmetic.Integer.mk_rem ctx e1 e2

      let pow e1 e2 = Arithmetic.mk_power ctx e1 e2

      let lt e1 e2 = Arithmetic.mk_lt ctx e1 e2

      let le e1 e2 = Arithmetic.mk_le ctx e1 e2

      let gt e1 e2 = Arithmetic.mk_gt ctx e1 e2

      let ge e1 e2 = Arithmetic.mk_ge ctx e1 e2
    end

    module Real = struct
      let neg e = Arithmetic.mk_unary_minus ctx e

      let to_int e = Arithmetic.Real.mk_real2int ctx e

      let add e1 e2 = Arithmetic.mk_add ctx [ e1; e2 ]

      let sub e1 e2 = Arithmetic.mk_sub ctx [ e1; e2 ]

      let mul e1 e2 = Arithmetic.mk_mul ctx [ e1; e2 ]

      let div e1 e2 = Arithmetic.mk_div ctx e1 e2

      let pow e1 e2 = Arithmetic.mk_power ctx e1 e2

      let lt e1 e2 = Arithmetic.mk_lt ctx e1 e2

      let le e1 e2 = Arithmetic.mk_le ctx e1 e2

      let gt e1 e2 = Arithmetic.mk_gt ctx e1 e2

      let ge e1 e2 = Arithmetic.mk_ge ctx e1 e2
    end

    module String = struct
      let v s = Seq.mk_string ctx s

      let length e = Seq.mk_seq_length ctx e

      let to_code e = Seq.mk_string_to_code ctx e

      let of_code e = Seq.mk_string_from_code ctx e

      let to_int e = Seq.mk_str_to_int ctx e

      let of_int e = Seq.mk_int_to_str ctx e

      let at str ~pos = Seq.mk_seq_at ctx str pos

      let concat e1 e2 = Seq.mk_seq_concat ctx [ e1; e2 ]

      let is_prefix e1 ~prefix = Seq.mk_seq_prefix ctx e1 prefix

      let is_suffix e1 ~suffix = Seq.mk_seq_suffix ctx e1 suffix

      let contains e1 ~sub = Seq.mk_seq_contains ctx e1 sub

      let sub str ~pos ~len = Seq.mk_seq_extract ctx str pos len

      let index_of e1 ~sub ~pos = Seq.mk_seq_index ctx e1 sub pos

      let replace e1 ~pattern ~with_ = Seq.mk_seq_replace ctx e1 pattern with_
    end

    module Bitv = struct
      let v bv bitwidth = BitVector.mk_numeral ctx bv bitwidth

      let neg e = BitVector.mk_neg ctx e

      let lognot e = BitVector.mk_not ctx e

      let add e1 e2 = BitVector.mk_add ctx e1 e2

      let sub e1 e2 = BitVector.mk_sub ctx e1 e2

      let mul e1 e2 = BitVector.mk_mul ctx e1 e2

      let div e1 e2 = BitVector.mk_sdiv ctx e1 e2

      let div_u e1 e2 = BitVector.mk_udiv ctx e1 e2

      let logor e1 e2 = BitVector.mk_or ctx e1 e2

      let logand e1 e2 = BitVector.mk_and ctx e1 e2

      let logxor e1 e2 = BitVector.mk_xor ctx e1 e2

      let shl e1 e2 = BitVector.mk_shl ctx e1 e2

      let ashr e1 e2 = BitVector.mk_ashr ctx e1 e2

      let lshr e1 e2 = BitVector.mk_lshr ctx e1 e2

      let rem e1 e2 = BitVector.mk_srem ctx e1 e2

      let rem_u e1 e2 = BitVector.mk_urem ctx e1 e2

      let rotate_left e1 e2 = BitVector.mk_ext_rotate_left ctx e1 e2

      let rotate_right e1 e2 = BitVector.mk_ext_rotate_right ctx e1 e2

      let lt e1 e2 = BitVector.mk_slt ctx e1 e2

      let lt_u e1 e2 = BitVector.mk_ult ctx e1 e2

      let le e1 e2 = BitVector.mk_sle ctx e1 e2

      let le_u e1 e2 = BitVector.mk_ule ctx e1 e2

      let gt e1 e2 = BitVector.mk_sgt ctx e1 e2

      let gt_u e1 e2 = BitVector.mk_ugt ctx e1 e2

      let ge e1 e2 = BitVector.mk_sge ctx e1 e2

      let ge_u e1 e2 = BitVector.mk_uge ctx e1 e2

      let concat e1 e2 = BitVector.mk_concat ctx e1 e2

      let extract e ~high ~low = BitVector.mk_extract ctx high low e

      let zero_extend n e = BitVector.mk_zero_ext ctx n e

      let sign_extend n e = BitVector.mk_sign_ext ctx n e
    end

    module Float = struct
      module Rounding_mode = struct
        let rne = FloatingPoint.RoundingMode.mk_rne ctx

        let rna = FloatingPoint.RoundingMode.mk_rna ctx

        let rtp = FloatingPoint.RoundingMode.mk_rtp ctx

        let rtn = FloatingPoint.RoundingMode.mk_rtn ctx

        let rtz = FloatingPoint.RoundingMode.mk_rtz ctx
      end

      let v f eb sb = FloatingPoint.mk_numeral_f ctx f (Types.float eb sb)

      let neg e = FloatingPoint.mk_neg ctx e

      let abs e = FloatingPoint.mk_abs ctx e

      let sqrt ~rm e = FloatingPoint.mk_sqrt ctx rm e

      let is_nan e = FloatingPoint.mk_is_nan ctx e

      let round_to_integral ~rm e = FloatingPoint.mk_round_to_integral ctx rm e

      let add ~rm e1 e2 = FloatingPoint.mk_add ctx rm e1 e2

      let sub ~rm e1 e2 = FloatingPoint.mk_sub ctx rm e1 e2

      let mul ~rm e1 e2 = FloatingPoint.mk_mul ctx rm e1 e2

      let div ~rm e1 e2 = FloatingPoint.mk_div ctx rm e1 e2

      let min e1 e2 = FloatingPoint.mk_min ctx e1 e2

      let max e1 e2 = FloatingPoint.mk_max ctx e1 e2

      let rem e1 e2 = FloatingPoint.mk_rem ctx e1 e2

      let eq e1 e2 = FloatingPoint.mk_eq ctx e1 e2

      let lt e1 e2 = FloatingPoint.mk_lt ctx e1 e2

      let le e1 e2 = FloatingPoint.mk_leq ctx e1 e2

      let gt e1 e2 = FloatingPoint.mk_gt ctx e1 e2

      let ge e1 e2 = FloatingPoint.mk_geq ctx e1 e2

      let to_fp eb sb ~rm fp =
        FloatingPoint.mk_to_fp_float ctx rm fp (Types.float eb sb)

      let sbv_to_fp eb sb ~rm bv =
        FloatingPoint.mk_to_fp_signed ctx rm bv (Types.float eb sb)

      let ubv_to_fp eb sb ~rm bv =
        FloatingPoint.mk_to_fp_unsigned ctx rm bv (Types.float eb sb)

      let to_ubv n ~rm fp = FloatingPoint.mk_to_ubv ctx rm fp n

      let to_sbv n ~rm fp = FloatingPoint.mk_to_sbv ctx rm fp n

      let of_ieee_bv eb sb bv =
        FloatingPoint.mk_to_fp_bv ctx bv (Types.float eb sb)

      let to_ieee_bv fp = FloatingPoint.mk_to_ieee_bv ctx fp
    end

    module Model = struct
      let get_symbols model =
        List.map
          (fun const ->
            let x = Symbol.to_string (FuncDecl.get_name const) in
            let t = Types.to_ety (FuncDecl.get_range const) in
            Sym.make t x )
          (Model.get_const_decls model)

      let eval ?(completion = false) model term =
        Model.eval model term completion
    end

    let pp_entry fmt (entry : Statistics.Entry.statistics_entry) =
      let key = Statistics.Entry.get_key entry in
      let value = Statistics.Entry.to_string_value entry in
      Format.fprintf fmt "(%s %s)" key value

    let pp_statistics fmt (stats : Statistics.statistics) =
      let entries = Statistics.get_entries stats in
      Format.pp_print_list
        ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
        pp_entry fmt entries

    let set_params (params : P.t) =
      Z3.set_global_param "smt.ematching"
        (string_of_bool @@ P.get params Ematching);
      Z3.Params.update_param_value ctx "timeout"
        (string_of_int @@ P.get params Timeout);
      Z3.Params.update_param_value ctx "model"
        (string_of_bool @@ P.get params Model);
      Z3.Params.update_param_value ctx "unsat_core"
        (string_of_bool @@ P.get params Unsat_core)

    module Solver = struct
      (* TODO: parameters *)
      let make ?params ?logic () =
        Option.iter set_params params;
        let logic =
          Option.map
            (fun l ->
              Format.kasprintf (Z3.Symbol.mk_string ctx) "%a" Ty.pp_logic l )
            logic
        in
        Z3.Solver.mk_solver ctx logic

      let clone solver = Solver.translate solver ctx

      let push solver = Solver.push solver

      let pop solver n = Solver.pop solver n

      let reset solver = Solver.reset solver

      let add solver terms = Solver.add solver terms

      let check solver ~assumptions =
        match Solver.check solver assumptions with
        | Solver.UNKNOWN -> `Unknown
        | Solver.SATISFIABLE -> `Sat
        | Solver.UNSATISFIABLE -> `Unsat

      let model solver = Solver.get_model solver

      let add_simplifier solver =
        let simplify = Simplifier.mk_simplifier ctx "simplify" in
        let solver_eqs = Simplifier.mk_simplifier ctx "solve-eqs" in
        let then_ =
          List.map
            (Simplifier.mk_simplifier ctx)
            [ "elim-unconstrained"; "propagate-values"; "simplify" ]
        in
        Simplifier.and_then ctx simplify solver_eqs then_
        |> Solver.add_simplifier ctx solver

      let interrupt () = Tactic.interrupt ctx

      let pp_statistics fmt solver =
        pp_statistics fmt @@ Solver.get_statistics solver
    end

    module Optimizer = struct
      let make () = Optimize.mk_opt ctx

      let push opt = Optimize.push opt

      let pop opt = Optimize.pop opt

      let add opt terms = Optimize.add opt terms

      let check opt =
        match Optimize.check opt with
        | Z3.Solver.UNKNOWN -> `Unknown
        | Z3.Solver.SATISFIABLE -> `Sat
        | Z3.Solver.UNSATISFIABLE -> `Unsat

      let model opt = Optimize.get_model opt

      let maximize opt term = Optimize.maximize opt term

      let minimize opt term = Optimize.minimize opt term

      let interrupt () = Tactic.interrupt ctx

      let pp_statistics fmt opt =
        pp_statistics fmt @@ Optimize.get_statistics opt
    end
  end

  include Make ()
end

module M' : Mappings_intf.M_with_make = M

include Mappings.Make (M)

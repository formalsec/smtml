(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2024 formalsec *)
(* Written by the Smtml programmers *)

include Mappings_intf

module M = struct
  module Make () = struct
    module Internals = struct
      let caches_consts = true

      let is_available = true
    end

    type ty = Z3.Sort.sort

    type term = Z3.Expr.expr

    type interp = term

    type model = Z3.Model.model

    type solver = Z3.Solver.solver

    type handle = Z3.Optimize.handle

    type optimizer = Z3.Optimize.optimize

    type func_decl = Z3.FuncDecl.func_decl

    let ctx = Z3.mk_context []

    let true_ = Z3.Boolean.mk_true ctx

    let false_ = Z3.Boolean.mk_false ctx

    let int i = Z3.Arithmetic.Integer.mk_numeral_i ctx i

    let real f = Z3.Arithmetic.Real.mk_numeral_s ctx (Float.to_string f)

    let const sym ty = Z3.Expr.mk_const_s ctx sym ty

    let not_ e = Z3.Boolean.mk_not ctx e

    let and_ e1 e2 = Z3.Boolean.mk_and ctx [ e1; e2 ]

    let or_ e1 e2 = Z3.Boolean.mk_or ctx [ e1; e2 ]

    let logand es = Z3.Boolean.mk_and ctx es

    let logor es = Z3.Boolean.mk_or ctx es

    let xor e1 e2 = Z3.Boolean.mk_xor ctx e1 e2

    let implies e1 e2 = Z3.Boolean.mk_implies ctx e1 e2

    let eq e1 e2 = Z3.Boolean.mk_eq ctx e1 e2

    let distinct es = Z3.Boolean.mk_distinct ctx es

    let ite cond e1 e2 = Z3.Boolean.mk_ite ctx cond e1 e2

    let forall vars body =
      Z3.Quantifier.mk_forall_const ctx vars body None [] [] None None
      |> Z3.Quantifier.expr_of_quantifier

    let exists vars body =
      Z3.Quantifier.mk_exists_const ctx vars body None [] [] None None
      |> Z3.Quantifier.expr_of_quantifier

    module Types = struct
      let int = lazy (Z3.Arithmetic.Integer.mk_sort ctx)

      let real = lazy (Z3.Arithmetic.Real.mk_sort ctx)

      let bool = lazy (Z3.Boolean.mk_sort ctx)

      let string = lazy (Z3.Seq.mk_string_sort ctx)

      let bitv n = Z3.BitVector.mk_sort ctx n

      let float eb sb = Z3.FloatingPoint.mk_sort ctx eb sb

      let roundingMode = lazy (Z3.FloatingPoint.RoundingMode.mk_sort ctx)

      let ty term = Z3.Expr.get_sort term

      let to_ety sort =
        match Z3.Sort.get_sort_kind sort with
        | Z3enums.INT_SORT -> Ty.Ty_int
        | REAL_SORT -> Ty.Ty_real
        | BOOL_SORT -> Ty.Ty_bool
        | SEQ_SORT -> Ty.Ty_str
        | BV_SORT -> Ty.Ty_bitv (Z3.BitVector.get_size sort)
        | FLOATING_POINT_SORT ->
          let ebits = Z3.FloatingPoint.get_ebits ctx sort in
          let sbits = Z3.FloatingPoint.get_sbits ctx sort in
          Ty.Ty_fp (ebits + sbits)
        | _ -> assert false
    end

    module Interp = struct
      let to_int interp = Z.to_int @@ Z3.Arithmetic.Integer.get_big_int interp

      let to_real interp = Q.to_float @@ Z3.Arithmetic.Real.get_ratio interp

      let to_bool interp =
        match Z3.Boolean.get_bool_value interp with
        | Z3enums.L_TRUE -> true
        | L_FALSE -> false
        | L_UNDEF ->
          Fmt.failwith "Z3_mappings2: to_bool: something went terribly wrong!"

      let to_string interp = Z3.Seq.get_string ctx interp

      let to_bitv interp _n =
        assert (Z3.Expr.is_numeral interp);
        let set (s : string) (i : int) (n : char) =
          let bs = Bytes.of_string s in
          Bytes.set bs i n;
          Bytes.to_string bs
        in
        Int64.of_string (set (Z3.Expr.to_string interp) 0 '0')

      let to_float fp _eb _sb =
        assert (Z3.Expr.is_numeral fp);
        if Z3.FloatingPoint.is_numeral_nan ctx fp then Float.nan
        else if Z3.FloatingPoint.is_numeral_inf ctx fp then
          if Z3.FloatingPoint.is_numeral_negative ctx fp then Float.neg_infinity
          else Float.infinity
        else if Z3.FloatingPoint.is_numeral_zero ctx fp then
          if Z3.FloatingPoint.is_numeral_negative ctx fp then
            Float.neg Float.zero
          else Float.zero
        else
          let sort = Z3.Expr.get_sort fp in
          let ebits = Z3.FloatingPoint.get_ebits ctx sort in
          let sbits = Z3.FloatingPoint.get_sbits ctx sort in
          let _, sign = Z3.FloatingPoint.get_numeral_sign ctx fp in
          (* true => biased exponent *)
          let _, exponent =
            Z3.FloatingPoint.get_numeral_exponent_int ctx fp true
          in
          let _, significand =
            Z3.FloatingPoint.get_numeral_significand_uint ctx fp
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
      let neg e = Z3.Arithmetic.mk_unary_minus ctx e

      let to_real e = Z3.Arithmetic.Integer.mk_int2real ctx e

      let add e1 e2 = Z3.Arithmetic.mk_add ctx [ e1; e2 ]

      let sub e1 e2 = Z3.Arithmetic.mk_sub ctx [ e1; e2 ]

      let mul e1 e2 = Z3.Arithmetic.mk_mul ctx [ e1; e2 ]

      let div e1 e2 = Z3.Arithmetic.mk_div ctx e1 e2

      let rem e1 e2 = Z3.Arithmetic.Integer.mk_rem ctx e1 e2

      let pow e1 e2 = Z3.Arithmetic.mk_power ctx e1 e2

      let lt e1 e2 = Z3.Arithmetic.mk_lt ctx e1 e2

      let le e1 e2 = Z3.Arithmetic.mk_le ctx e1 e2

      let gt e1 e2 = Z3.Arithmetic.mk_gt ctx e1 e2

      let ge e1 e2 = Z3.Arithmetic.mk_ge ctx e1 e2
    end

    module Real = struct
      let neg e = Z3.Arithmetic.mk_unary_minus ctx e

      let to_int e = Z3.Arithmetic.Real.mk_real2int ctx e

      let add e1 e2 = Z3.Arithmetic.mk_add ctx [ e1; e2 ]

      let sub e1 e2 = Z3.Arithmetic.mk_sub ctx [ e1; e2 ]

      let mul e1 e2 = Z3.Arithmetic.mk_mul ctx [ e1; e2 ]

      let div e1 e2 = Z3.Arithmetic.mk_div ctx e1 e2

      let pow e1 e2 = Z3.Arithmetic.mk_power ctx e1 e2

      let lt e1 e2 = Z3.Arithmetic.mk_lt ctx e1 e2

      let le e1 e2 = Z3.Arithmetic.mk_le ctx e1 e2

      let gt e1 e2 = Z3.Arithmetic.mk_gt ctx e1 e2

      let ge e1 e2 = Z3.Arithmetic.mk_ge ctx e1 e2
    end

    module String = struct
      let v s = Z3.Seq.mk_string ctx s

      let length e = Z3.Seq.mk_seq_length ctx e

      let to_code e = Z3.Seq.mk_string_to_code ctx e

      let of_code e = Z3.Seq.mk_string_from_code ctx e

      let to_int e = Z3.Seq.mk_str_to_int ctx e

      let of_int e = Z3.Seq.mk_int_to_str ctx e

      let to_re e = Z3.Seq.mk_seq_to_re ctx e

      let in_re e1 e2 = Z3.Seq.mk_seq_in_re ctx e1 e2

      let at str ~pos = Z3.Seq.mk_seq_at ctx str pos

      let concat es = Z3.Seq.mk_seq_concat ctx es

      let is_prefix e1 ~prefix = Z3.Seq.mk_seq_prefix ctx e1 prefix

      let is_suffix e1 ~suffix = Z3.Seq.mk_seq_suffix ctx e1 suffix

      let lt a b = Z3.Seq.mk_str_lt ctx a b

      let le a b = Z3.Seq.mk_str_le ctx a b

      let contains e1 ~sub = Z3.Seq.mk_seq_contains ctx e1 sub

      let sub str ~pos ~len = Z3.Seq.mk_seq_extract ctx str pos len

      let index_of e1 ~sub ~pos = Z3.Seq.mk_seq_index ctx e1 sub pos

      let replace e1 ~pattern ~with_ =
        Z3.Seq.mk_seq_replace ctx e1 pattern with_
    end

    module Re = struct
      let star e = Z3.Seq.mk_re_star ctx e

      let plus e = Z3.Seq.mk_re_plus ctx e

      let opt e = Z3.Seq.mk_re_option ctx e

      let comp e = Z3.Seq.mk_re_complement ctx e

      let range e1 e2 = Z3.Seq.mk_re_range ctx e1 e2

      let loop e i1 i2 = Z3.Seq.mk_re_loop ctx e i1 i2

      let union es = Z3.Seq.mk_re_union ctx es

      let concat es = Z3.Seq.mk_re_concat ctx es
    end

    module Bitv = struct
      let v bv bitwidth = Z3.BitVector.mk_numeral ctx bv bitwidth

      let neg e = Z3.BitVector.mk_neg ctx e

      let lognot e = Z3.BitVector.mk_not ctx e

      let add e1 e2 = Z3.BitVector.mk_add ctx e1 e2

      let sub e1 e2 = Z3.BitVector.mk_sub ctx e1 e2

      let mul e1 e2 = Z3.BitVector.mk_mul ctx e1 e2

      let div e1 e2 = Z3.BitVector.mk_sdiv ctx e1 e2

      let div_u e1 e2 = Z3.BitVector.mk_udiv ctx e1 e2

      let logor e1 e2 = Z3.BitVector.mk_or ctx e1 e2

      let logand e1 e2 = Z3.BitVector.mk_and ctx e1 e2

      let logxor e1 e2 = Z3.BitVector.mk_xor ctx e1 e2

      let shl e1 e2 = Z3.BitVector.mk_shl ctx e1 e2

      let ashr e1 e2 = Z3.BitVector.mk_ashr ctx e1 e2

      let lshr e1 e2 = Z3.BitVector.mk_lshr ctx e1 e2

      let rem e1 e2 = Z3.BitVector.mk_srem ctx e1 e2

      let rem_u e1 e2 = Z3.BitVector.mk_urem ctx e1 e2

      let rotate_left e1 e2 = Z3.BitVector.mk_ext_rotate_left ctx e1 e2

      let rotate_right e1 e2 = Z3.BitVector.mk_ext_rotate_right ctx e1 e2

      let lt e1 e2 = Z3.BitVector.mk_slt ctx e1 e2

      let lt_u e1 e2 = Z3.BitVector.mk_ult ctx e1 e2

      let le e1 e2 = Z3.BitVector.mk_sle ctx e1 e2

      let le_u e1 e2 = Z3.BitVector.mk_ule ctx e1 e2

      let gt e1 e2 = Z3.BitVector.mk_sgt ctx e1 e2

      let gt_u e1 e2 = Z3.BitVector.mk_ugt ctx e1 e2

      let ge e1 e2 = Z3.BitVector.mk_sge ctx e1 e2

      let ge_u e1 e2 = Z3.BitVector.mk_uge ctx e1 e2

      let concat e1 e2 = Z3.BitVector.mk_concat ctx e1 e2

      let extract e ~high ~low = Z3.BitVector.mk_extract ctx high low e

      let zero_extend n e = Z3.BitVector.mk_zero_ext ctx n e

      let sign_extend n e = Z3.BitVector.mk_sign_ext ctx n e
    end

    module Float = struct
      module Rounding_mode = struct
        let rne = Z3.FloatingPoint.RoundingMode.mk_rne ctx

        let rna = Z3.FloatingPoint.RoundingMode.mk_rna ctx

        let rtp = Z3.FloatingPoint.RoundingMode.mk_rtp ctx

        let rtn = Z3.FloatingPoint.RoundingMode.mk_rtn ctx

        let rtz = Z3.FloatingPoint.RoundingMode.mk_rtz ctx
      end

      let v f eb sb = Z3.FloatingPoint.mk_numeral_f ctx f (Types.float eb sb)

      let neg e = Z3.FloatingPoint.mk_neg ctx e

      let abs e = Z3.FloatingPoint.mk_abs ctx e

      let sqrt ~rm e = Z3.FloatingPoint.mk_sqrt ctx rm e

      let is_normal e = Z3.FloatingPoint.mk_is_normal ctx e

      let is_subnormal e = Z3.FloatingPoint.mk_is_subnormal ctx e

      let is_negative e = Z3.FloatingPoint.mk_is_negative ctx e

      let is_positive e = Z3.FloatingPoint.mk_is_positive ctx e

      let is_infinite e = Z3.FloatingPoint.mk_is_infinite ctx e

      let is_nan e = Z3.FloatingPoint.mk_is_nan ctx e

      let is_zero e = Z3.FloatingPoint.mk_is_zero ctx e

      let round_to_integral ~rm e =
        Z3.FloatingPoint.mk_round_to_integral ctx rm e

      let add ~rm e1 e2 = Z3.FloatingPoint.mk_add ctx rm e1 e2

      let sub ~rm e1 e2 = Z3.FloatingPoint.mk_sub ctx rm e1 e2

      let mul ~rm e1 e2 = Z3.FloatingPoint.mk_mul ctx rm e1 e2

      let div ~rm e1 e2 = Z3.FloatingPoint.mk_div ctx rm e1 e2

      let min e1 e2 = Z3.FloatingPoint.mk_min ctx e1 e2

      let max e1 e2 = Z3.FloatingPoint.mk_max ctx e1 e2

      let fma ~rm a b c = Z3.FloatingPoint.mk_fma ctx rm a b c

      let rem e1 e2 = Z3.FloatingPoint.mk_rem ctx e1 e2

      let eq e1 e2 = Z3.FloatingPoint.mk_eq ctx e1 e2

      let lt e1 e2 = Z3.FloatingPoint.mk_lt ctx e1 e2

      let le e1 e2 = Z3.FloatingPoint.mk_leq ctx e1 e2

      let gt e1 e2 = Z3.FloatingPoint.mk_gt ctx e1 e2

      let ge e1 e2 = Z3.FloatingPoint.mk_geq ctx e1 e2

      let to_fp eb sb ~rm fp =
        Z3.FloatingPoint.mk_to_fp_float ctx rm fp (Types.float eb sb)

      let sbv_to_fp eb sb ~rm bv =
        Z3.FloatingPoint.mk_to_fp_signed ctx rm bv (Types.float eb sb)

      let ubv_to_fp eb sb ~rm bv =
        Z3.FloatingPoint.mk_to_fp_unsigned ctx rm bv (Types.float eb sb)

      let to_ubv n ~rm fp = Z3.FloatingPoint.mk_to_ubv ctx rm fp n

      let to_sbv n ~rm fp = Z3.FloatingPoint.mk_to_sbv ctx rm fp n

      let of_ieee_bv eb sb bv =
        Z3.FloatingPoint.mk_to_fp_bv ctx bv (Types.float eb sb)

      let to_ieee_bv = Some (fun fp -> Z3.FloatingPoint.mk_to_ieee_bv ctx fp)
    end

    module Func = struct
      let make name params ret = Z3.FuncDecl.mk_func_decl_s ctx name params ret

      let apply f params = Z3.FuncDecl.apply f params
    end

    module Model = struct
      let get_symbols model =
        List.map
          (fun const ->
            let x = Z3.Symbol.to_string (Z3.FuncDecl.get_name const) in
            let t = Types.to_ety (Z3.FuncDecl.get_range const) in
            Symbol.make t x )
          (Z3.Model.get_const_decls model)

      let eval ?ctx:_ ?(completion = false) model term =
        Z3.Model.eval model term completion
    end

    let pp_entry fmt (entry : Z3.Statistics.Entry.statistics_entry) =
      let key = Z3.Statistics.Entry.get_key entry in
      let value = Z3.Statistics.Entry.to_string_value entry in
      Fmt.pf fmt "(%s %s)" key value

    let pp_statistics fmt (stats : Z3.Statistics.statistics) =
      let entries = Z3.Statistics.get_entries stats in
      Fmt.list ~sep:(fun fmt () -> Fmt.pf fmt "@\n") pp_entry fmt entries

    let get_statistics (stats : Z3.Statistics.statistics) =
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

    let set_param (type a) (param : a Params.param) (v : a) : unit =
      match param with
      | Timeout -> Z3.Params.update_param_value ctx "timeout" (string_of_int v)
      | Model -> Z3.Params.update_param_value ctx "model" (string_of_bool v)
      | Unsat_core ->
        Z3.Params.update_param_value ctx "unsat_core" (string_of_bool v)
      | Ematching -> Z3.set_global_param "smt.ematching" (string_of_bool v)
      | Parallel -> Z3.set_global_param "parallel.enable" (string_of_bool v)
      | Num_threads ->
        Z3.set_global_param "parallel.threads.max" (string_of_int v)
      | Debug -> ()

    let set_params (params : Params.t) =
      List.iter (fun (Params.P (p, v)) -> set_param p v) (Params.to_list params)

    module Solver = struct
      (* TODO: parameters *)
      let make ?params ?logic () =
        Option.iter set_params params;
        let logic =
          Option.map
            (fun l -> Fmt.kstr (Z3.Symbol.mk_string ctx) "%a" Logic.pp l)
            logic
        in
        Z3.Solver.mk_solver ctx logic

      let clone solver = Z3.Solver.translate solver ctx

      let push solver = Z3.Solver.push solver

      let pop solver n = Z3.Solver.pop solver n

      let reset solver = Z3.Solver.reset solver

      let add ?ctx:_ solver terms = Z3.Solver.add solver terms

      let check ?ctx:_ solver ~assumptions =
        match Z3.Solver.check solver assumptions with
        | Z3.Solver.UNKNOWN -> `Unknown
        | Z3.Solver.SATISFIABLE -> `Sat
        | Z3.Solver.UNSATISFIABLE -> `Unsat

      let model solver = Z3.Solver.get_model solver

      let add_simplifier solver =
        let simplify = Z3.Simplifier.mk_simplifier ctx "simplify" in
        let solver_eqs = Z3.Simplifier.mk_simplifier ctx "solve-eqs" in
        let then_ =
          List.map
            (Z3.Simplifier.mk_simplifier ctx)
            [ "elim-unconstrained"; "propagate-values"; "simplify" ]
        in
        Z3.Simplifier.and_then ctx simplify solver_eqs then_
        |> Z3.Solver.add_simplifier ctx solver

      let interrupt () = Z3.Tactic.interrupt ctx

      let get_statistics solver =
        get_statistics (Z3.Solver.get_statistics solver)

      let pp_statistics fmt solver =
        pp_statistics fmt @@ Z3.Solver.get_statistics solver
    end

    module Optimizer = struct
      let make () = Z3.Optimize.mk_opt ctx

      let push opt = Z3.Optimize.push opt

      let pop opt = Z3.Optimize.pop opt

      let add opt terms = Z3.Optimize.add opt terms

      let check opt =
        match Z3.Optimize.check opt with
        | Z3.Solver.UNKNOWN -> `Unknown
        | Z3.Solver.SATISFIABLE -> `Sat
        | Z3.Solver.UNSATISFIABLE -> `Unsat

      let model opt = Z3.Optimize.get_model opt

      let maximize opt term = Z3.Optimize.maximize opt term

      let minimize opt term = Z3.Optimize.minimize opt term

      let interrupt () = Z3.Tactic.interrupt ctx

      let get_statistics opt = get_statistics (Z3.Optimize.get_statistics opt)

      let pp_statistics fmt opt =
        pp_statistics fmt @@ Z3.Optimize.get_statistics opt
    end

    module Smtlib = struct
      let pp ?name ?logic ?status fmt =
        let name = Option.value name ~default:"" in
        let logic =
          Fmt.str "%a"
            (Fmt.option ~none:(fun fmt () -> Fmt.string fmt "ALL") Logic.pp)
            logic
        in
        let status =
          match Option.value status ~default:`Unknown with
          | `Sat -> "sat"
          | `Unsat -> "unsat"
          | `Unknown -> "unknown"
        in
        function
        | [] -> ()
        | [ x ] ->
          Fmt.pf fmt "%s"
            (Z3.SMT.benchmark_to_smtstring ctx name logic status "" [] x)
        | hd :: tl ->
          (* Prints formulas in correct order? *)
          let tl = List.rev tl in
          Fmt.pf fmt "%s"
            (Z3.SMT.benchmark_to_smtstring ctx name logic status "" tl hd)
    end
  end

  include Make ()

  let is_available = Internals.is_available
end

module M' : Mappings_intf.M_with_make = M

include Mappings.Make (M)

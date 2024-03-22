include Mappings_intf
module P = Params
module Sym = Symbol

let err = Log.err

module Impl = struct
  open Z3

  type ty = Sort.sort

  type term = Expr.expr

  type interp = term

  type model = Model.model

  type solver = Solver.solver

  type handle = Optimize.handle

  type optimizer = Optimize.optimize

  type cont = context

  type 'a t = cont -> 'a

  let make_cont () = mk_context []

  module Cont = struct
    let return v _ = v

    let bind (v : 'a t) (f : 'a -> 'b t) : 'b t =
     fun (ctx : cont) ->
      let v = v ctx in
      (f v) ctx

    let ( let* ) v f = bind v f

    let map (v : 'a t) (f : 'a -> 'b) : 'b t =
     fun (ctx : cont) ->
      let v = v ctx in
      f v

    let ( let+ ) v f = map v f

    let run (v : 'a t) (ctx : cont) = v ctx
  end

  let true_ = Boolean.mk_true

  let false_ = Boolean.mk_false

  let int i ctx = Arithmetic.Integer.mk_numeral_i ctx i

  let real f ctx = Arithmetic.Real.mk_numeral_s ctx (Float.to_string f)

  let const sym ty ctx = Expr.mk_const_s ctx sym ty

  let not_ e ctx = Boolean.mk_not ctx e

  let and_ e1 e2 ctx = Boolean.mk_and ctx [ e1; e2 ]

  let or_ e1 e2 ctx = Boolean.mk_or ctx [ e1; e2 ]

  let xor e1 e2 ctx = Boolean.mk_xor ctx e1 e2

  let eq e1 e2 ctx = Boolean.mk_eq ctx e1 e2

  let distinct es ctx = Boolean.mk_distinct ctx es

  let ite cond e1 e2 ctx = Boolean.mk_ite ctx cond e1 e2

  module Types = struct
    let int ctx = Arithmetic.Integer.mk_sort ctx

    let real ctx = Arithmetic.Real.mk_sort ctx

    let bool ctx = Boolean.mk_sort ctx

    let string ctx = Seq.mk_string_sort ctx

    let bitv n ctx = BitVector.mk_sort ctx n

    let float eb sb ctx = FloatingPoint.mk_sort ctx eb sb

    let ty term = Expr.get_sort term

    let to_ety sort ctx =
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

    let to_string interp ctx = Seq.get_string ctx interp

    let to_bitv interp _n =
      assert (Expr.is_numeral interp);
      let set (s : string) (i : int) (n : char) =
        let bs = Bytes.of_string s in
        Bytes.set bs i n;
        Bytes.to_string bs
      in
      Int64.of_string (set (Expr.to_string interp) 0 '0')

    let to_float fp _eb _sb ctx =
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
        let _, exponent = FloatingPoint.get_numeral_exponent_int ctx fp true in
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
    let neg e ctx = Arithmetic.mk_unary_minus ctx e

    let to_real e ctx = Arithmetic.Integer.mk_int2real ctx e

    let add e1 e2 ctx = Arithmetic.mk_add ctx [ e1; e2 ]

    let sub e1 e2 ctx = Arithmetic.mk_sub ctx [ e1; e2 ]

    let mul e1 e2 ctx = Arithmetic.mk_mul ctx [ e1; e2 ]

    let div e1 e2 ctx = Arithmetic.mk_div ctx e1 e2

    let rem e1 e2 ctx = Arithmetic.Integer.mk_rem ctx e1 e2

    let pow e1 e2 ctx = Arithmetic.mk_power ctx e1 e2

    let lt e1 e2 ctx = Arithmetic.mk_lt ctx e1 e2

    let le e1 e2 ctx = Arithmetic.mk_le ctx e1 e2

    let gt e1 e2 ctx = Arithmetic.mk_gt ctx e1 e2

    let ge e1 e2 ctx = Arithmetic.mk_ge ctx e1 e2
  end

  module Real = struct
    let neg e ctx = Arithmetic.mk_unary_minus ctx e

    let to_int e ctx = Arithmetic.Real.mk_real2int ctx e

    let add e1 e2 ctx = Arithmetic.mk_add ctx [ e1; e2 ]

    let sub e1 e2 ctx = Arithmetic.mk_sub ctx [ e1; e2 ]

    let mul e1 e2 ctx = Arithmetic.mk_mul ctx [ e1; e2 ]

    let div e1 e2 ctx = Arithmetic.mk_div ctx e1 e2

    let pow e1 e2 ctx = Arithmetic.mk_power ctx e1 e2

    let lt e1 e2 ctx = Arithmetic.mk_lt ctx e1 e2

    let le e1 e2 ctx = Arithmetic.mk_le ctx e1 e2

    let gt e1 e2 ctx = Arithmetic.mk_gt ctx e1 e2

    let ge e1 e2 ctx = Arithmetic.mk_ge ctx e1 e2
  end

  module String = struct
    let v s ctx = Seq.mk_string ctx s

    let length e ctx = Seq.mk_seq_length ctx e

    let to_code e ctx = Seq.mk_string_to_code ctx e

    let of_code e ctx = Seq.mk_string_from_code ctx e

    let at str ~pos ctx = Seq.mk_seq_at ctx str pos

    let concat e1 e2 ctx = Seq.mk_seq_concat ctx [ e1; e2 ]

    let sub str ~pos ~len ctx = Seq.mk_seq_extract ctx str pos len
  end

  module Bitv = struct
    let v bv bitwidth ctx = BitVector.mk_numeral ctx bv bitwidth

    let neg e ctx = BitVector.mk_neg ctx e

    let lognot e ctx = BitVector.mk_not ctx e

    let add e1 e2 ctx = BitVector.mk_add ctx e1 e2

    let sub e1 e2 ctx = BitVector.mk_sub ctx e1 e2

    let mul e1 e2 ctx = BitVector.mk_mul ctx e1 e2

    let div e1 e2 ctx = BitVector.mk_sdiv ctx e1 e2

    let div_u e1 e2 ctx = BitVector.mk_udiv ctx e1 e2

    let logor e1 e2 ctx = BitVector.mk_or ctx e1 e2

    let logand e1 e2 ctx = BitVector.mk_and ctx e1 e2

    let logxor e1 e2 ctx = BitVector.mk_xor ctx e1 e2

    let shl e1 e2 ctx = BitVector.mk_shl ctx e1 e2

    let ashr e1 e2 ctx = BitVector.mk_ashr ctx e1 e2

    let lshr e1 e2 ctx = BitVector.mk_lshr ctx e1 e2

    let rem e1 e2 ctx = BitVector.mk_srem ctx e1 e2

    let rem_u e1 e2 ctx = BitVector.mk_urem ctx e1 e2

    let rotate_left e1 e2 ctx = BitVector.mk_ext_rotate_left ctx e1 e2

    let rotate_right e1 e2 ctx = BitVector.mk_ext_rotate_right ctx e1 e2

    let lt e1 e2 ctx = BitVector.mk_slt ctx e1 e2

    let lt_u e1 e2 ctx = BitVector.mk_ult ctx e1 e2

    let le e1 e2 ctx = BitVector.mk_sle ctx e1 e2

    let le_u e1 e2 ctx = BitVector.mk_ule ctx e1 e2

    let gt e1 e2 ctx = BitVector.mk_sgt ctx e1 e2

    let gt_u e1 e2 ctx = BitVector.mk_ugt ctx e1 e2

    let ge e1 e2 ctx = BitVector.mk_sge ctx e1 e2

    let ge_u e1 e2 ctx = BitVector.mk_uge ctx e1 e2

    let concat e1 e2 ctx = BitVector.mk_concat ctx e1 e2

    let extract e ~high ~low ctx = BitVector.mk_extract ctx high low e

    let zero_extend n e ctx = BitVector.mk_zero_ext ctx n e

    let sign_extend n e ctx = BitVector.mk_sign_ext ctx n e
  end

  module Float = struct
    module Rounding_mode = struct
      let rne ctx = FloatingPoint.RoundingMode.mk_rne ctx

      let rna ctx = FloatingPoint.RoundingMode.mk_rna ctx

      let rtp ctx = FloatingPoint.RoundingMode.mk_rtp ctx

      let rtn ctx = FloatingPoint.RoundingMode.mk_rtn ctx

      let rtz ctx = FloatingPoint.RoundingMode.mk_rtz ctx
    end

    let v f eb sb ctx = FloatingPoint.mk_numeral_f ctx f (Types.float eb sb ctx)

    let neg e ctx = FloatingPoint.mk_neg ctx e

    let abs e ctx = FloatingPoint.mk_abs ctx e

    let sqrt ~rm e ctx = FloatingPoint.mk_sqrt ctx rm e

    let is_nan e ctx = FloatingPoint.mk_is_nan ctx e

    let round_to_integral ~rm e ctx =
      FloatingPoint.mk_round_to_integral ctx rm e

    let add ~rm e1 e2 ctx = FloatingPoint.mk_add ctx rm e1 e2

    let sub ~rm e1 e2 ctx = FloatingPoint.mk_sub ctx rm e1 e2

    let mul ~rm e1 e2 ctx = FloatingPoint.mk_mul ctx rm e1 e2

    let div ~rm e1 e2 ctx = FloatingPoint.mk_div ctx rm e1 e2

    let min e1 e2 ctx = FloatingPoint.mk_min ctx e1 e2

    let max e1 e2 ctx = FloatingPoint.mk_max ctx e1 e2

    let rem e1 e2 ctx = FloatingPoint.mk_rem ctx e1 e2

    let eq e1 e2 ctx = FloatingPoint.mk_eq ctx e1 e2

    let lt e1 e2 ctx = FloatingPoint.mk_lt ctx e1 e2

    let le e1 e2 ctx = FloatingPoint.mk_leq ctx e1 e2

    let gt e1 e2 ctx = FloatingPoint.mk_gt ctx e1 e2

    let ge e1 e2 ctx = FloatingPoint.mk_geq ctx e1 e2

    let to_fp eb sb ~rm fp ctx =
      FloatingPoint.mk_to_fp_float ctx rm fp (Types.float eb sb ctx)

    let sbv_to_fp eb sb ~rm bv ctx =
      FloatingPoint.mk_to_fp_signed ctx rm bv (Types.float eb sb ctx)

    let ubv_to_fp eb sb ~rm bv ctx =
      FloatingPoint.mk_to_fp_unsigned ctx rm bv (Types.float eb sb ctx)

    let to_ubv n ~rm fp ctx = FloatingPoint.mk_to_ubv ctx rm fp n

    let to_sbv n ~rm fp ctx = FloatingPoint.mk_to_sbv ctx rm fp n

    let of_ieee_bv eb sb bv ctx =
      FloatingPoint.mk_to_fp_bv ctx bv (Types.float eb sb ctx)

    let to_ieee_bv fp ctx = FloatingPoint.mk_to_ieee_bv ctx fp
  end

  module Model = struct
    let get_symbols model ctx =
      List.map
        (fun const ->
          let x = Symbol.to_string (FuncDecl.get_name const) in
          let t = Types.to_ety (FuncDecl.get_range const) ctx in
          Sym.make t x )
        (Model.get_const_decls model)

    let eval ?(completion = false) model term = Model.eval model term completion
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

  let _set_and_build_params (params : P.t) : (string * string) list =
    Z3.set_global_param "smt.ematching"
      (string_of_bool @@ P.get params Ematching);
    [ ("timeout", string_of_int @@ P.get params Timeout)
    ; ("model", string_of_bool @@ P.get params Model)
    ; ("unsat_core", string_of_bool @@ P.get params Unsat_core)
    ]

  module Solver = struct
    let make ?params:_ ?logic () ctx =
      (* let params = Option.fold params ~none:[] ~some:set_and_build_params in *)
      (* let ctx = mk_context params in *)
      let logic =
        Option.map
          (fun l ->
            Format.kasprintf (Z3.Symbol.mk_string ctx) "%a" Ty.pp_logic l )
          logic
      in
      Z3.Solver.mk_solver ctx logic

    let clone solver ctx = Solver.translate solver ctx

    let push solver = Solver.push solver

    let pop solver n = Solver.pop solver n

    let reset solver = Solver.reset solver

    let add solver terms = Solver.add solver terms

    let check solver ~assumptions =
      match Solver.check solver assumptions with
      | Solver.UNKNOWN -> Unknown
      | Solver.SATISFIABLE -> Satisfiable
      | Solver.UNSATISFIABLE -> Unsatisfiable

    let model solver = Solver.get_model solver

    let add_simplifier solver ctx =
      let simplify = Simplifier.mk_simplifier ctx "simplify" in
      let solver_eqs = Simplifier.mk_simplifier ctx "solve-eqs" in
      let then_ =
        List.map
          (Simplifier.mk_simplifier ctx)
          [ "elim-unconstrained"; "propagate-values"; "simplify" ]
      in
      Simplifier.and_then ctx simplify solver_eqs then_
      |> Solver.add_simplifier ctx solver

    let interrupt () = Tactic.interrupt

    let pp_statistics fmt solver =
      pp_statistics fmt @@ Solver.get_statistics solver
  end

  module Optimizer = struct
    let make () = Optimize.mk_opt

    let push opt = Optimize.push opt

    let pop opt = Optimize.pop opt

    let add opt terms = Optimize.add opt terms

    let check opt =
      match Optimize.check opt with
      | Z3.Solver.UNKNOWN -> Unknown
      | Z3.Solver.SATISFIABLE -> Satisfiable
      | Z3.Solver.UNSATISFIABLE -> Unsatisfiable

    let model opt = Optimize.get_model opt

    let maximize opt term = Optimize.maximize opt term

    let minimize opt term = Optimize.minimize opt term

    let interrupt () = Tactic.interrupt

    let pp_statistics fmt opt = pp_statistics fmt @@ Optimize.get_statistics opt
  end
end

module Impl' : M = Impl

include Mappings.Make (Impl)

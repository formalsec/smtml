(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2024 formalsec *)
(* Written by Joao Pereira *)

open Cvc5
include Mappings_intf

module Fresh_cvc5 () = struct
  module Internals = struct
    let caches_consts = false

    let is_available = true
  end

  type ty = Sort.sort

  type term = Term.term

  type interp = Term.term

  type model = Solver.solver

  type solver = Solver.solver

  type handle = unit

  type optimizer = unit (* Not supported *)

  type func_decl = unit

  let tm = TermManager.mk_tm ()

  let true_ = Term.mk_true tm

  let false_ = Term.mk_false tm

  let int i = Term.mk_int tm i

  let real r = Term.mk_real_s tm (Float.to_string r)

  let const symbol ty = Term.mk_const_s tm ty symbol

  let not_ t = Term.mk_term tm Kind.Not [| t |]

  let and_ t1 t2 = Term.mk_term tm Kind.And [| t1; t2 |]

  let or_ t1 t2 = Term.mk_term tm Kind.Or [| t1; t2 |]

  let logand ts = Term.mk_term tm Kind.And (Array.of_list ts)

  let logor ts = Term.mk_term tm Kind.Or (Array.of_list ts)

  let xor t1 t2 = Term.mk_term tm Kind.Xor [| t1; t2 |]

  let implies t1 t2 = Term.mk_term tm Kind.Implies [| t1; t2 |]

  let eq t1 t2 = Term.mk_term tm Kind.Equal [| t1; t2 |]

  let distinct ts = Term.mk_term tm Kind.Distinct (Array.of_list ts)

  let ite cond t1 t2 = Term.mk_term tm Kind.Ite [| cond; t1; t2 |]

  let forall _ = Fmt.failwith "Cvc5_mappings: forall not implemented"

  let exists _ = Fmt.failwith "Cvc5_mappings: exists not implemented"

  module Types = struct
    let int = Sort.mk_int_sort tm

    let real = Sort.mk_real_sort tm

    let bool = Sort.mk_bool_sort tm

    let string = Sort.mk_string_sort tm

    let bitv bitwidth = Sort.mk_bv_sort tm bitwidth

    let float ebits sbits = Sort.mk_fp_sort tm ebits sbits

    let roundingMode = Sort.mk_rm_sort tm

    let regexp = Sort.mk_bool_sort tm

    let ty t = Term.sort t

    let to_ety _ = assert false
  end

  module Interp = struct
    let to_int t = Term.get_int t

    let to_real t = Term.get_real t

    let to_bool t = Term.get_bool t

    let to_string _ = assert false

    let to_bitv t bitwidth =
      assert (Term.is_bv t);
      let set (s : string) (i : int) (n : char) =
        let bs = Bytes.of_string s in
        Bytes.set bs i n;
        Bytes.to_string bs
      in
      let bv =
        let bv = Term.get_bv t bitwidth in
        if String.starts_with ~prefix:"#" bv then set bv 0 '0' else bv
      in
      Z.of_string bv

    let to_float _t _ebits _sbits = assert false
  end

  module Int = struct
    let neg t = Term.mk_term tm Kind.Neg [| t |]

    let to_real t = Term.mk_term tm Kind.To_real [| t |]

    let to_bv _m _t = Fmt.failwith "Cvc5_mappings: Int.to_bv not implemented"

    let add t1 t2 = Term.mk_term tm Kind.Add [| t1; t2 |]

    let sub t1 t2 = Term.mk_term tm Kind.Sub [| t1; t2 |]

    let mul t1 t2 = Term.mk_term tm Kind.Mult [| t1; t2 |]

    let div t1 t2 = Term.mk_term tm Kind.Ints_division [| t1; t2 |]

    let rem t1 t2 = Term.mk_term tm Kind.Ints_modulus [| t1; t2 |]

    let mod_ t1 t2 = Term.mk_term tm Kind.Ints_modulus [| t1; t2 |]

    let pow t1 t2 = Term.mk_term tm Kind.Pow [| t1; t2 |]

    let lt t1 t2 = Term.mk_term tm Kind.Lt [| t1; t2 |]

    let le t1 t2 = Term.mk_term tm Kind.Leq [| t1; t2 |]

    let gt t1 t2 = Term.mk_term tm Kind.Gt [| t1; t2 |]

    let ge t1 t2 = Term.mk_term tm Kind.Geq [| t1; t2 |]
  end

  module Real = struct
    let neg t = Term.mk_term tm Kind.Neg [| t |]

    let to_int t = Term.mk_term tm Kind.To_integer [| t |]

    let add t1 t2 = Term.mk_term tm Kind.Add [| t1; t2 |]

    let sub t1 t2 = Term.mk_term tm Kind.Sub [| t1; t2 |]

    let mul t1 t2 = Term.mk_term tm Kind.Mult [| t1; t2 |]

    let div t1 t2 = Term.mk_term tm Kind.Division [| t1; t2 |]

    let pow t1 t2 = Term.mk_term tm Kind.Pow [| t1; t2 |]

    let lt t1 t2 = Term.mk_term tm Kind.Lt [| t1; t2 |]

    let le t1 t2 = Term.mk_term tm Kind.Leq [| t1; t2 |]

    let gt t1 t2 = Term.mk_term tm Kind.Gt [| t1; t2 |]

    let ge t1 t2 = Term.mk_term tm Kind.Geq [| t1; t2 |]
  end

  module String = struct
    let v s = Term.mk_string tm s

    let length t = Term.mk_term tm Kind.String_length [| t |]

    let to_code t = Term.mk_term tm Kind.String_to_code [| t |]

    let of_code t = Term.mk_term tm Kind.String_from_code [| t |]

    let to_int t = Term.mk_term tm Kind.String_to_int [| t |]

    let of_int t = Term.mk_term tm Kind.String_from_int [| t |]

    let to_re t = Term.mk_term tm Kind.String_to_regexp [| t |]

    let in_re t1 t2 = Term.mk_term tm Kind.String_in_regexp [| t1; t2 |]

    let at t ~pos =
      let one = Term.mk_int tm 1 in
      Term.mk_term tm Kind.String_substr [| t; pos; one |]

    let concat ts = Term.mk_term tm Kind.String_concat (Array.of_list ts)

    let contains t1 ~sub = Term.mk_term tm Kind.String_contains [| t1; sub |]

    let is_prefix t1 ~prefix =
      Term.mk_term tm Kind.String_prefix [| t1; prefix |]

    let is_suffix t1 ~suffix =
      Term.mk_term tm Kind.String_suffix [| t1; suffix |]

    let lt _ = Fmt.failwith "Cvc5_mappings: String.lt not implemented"

    let le _ = Fmt.failwith "Cvc5_mappings: String.le not implemented"

    let sub s ~pos ~len = Term.mk_term tm Kind.String_substr [| s; pos; len |]

    let index_of t1 ~sub ~pos =
      Term.mk_term tm Kind.String_indexof [| t1; sub; pos |]

    let replace t1 ~pattern ~with_ =
      Term.mk_term tm Kind.String_replace [| t1; pattern; with_ |]

    let replace_all t1 ~pattern ~with_ =
      Term.mk_term tm Kind.String_replace_all [| t1; pattern; with_ |]

    let replace_re t1 ~pattern ~with_ =
      Term.mk_term tm Kind.String_replace_re [| t1; pattern; with_ |]

    let replace_re_all t1 ~pattern ~with_ =
      Term.mk_term tm Kind.String_replace_re_all [| t1; pattern; with_ |]
  end

  module Re = struct
    let allchar () = Term.mk_term tm Kind.Regexp_allchar [||]

    let all () = Term.mk_term tm Kind.Regexp_all [||]

    let none () = Term.mk_term tm Kind.Regexp_none [||]

    let star t = Term.mk_term tm Kind.Regexp_star [| t |]

    let plus t = Term.mk_term tm Kind.Regexp_plus [| t |]

    let opt t = Term.mk_term tm Kind.Regexp_opt [| t |]

    let comp t = Term.mk_term tm Kind.Regexp_complement [| t |]

    let range t1 t2 = Term.mk_term tm Kind.Regexp_range [| t1; t2 |]

    let diff t1 t2 = Term.mk_term tm Kind.Regexp_diff [| t1; t2 |]

    let inter t1 t2 = Term.mk_term tm Kind.Regexp_inter [| t1; t2 |]

    let loop t i1 i2 =
      let op = Op.mk_op tm Kind.Regexp_loop [| i1; i2 |] in
      Term.mk_term_op tm op [| t |]

    let union ts = Term.mk_term tm Kind.Regexp_union (Array.of_list ts)

    let concat ts = Term.mk_term tm Kind.Regexp_concat (Array.of_list ts)
  end

  module Bitv = struct
    let v v_str bitwidth = Term.mk_bv_s tm bitwidth v_str 10

    let neg t = Term.mk_term tm Kind.Bitvector_neg [| t |]

    let lognot t = Term.mk_term tm Kind.Bitvector_not [| t |]

    let to_int ~signed:_ _t =
      Fmt.failwith "Cvc5_mappings: Int.to_int not implemented"

    let add t1 t2 = Term.mk_term tm Kind.Bitvector_add [| t1; t2 |]

    let sub t1 t2 = Term.mk_term tm Kind.Bitvector_sub [| t1; t2 |]

    let mul t1 t2 = Term.mk_term tm Kind.Bitvector_mult [| t1; t2 |]

    let div t1 t2 = Term.mk_term tm Kind.Bitvector_sdiv [| t1; t2 |]

    let div_u t1 t2 = Term.mk_term tm Kind.Bitvector_udiv [| t1; t2 |]

    let logor t1 t2 = Term.mk_term tm Kind.Bitvector_or [| t1; t2 |]

    let logand t1 t2 = Term.mk_term tm Kind.Bitvector_and [| t1; t2 |]

    let logxor t1 t2 = Term.mk_term tm Kind.Bitvector_xor [| t1; t2 |]

    let shl t1 t2 = Term.mk_term tm Kind.Bitvector_shl [| t1; t2 |]

    let ashr t1 t2 = Term.mk_term tm Kind.Bitvector_ashr [| t1; t2 |]

    let lshr t1 t2 = Term.mk_term tm Kind.Bitvector_lshr [| t1; t2 |]

    let rem t1 t2 = Term.mk_term tm Kind.Bitvector_srem [| t1; t2 |]

    let rem_u t1 t2 = Term.mk_term tm Kind.Bitvector_urem [| t1; t2 |]

    let rotate_left t1 t2 =
      Term.mk_term tm Kind.Bitvector_rotate_left [| t1; t2 |]

    let rotate_right t1 t2 =
      Term.mk_term tm Kind.Bitvector_rotate_right [| t1; t2 |]

    let lt t1 t2 = Term.mk_term tm Kind.Bitvector_slt [| t1; t2 |]

    let lt_u t1 t2 = Term.mk_term tm Kind.Bitvector_ult [| t1; t2 |]

    let le t1 t2 = Term.mk_term tm Kind.Bitvector_sle [| t1; t2 |]

    let le_u t1 t2 = Term.mk_term tm Kind.Bitvector_ule [| t1; t2 |]

    let gt t1 t2 = Term.mk_term tm Kind.Bitvector_sgt [| t1; t2 |]

    let gt_u t1 t2 = Term.mk_term tm Kind.Bitvector_ugt [| t1; t2 |]

    let ge t1 t2 = Term.mk_term tm Kind.Bitvector_sge [| t1; t2 |]

    let ge_u t1 t2 = Term.mk_term tm Kind.Bitvector_uge [| t1; t2 |]

    let concat t1 t2 = Term.mk_term tm Kind.Bitvector_concat [| t1; t2 |]

    let extract x ~high ~low =
      let op = Op.mk_op tm Kind.Bitvector_extract [| high; low |] in
      Term.mk_term_op tm op [| x |]

    let zero_extend n t =
      let op = Op.mk_op tm Kind.Bitvector_zero_extend [| n |] in
      Term.mk_term_op tm op [| t |]

    let sign_extend n t =
      let op = Op.mk_op tm Kind.Bitvector_sign_extend [| n |] in
      Term.mk_term_op tm op [| t |]
  end

  module Float = struct
    module Rounding_mode = struct
      let rne = Term.mk_rm tm RoundingMode.Rne

      let rna = Term.mk_rm tm RoundingMode.Rna

      let rtp = Term.mk_rm tm RoundingMode.Rtp

      let rtn = Term.mk_rm tm RoundingMode.Rtn

      let rtz = Term.mk_rm tm RoundingMode.Rtz
    end

    let extract_sign_i32 i32 =
      (Int32.to_int @@ Int32.shift_right_logical i32 31) land 1

    let extract_exponent_i32 i32 =
      Int32.logand (Int32.shift_right_logical i32 23) 0xFFl

    let extract_significand_i32 i32 = Int32.logand i32 0x7FFFFFl

    let v32 (i : int32) es eb =
      let sign = Term.mk_bv_s tm 1 (string_of_int @@ extract_sign_i32 i) 10 in
      let exp =
        Term.mk_bv_s tm es (Int32.to_string @@ extract_exponent_i32 i) 10
      in
      let sig_ =
        Term.mk_bv_s tm (eb - 1)
          (Int32.to_string @@ extract_significand_i32 i)
          10
      in
      Term.mk_fp_from_terms tm sign exp sig_

    let extract_sign_i64 i64 =
      (Int64.to_int @@ Int64.shift_right_logical i64 63) land 1

    let extract_exponent_i64 i64 =
      Int64.logand (Int64.shift_right_logical i64 52) 0x7FFL

    let extract_significand_i64 i64 = Int64.logand i64 0xFFFFFFFFFFFFFL

    let v64 (i : int64) es eb =
      let sign = Term.mk_bv_s tm 1 (string_of_int @@ extract_sign_i64 i) 10 in
      let exp =
        Term.mk_bv_s tm es (Int64.to_string @@ extract_exponent_i64 i) 10
      in
      let sig_ =
        Term.mk_bv_s tm (eb - 1)
          (Int64.to_string @@ extract_significand_i64 i)
          10
      in
      Term.mk_fp_from_terms tm sign exp sig_

    let v f es eb =
      match es + eb with
      | 32 -> v32 (Int32.bits_of_float f) es eb
      | 64 -> v64 (Int64.bits_of_float f) es eb
      | _ -> Fmt.failwith "Cvc5_mappings: Unsupported floating-point size"

    let neg t = Term.mk_term tm Kind.Floatingpoint_neg [| t |]

    let abs t = Term.mk_term tm Kind.Floatingpoint_abs [| t |]

    let sqrt ~rm t = Term.mk_term tm Kind.Floatingpoint_sqrt [| rm; t |]

    let is_normal t = Term.mk_term tm Kind.Floatingpoint_is_normal [| t |]

    let is_subnormal t = Term.mk_term tm Kind.Floatingpoint_is_subnormal [| t |]

    let is_negative t = Term.mk_term tm Kind.Floatingpoint_is_neg [| t |]

    let is_positive t = Term.mk_term tm Kind.Floatingpoint_is_pos [| t |]

    let is_infinite t = Term.mk_term tm Kind.Floatingpoint_is_inf [| t |]

    let is_nan t = Term.mk_term tm Kind.Floatingpoint_is_nan [| t |]

    let is_zero t = Term.mk_term tm Kind.Floatingpoint_is_zero [| t |]

    let round_to_integral ~rm t =
      Term.mk_term tm Kind.Floatingpoint_rti [| rm; t |]

    let add ~rm t1 t2 = Term.mk_term tm Kind.Floatingpoint_add [| rm; t1; t2 |]

    let sub ~rm t1 t2 = Term.mk_term tm Kind.Floatingpoint_sub [| rm; t1; t2 |]

    let mul ~rm t1 t2 = Term.mk_term tm Kind.Floatingpoint_mult [| rm; t1; t2 |]

    let div ~rm t1 t2 = Term.mk_term tm Kind.Floatingpoint_div [| rm; t1; t2 |]

    let min t1 t2 = Term.mk_term tm Kind.Floatingpoint_min [| t1; t2 |]

    let max t1 t2 = Term.mk_term tm Kind.Floatingpoint_max [| t1; t2 |]

    let fma ~rm a b c = Term.mk_term tm Kind.Floatingpoint_fma [| rm; a; b; c |]

    let rem t1 t2 = Term.mk_term tm Kind.Floatingpoint_rem [| t1; t2 |]

    let eq t1 t2 = Term.mk_term tm Kind.Floatingpoint_eq [| t1; t2 |]

    let lt t1 t2 = Term.mk_term tm Kind.Floatingpoint_lt [| t1; t2 |]

    let le t1 t2 = Term.mk_term tm Kind.Floatingpoint_leq [| t1; t2 |]

    let gt t1 t2 = Term.mk_term tm Kind.Floatingpoint_gt [| t1; t2 |]

    let ge t1 t2 = Term.mk_term tm Kind.Floatingpoint_geq [| t1; t2 |]

    let to_fp i1 i2 ~rm t =
      let op = Op.mk_op tm Kind.Floatingpoint_to_fp_from_fp [| i1; i2 |] in
      Term.mk_term_op tm op [| rm; t |]

    let sbv_to_fp i1 i2 ~rm t =
      let op = Op.mk_op tm Kind.Floatingpoint_to_fp_from_sbv [| i1; i2 |] in
      Term.mk_term_op tm op [| rm; t |]

    let ubv_to_fp i1 i2 ~rm t =
      let op = Op.mk_op tm Kind.Floatingpoint_to_fp_from_ubv [| i1; i2 |] in
      Term.mk_term_op tm op [| rm; t |]

    let to_ubv i ~rm t =
      let op = Op.mk_op tm Kind.Floatingpoint_to_ubv [| i |] in
      Term.mk_term_op tm op [| rm; t |]

    let to_sbv i ~rm t =
      let op = Op.mk_op tm Kind.Floatingpoint_to_sbv [| i |] in
      Term.mk_term_op tm op [| rm; t |]

    let of_ieee_bv i1 i2 t =
      let op = Op.mk_op tm Kind.Floatingpoint_to_fp_from_ieee_bv [| i1; i2 |] in
      Term.mk_term_op tm op [| t |]

    let to_ieee_bv = None
  end

  module Func = struct
    let make _ _ _ = ()

    let apply () _ = false_
  end

  (* TODO *)
  module Model = struct
    let get_symbols _ =
      Fmt.failwith "Cvc5_mappings: get_symbols not implemented"

    let eval ?ctx:_ ?completion:_ solver term =
      Some (Solver.get_value solver term)
  end

  module Solver = struct
    let set_param (type a) slv (param : a Params.param) (v : a) : unit =
      match param with
      | Timeout -> Solver.set_option slv "tlimit" (string_of_int v)
      | Model -> Solver.set_option slv "produce-models" (string_of_bool v)
      | Unsat_core ->
        Solver.set_option slv "produce-unsat-cores" (string_of_bool v)
      | Ematching -> Solver.set_option slv "e-matching" (string_of_bool v)
      | Parallel | Num_threads | Debug -> ()

    let set_params slv params =
      List.iter
        (fun (Params.P (p, v)) -> set_param slv p v)
        (Params.to_list params)

    let make ?params ?logic () =
      let logic = Option.map (fun l -> Fmt.str "%a" Logic.pp l) logic in
      let slv = Solver.mk_solver ?logic tm in
      begin
        match params with
        | None -> set_params slv (Params.default ())
        | Some params -> set_params slv params
      end;
      slv

    let clone _ = assert false

    let push solver = Solver.push solver 1

    let pop solver n = Solver.pop solver n

    let reset solver = Solver.reset solver

    let add ?ctx:_ solver ts = List.iter (Solver.assert_formula solver) ts

    (* FIXME: refactor Result class to only include
       SAT/UNSAT/UNKNOWN types? *)
    let check ?ctx:_ solver ~assumptions =
      let assumptions = Array.of_list assumptions in
      let result = Solver.check_sat_assuming solver assumptions in
      match Result.is_sat result with
      | true -> `Sat
      | false -> (
        match Result.is_unsat result with true -> `Unsat | false -> `Unknown )

    let model solver = Some solver

    let add_simplifier solver = solver

    let interrupt _ = ()

    let get_statistics _ =
      Fmt.failwith "Cvc5_mappings: Solver.get_statistics not implemented!"

    let pp_statistics _ =
      Fmt.failwith "Cvc5_mappings: Solver.pp_statistics not implemented!"
  end

  (* Not supported *)
  module Optimizer = struct
    let make _ = Fmt.failwith "Cvc5_mappings: Optimizer.make not implemented"

    let push _ = Fmt.failwith "Cvc5_mappings: Optimizer.push not implemented"

    let pop _ = Fmt.failwith "Cvc5_mappings: Optimizer.pop not implemented"

    let add _ = Fmt.failwith "Cvc5_mappings: Optimizer.add not implemented"

    let check _ = Fmt.failwith "Cvc5_mappings: Optimizer.check not implemented"

    let model _ = Fmt.failwith "Cvc5_mappings: Optimizer.model not implemented"

    let maximize _ =
      Fmt.failwith "Cvc5_mappings: Optimizer.maximize not implemented"

    let minimize _ =
      Fmt.failwith "Cvc5_mappings: Optimizer.minimize not implemented"

    let interrupt _ =
      Fmt.failwith "Cvc5_mappings: Optimizer.interrupt not implemented"

    let pp_statistics _ =
      Fmt.failwith "Cvc5_mappings: Optimizer.pp_statistics not implemented"

    let get_statistics _ =
      Fmt.failwith "Cvc5_mappings: Optimizer.get_statistics not implemented"
  end

  module Smtlib = struct
    let pp ?name:_ ?logic:_ ?status:_ _fmt _ =
      Fmt.failwith "Cvc5_mappings: Smtlib.pp not implemented"
  end
end

module Cvc5_with_make : Mappings_intf.M_with_make = struct
  module Make () = Fresh_cvc5 ()
  include Fresh_cvc5 ()

  let is_available = Internals.is_available
end

include Mappings.Make (Cvc5_with_make)

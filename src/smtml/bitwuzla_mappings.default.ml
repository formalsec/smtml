(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2024 formalsec *)
(* Written by the Smtml programmers *)

include Mappings_intf

module Fresh_bitwuzla (B : Bitwuzla_cxx.S) : M = struct
  open B

  module Internals = struct
    let caches_consts = false

    let is_available = true
  end

  type ty = Sort.t

  type term = Term.t

  type interp = Term.t

  type model = Solver.t

  type solver = Solver.t

  (* Not supported *)
  type handle = unit

  (* Not supported *)
  type optimizer = unit

  (* Not supported *)
  type func_decl = Term.t

  let true_ = mk_true ()

  let false_ = mk_false ()

  let int _ = Fmt.failwith "Bitwuzla_mappings: int not implemented"

  let real _ = Fmt.failwith "Bitwuzla_mappings: real not implemented"

  let const symbol ty = mk_const ~symbol ty

  let not_ t = mk_term1 Kind.Not t

  let and_ t1 t2 = mk_term2 Kind.And t1 t2

  let or_ t1 t2 = mk_term2 Kind.Or t1 t2

  let logand ts = mk_term Kind.And (Array.of_list ts)

  let logor ts = mk_term Kind.Or (Array.of_list ts)

  let xor t1 t2 = mk_term2 Kind.Xor t1 t2

  let implies t1 t2 = mk_term2 Kind.Implies t1 t2

  let eq t1 t2 = mk_term2 Kind.Equal t1 t2

  let distinct ts = mk_term Kind.Distinct (Array.of_list ts)

  let ite cond t1 t2 = mk_term3 Kind.Ite cond t1 t2

  let forall _ = Fmt.failwith "Bitwuzla_mappings: forall not implemented"

  let exists _ = Fmt.failwith "Bitwuzla_mappings: exists not implemented"

  module Types = struct
    let int = mk_bool_sort ()

    let real = mk_bool_sort ()

    let bool = mk_bool_sort ()

    let string = mk_bool_sort ()

    let bitv bitwidth = mk_bv_sort bitwidth

    let float ebits sbits = mk_fp_sort ebits sbits

    let roundingMode = mk_rm_sort ()

    let regexp = mk_bool_sort ()

    let ty t = Term.sort t

    let to_ety _ = Fmt.failwith "Bitwuzla_mappings: to_ety not implemented"
  end

  module Interp = struct
    let to_int _ = Fmt.failwith "Bitwuzla_mappings: to_int not implemented"

    let to_real _ = Fmt.failwith "Bitwuzla_mappings: to_real not implemented"

    let to_bool t = Term.value Term.Bool t

    let to_string _ =
      Fmt.failwith "Bitwuzla_mappings: to_string not implemented"

    let to_bitv t _bitwidth = Z.to_int64 @@ Term.value Term.Z t

    let to_float t ebits sbits =
      let fp_size = ebits + sbits in
      let sign, exp, significant = Term.value Term.IEEE_754 t in
      let bs = String.(cat sign (cat exp significant)) in
      assert (String.length bs = fp_size);
      let _n, int64_ =
        String.fold_left
          (fun (n, acc) c ->
            let bit =
              match c with '0' -> 0L | '1' -> 1L | _ -> assert false
            in
            let acc = Int64.logor acc bit in
            let acc = if n = fp_size - 1 then acc else Int64.shift_left acc 1 in
            (n + 1, acc) )
          (0, 0L) bs
      in
      match fp_size with
      | 32 -> Int32.float_of_bits (Int64.to_int32 int64_)
      | 64 -> Int64.float_of_bits int64_
      | _ -> assert false
  end

  module Int = struct
    let neg _ = Fmt.failwith "Bitwuzla_mappings: Int.neg not implemented"

    let to_real _ =
      Fmt.failwith "Bitwuzla_mappings: Int.to_real not implemented"

    let add _ = Fmt.failwith "Bitwuzla_mappings: Int.add not implemented"

    let sub _ = Fmt.failwith "Bitwuzla_mappings: Int.sub not implemented"

    let mul _ = Fmt.failwith "Bitwuzla_mappings: Int.mul not implemented"

    let div _ = Fmt.failwith "Bitwuzla_mappings: Int.div not implemented"

    let rem _ = Fmt.failwith "Bitwuzla_mappings: Int.rem not implemented"

    let pow _ = Fmt.failwith "Bitwuzla_mappings: Int.pow not implemented"

    let lt _ = Fmt.failwith "Bitwuzla_mappings: Int.lt not implemented"

    let le _ = Fmt.failwith "Bitwuzla_mappings: Int.le not implemented"

    let gt _ = Fmt.failwith "Bitwuzla_mappings: Int.gt not implemented"

    let ge _ = Fmt.failwith "Bitwuzla_mappings: Int.ge not implemented"
  end

  module Real = struct
    let neg _ = Fmt.failwith "Bitwuzla_mappings: Real.neg not implemented"

    let to_int _ = Fmt.failwith "Bitwuzla_mappings: Real.to_int not implemented"

    let add _ = Fmt.failwith "Bitwuzla_mappings: Real.add not implemented"

    let sub _ = Fmt.failwith "Bitwuzla_mappings: Real.sub not implemented"

    let mul _ = Fmt.failwith "Bitwuzla_mappings: Real.mul not implemented"

    let div _ = Fmt.failwith "Bitwuzla_mappings: Real.div not implemented"

    let pow _ = Fmt.failwith "Bitwuzla_mappings: Real.pow not implemented"

    let lt _ = Fmt.failwith "Bitwuzla_mappings: Real.lt not implemented"

    let le _ = Fmt.failwith "Bitwuzla_mappings: Real.le not implemented"

    let gt _ = Fmt.failwith "Bitwuzla_mappings: Real.gt not implemented"

    let ge _ = Fmt.failwith "Bitwuzla_mappings: Real.ge not implemented"
  end

  module String = struct
    let v _ = Fmt.failwith "Bitwuzla_mappings: String.v not implemented"

    let length _ =
      Fmt.failwith "Bitwuzla_mappings: String.length not implemented"

    let to_code _ =
      Fmt.failwith "Bitwuzla_mappings: String.to_code not implemented"

    let of_code _ =
      Fmt.failwith "Bitwuzla_mappings: String.of_code not implemented"

    let to_int _ =
      Fmt.failwith "Bitwuzla_mappings: String.to_int not implemented"

    let of_int _ =
      Fmt.failwith "Bitwuzla_mappings: String.of_int not implemented"

    let to_re _ = Fmt.failwith "Bitwuzla_mappings: String.to_re not implemented"

    let in_re _ = Fmt.failwith "Bitwuzla_mappings: String.in_re not implemented"

    let at _ = Fmt.failwith "Bitwuzla_mappings: String.at not implemented"

    let concat _ =
      Fmt.failwith "Bitwuzla_mappings: String.concat not implemented"

    let contains _ ~sub:_ =
      Fmt.failwith "Bitwuzla_mappings: String.contains not implemented"

    let is_prefix _ ~prefix:_ =
      Fmt.failwith "Bitwuzla_mappings: String.is_prefix not implemented"

    let is_suffix _ ~suffix:_ =
      Fmt.failwith "Bitwuzla_mappings: String.is_suffix not implemented"

    let lt _ = Fmt.failwith "Bitwuzla_mappings: String.lt not implemented"

    let le _ = Fmt.failwith "Bitwuzla_mappings: String.le not implemented"

    let sub _ ~pos:_ ~len:_ =
      Fmt.failwith "Bitwuzla_mappings: String.sub not implemented"

    let index_of _ ~sub:_ ~pos:_ =
      Fmt.failwith "Bitwuzla_mappings: String.index_of not implemented"

    let replace _ ~pattern:_ ~with_:_ =
      Fmt.failwith "Bitwuzla_mappings: String.replace not implemented"

    let replace_all _ ~pattern:_ ~with_:_ =
      Fmt.failwith "Bitwuzla_mappings: String.replace_all not implemented"
  end

  module Re = struct
    let allchar _ = Fmt.failwith "Bitwuzla_mappings: Re.allchar not implemented"

    let all _ = Fmt.failwith "Bitwuzla_mappings: Re.all not implemented"

    let none _ = Fmt.failwith "Bitwuzla_mappings: Re.none not implemented"

    let star _ = Fmt.failwith "Bitwuzla_mappings: Re.star not implemented"

    let plus _ = Fmt.failwith "Bitwuzla_mappings: Re.plus not implemented"

    let opt _ = Fmt.failwith "Bitwuzla_mappings: Re.opt not implemented"

    let comp _ = Fmt.failwith "Bitwuzla_mappings: Re.comp not implemented"

    let range _ = Fmt.failwith "Bitwuzla_mappings: Re.range not implemented"

    let inter _ = Fmt.failwith "Bitwuzla_mappings: Re.inter not implemented"

    let loop _ = Fmt.failwith "Bitwuzla_mappings: Re.loop not implemented"

    let union _ = Fmt.failwith "Bitwuzla_mappings: Re.union not implemented"

    let concat _ = Fmt.failwith "Bitwuzla_mappings: Re.concat not implemented"
  end

  module Bitv = struct
    let v str bitwidth = mk_bv_value (Types.bitv bitwidth) str 10

    let neg t = mk_term1 Kind.Bv_neg t

    let lognot t = mk_term1 Kind.Bv_not t

    let add t1 t2 = mk_term2 Kind.Bv_add t1 t2

    let sub t1 t2 = mk_term2 Kind.Bv_sub t1 t2

    let mul t1 t2 = mk_term2 Kind.Bv_mul t1 t2

    let div t1 t2 = mk_term2 Kind.Bv_sdiv t1 t2

    let div_u t1 t2 = mk_term2 Kind.Bv_udiv t1 t2

    let logor t1 t2 = mk_term2 Kind.Bv_or t1 t2

    let logand t1 t2 = mk_term2 Kind.Bv_and t1 t2

    let logxor t1 t2 = mk_term2 Kind.Bv_xor t1 t2

    let shl t1 t2 = mk_term2 Kind.Bv_shl t1 t2

    let ashr t1 t2 = mk_term2 Kind.Bv_ashr t1 t2

    let lshr t1 t2 = mk_term2 Kind.Bv_shr t1 t2

    let rem t1 t2 = mk_term2 Kind.Bv_srem t1 t2

    let rem_u t1 t2 = mk_term2 Kind.Bv_urem t1 t2

    let rotate_left t1 t2 = mk_term2 Kind.Bv_rol t1 t2

    let rotate_right t1 t2 = mk_term2 Kind.Bv_ror t1 t2

    let lt t1 t2 = mk_term2 Kind.Bv_slt t1 t2

    let lt_u t1 t2 = mk_term2 Kind.Bv_ult t1 t2

    let le t1 t2 = mk_term2 Kind.Bv_sle t1 t2

    let le_u t1 t2 = mk_term2 Kind.Bv_ule t1 t2

    let gt t1 t2 = mk_term2 Kind.Bv_sgt t1 t2

    let gt_u t1 t2 = mk_term2 Kind.Bv_ugt t1 t2

    let ge t1 t2 = mk_term2 Kind.Bv_sge t1 t2

    let ge_u t1 t2 = mk_term2 Kind.Bv_uge t1 t2

    let concat t1 t2 = mk_term2 Kind.Bv_concat t1 t2

    let extract x ~high ~low = mk_term1_indexed2 Kind.Bv_extract x high low

    let zero_extend n t = mk_term1_indexed1 Kind.Bv_zero_extend t n

    let sign_extend n t = mk_term1_indexed1 Kind.Bv_sign_extend t n
  end

  module Float = struct
    module Rounding_mode = struct
      let rne = mk_rm_value RoundingMode.Rne

      let rna = mk_rm_value RoundingMode.Rna

      let rtp = mk_rm_value RoundingMode.Rtp

      let rtn = mk_rm_value RoundingMode.Rtn

      let rtz = mk_rm_value RoundingMode.Rtz
    end

    let extract_sign_i32 i32 =
      (Int32.to_int @@ Int32.shift_right_logical i32 31) land 1

    let extract_exponent_i32 i32 =
      Int32.logand (Int32.shift_right_logical i32 23) 0xFFl

    let extract_significand_i32 i32 = Int32.logand i32 0x7FFFFFl

    let v32 (i : int32) es eb =
      let sort_sign = mk_bv_sort 1 in
      let sign =
        mk_bv_value sort_sign (string_of_int @@ extract_sign_i32 i) 10
      in
      let sort_exp = mk_bv_sort es in
      let exp =
        mk_bv_value sort_exp (Int32.to_string @@ extract_exponent_i32 i) 10
      in
      let sort_sig_ = mk_bv_sort (eb - 1) in
      let sig_ =
        mk_bv_value sort_sig_ (Int32.to_string @@ extract_significand_i32 i) 10
      in
      mk_fp_value sign exp sig_

    let extract_sign_i64 i64 =
      (Int64.to_int @@ Int64.shift_right_logical i64 63) land 1

    let extract_exponent_i64 i64 =
      Int64.logand (Int64.shift_right_logical i64 52) 0x7FFL

    let extract_significand_i64 i64 = Int64.logand i64 0xFFFFFFFFFFFFFL

    let v64 (i : int64) es eb =
      let sort_sign = mk_bv_sort 1 in
      let sign =
        mk_bv_value sort_sign (string_of_int @@ extract_sign_i64 i) 10
      in
      let sort_exp = mk_bv_sort es in
      let exp =
        mk_bv_value sort_exp (Int64.to_string @@ extract_exponent_i64 i) 10
      in
      let sort_sig_ = mk_bv_sort (eb - 1) in
      let sig_ =
        mk_bv_value sort_sig_ (Int64.to_string @@ extract_significand_i64 i) 10
      in
      mk_fp_value sign exp sig_

    let v f es eb =
      match es + eb with
      | 32 -> v32 (Int32.bits_of_float f) es eb
      | 64 -> v64 (Int64.bits_of_float f) es eb
      | _ -> Fmt.failwith "Bitwuzla_mappings: Unsupported floating-point size"

    let neg t = mk_term1 Kind.Fp_neg t

    let abs t = mk_term1 Kind.Fp_abs t

    let sqrt ~rm t = mk_term2 Kind.Fp_sqrt rm t

    let is_normal t = mk_term1 Kind.Fp_is_normal t

    let is_subnormal t = mk_term1 Kind.Fp_is_subnormal t

    let is_negative t = mk_term1 Kind.Fp_is_neg t

    let is_positive t = mk_term1 Kind.Fp_is_pos t

    let is_infinite t = mk_term1 Kind.Fp_is_inf t

    let is_nan t = mk_term1 Kind.Fp_is_nan t

    let is_zero t = mk_term1 Kind.Fp_is_zero t

    let round_to_integral ~rm t = mk_term2 Kind.Fp_rti rm t

    let add ~rm lhs rhs = mk_term3 Kind.Fp_add rm lhs rhs

    let sub ~rm lhs rhs = mk_term3 Kind.Fp_sub rm lhs rhs

    let mul ~rm lhs rhs = mk_term3 Kind.Fp_mul rm lhs rhs

    let div ~rm lhs rhs = mk_term3 Kind.Fp_div rm lhs rhs

    let min t1 t2 = mk_term2 Kind.Fp_min t1 t2

    let max t1 t2 = mk_term2 Kind.Fp_max t1 t2

    let fma ~rm a b c = mk_term Kind.Fp_fma [| rm; a; b; c |]

    let rem t1 t2 = mk_term2 Kind.Fp_rem t1 t2

    let eq t1 t2 = mk_term2 Kind.Fp_equal t1 t2

    let lt t1 t2 = mk_term2 Kind.Fp_lt t1 t2

    let le t1 t2 = mk_term2 Kind.Fp_leq t1 t2

    let gt t1 t2 = mk_term2 Kind.Fp_gt t1 t2

    let ge t1 t2 = mk_term2 Kind.Fp_geq t1 t2

    let to_fp eb sb ~rm t = mk_term2_indexed2 Kind.Fp_to_fp_from_fp rm t eb sb

    let sbv_to_fp eb sb ~rm bv =
      mk_term2_indexed2 Kind.Fp_to_fp_from_sbv rm bv eb sb

    let ubv_to_fp eb sb ~rm bv =
      mk_term2_indexed2 Kind.Fp_to_fp_from_ubv rm bv eb sb

    let to_ubv m ~rm t = mk_term2_indexed1 Kind.Fp_to_ubv rm t m

    let to_sbv m ~rm t = mk_term2_indexed1 Kind.Fp_to_sbv rm t m

    let of_ieee_bv eb sb bv = mk_term1_indexed2 Kind.Fp_to_fp_from_bv bv eb sb

    let to_ieee_bv = None
  end

  module Func = struct
    let make symbol args return_ =
      let args = Array.of_list args in
      let sort = B.mk_fun_sort args return_ in
      B.mk_const sort ~symbol

    let apply f args =
      let args = f :: args in
      mk_term Kind.Apply @@ Array.of_list args
  end

  module Model = struct
    let get_symbols _ =
      Fmt.failwith "Bitwuzla_mappings: get_symbols not implemented"

    let eval ?ctx:_ ?completion:_ solver term =
      Some (Solver.get_value solver term)
  end

  module Solver = struct
    let update_options _params options =
      Bitwuzla_cxx.Options.(set options Produce_models true);
      options

    let make ?params ?logic:_ () =
      Bitwuzla_cxx.Options.default () |> update_options params |> Solver.create

    let clone _solver = Fmt.failwith "Bitwuzla_mappings: clone not implemented"

    let push solver = Solver.push solver 1

    let pop solver n = Solver.pop solver n

    let reset _ = Fmt.failwith "Bitwuzla_mappings: reset not implemented"

    let add ?ctx:_ solver ts = List.iter (Solver.assert_formula solver) ts

    let check ?ctx:_ solver ~assumptions =
      let assumptions = Array.of_list assumptions in
      match Solver.check_sat ~assumptions solver with
      | Result.Sat -> `Sat
      | Result.Unsat -> `Unsat
      | Result.Unknown -> `Unknown

    let model solver = Some solver

    let add_simplifier solver =
      (* does nothing *)
      solver

    let interrupt _ =
      (* does nothing *)
      ()

    let get_statistics _ =
      Fmt.failwith "Bitwuzla_mappings: Solver.get_statistics not implemented"

    let pp_statistics fmt solver = Solver.pp_statistics fmt solver
  end

  module Optimizer = struct
    let make _ =
      Fmt.failwith "Bitwuzla_mappings: Optimizer.make not implemented"

    let push _ =
      Fmt.failwith "Bitwuzla_mappings: Optimizer.push not implemented"

    let pop _ = Fmt.failwith "Bitwuzla_mappings: Optimizer.pop not implemented"

    let add _ = Fmt.failwith "Bitwuzla_mappings: Optimizer.add not implemented"

    let check _ =
      Fmt.failwith "Bitwuzla_mappings: Optimizer.check not implemented"

    let model _ =
      Fmt.failwith "Bitwuzla_mappings: Optimizer.model not implemented"

    let maximize _ =
      Fmt.failwith "Bitwuzla_mappings: Optimizer.maximize not implemented"

    let minimize _ =
      Fmt.failwith "Bitwuzla_mappings: Optimizer.minimize not implemented"

    let interrupt _ =
      Fmt.failwith "Bitwuzla_mappings: Optimizer.interrupt not implemented"

    let get_statistics _ =
      Fmt.failwith "Bitwuzla_mappings: Solver.get_statistics not implemented"

    let pp_statistics _ =
      Fmt.failwith "Bitwuzla_mappings: Optimizer.pp_statistics not implemented"
  end

  module Smtlib = struct
    let pp ?name:_ ?logic:_ ?status:_ _fmt _ =
      Fmt.failwith "Bitwuzla_mappings: Smtlib.pp not implemented"
  end
end

include (
  Mappings.Make (struct
    module Make () = Fresh_bitwuzla (Bitwuzla_cxx.Make ())
    include Fresh_bitwuzla (Bitwuzla_cxx)

    let is_available = Internals.is_available
  end) :
    S_with_fresh )

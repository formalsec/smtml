(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2026 formalsec *)
(* Written by the Smtml programmers *)

include Mappings_intf

module Fresh_bitwuzla (B : Bitwuzla_cxx.S) : M = struct
  open B

  module Internals = struct
    let name = "bitwuzla"

    let caches_consts = false

    let is_available = true

    let was_interrupted = ref false
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

  let int _ =
    Fmt.failwith "%s:%d: %s not implemented" __MODULE__ __LINE__ __FUNCTION__

  let real _ =
    Fmt.failwith "%s:%d: %s not implemented" __MODULE__ __LINE__ __FUNCTION__

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

  let forall _ =
    Fmt.failwith "%s:%d: %s not implemented" __MODULE__ __LINE__ __FUNCTION__

  let exists _ =
    Fmt.failwith "%s:%d: %s not implemented" __MODULE__ __LINE__ __FUNCTION__

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

    let to_ety _ =
      Fmt.failwith "%s:%d: %s not implemented" __MODULE__ __LINE__ __FUNCTION__
  end

  module Interp = struct
    let to_int _ =
      Fmt.failwith "%s:%d: %s not implemented" __MODULE__ __LINE__ __FUNCTION__

    let to_real _ =
      Fmt.failwith "%s:%d: %s not implemented" __MODULE__ __LINE__ __FUNCTION__

    let to_bool t = Term.value Term.Bool t

    let to_string _ =
      Fmt.failwith "%s:%d: %s not implemented" __MODULE__ __LINE__ __FUNCTION__

    let to_bitv t _bitwidth = Term.value Term.Z t

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
    let neg _ =
      Fmt.failwith "%s:%d: %s not implemented" __MODULE__ __LINE__ __FUNCTION__

    let to_real _ =
      Fmt.failwith "%s:%d: %s not implemented" __MODULE__ __LINE__ __FUNCTION__

    let to_bv _ =
      Fmt.failwith "%s:%d: %s not implemented" __MODULE__ __LINE__ __FUNCTION__

    let add _ =
      Fmt.failwith "%s:%d: %s not implemented" __MODULE__ __LINE__ __FUNCTION__

    let sub _ =
      Fmt.failwith "%s:%d: %s not implemented" __MODULE__ __LINE__ __FUNCTION__

    let mul _ =
      Fmt.failwith "%s:%d: %s not implemented" __MODULE__ __LINE__ __FUNCTION__

    let div _ =
      Fmt.failwith "%s:%d: %s not implemented" __MODULE__ __LINE__ __FUNCTION__

    let rem _ =
      Fmt.failwith "%s:%d: %s not implemented" __MODULE__ __LINE__ __FUNCTION__

    let mod_ _ =
      Fmt.failwith "%s:%d: %s not implemented" __MODULE__ __LINE__ __FUNCTION__

    let pow _ =
      Fmt.failwith "%s:%d: %s not implemented" __MODULE__ __LINE__ __FUNCTION__

    let lt _ =
      Fmt.failwith "%s:%d: %s not implemented" __MODULE__ __LINE__ __FUNCTION__

    let le _ =
      Fmt.failwith "%s:%d: %s not implemented" __MODULE__ __LINE__ __FUNCTION__

    let gt _ =
      Fmt.failwith "%s:%d: %s not implemented" __MODULE__ __LINE__ __FUNCTION__

    let ge _ =
      Fmt.failwith "%s:%d: %s not implemented" __MODULE__ __LINE__ __FUNCTION__
  end

  module Real = struct
    let neg _ =
      Fmt.failwith "%s:%d: %s not implemented" __MODULE__ __LINE__ __FUNCTION__

    let to_int _ =
      Fmt.failwith "%s:%d: %s not implemented" __MODULE__ __LINE__ __FUNCTION__

    let add _ =
      Fmt.failwith "%s:%d: %s not implemented" __MODULE__ __LINE__ __FUNCTION__

    let sub _ =
      Fmt.failwith "%s:%d: %s not implemented" __MODULE__ __LINE__ __FUNCTION__

    let mul _ =
      Fmt.failwith "%s:%d: %s not implemented" __MODULE__ __LINE__ __FUNCTION__

    let div _ =
      Fmt.failwith "%s:%d: %s not implemented" __MODULE__ __LINE__ __FUNCTION__

    let pow _ =
      Fmt.failwith "%s:%d: %s not implemented" __MODULE__ __LINE__ __FUNCTION__

    let lt _ =
      Fmt.failwith "%s:%d: %s not implemented" __MODULE__ __LINE__ __FUNCTION__

    let le _ =
      Fmt.failwith "%s:%d: %s not implemented" __MODULE__ __LINE__ __FUNCTION__

    let gt _ =
      Fmt.failwith "%s:%d: %s not implemented" __MODULE__ __LINE__ __FUNCTION__

    let ge _ =
      Fmt.failwith "%s:%d: %s not implemented" __MODULE__ __LINE__ __FUNCTION__
  end

  module String = struct
    let v _ =
      Fmt.failwith "%s:%d: %s not implemented" __MODULE__ __LINE__ __FUNCTION__

    let length _ =
      Fmt.failwith "%s:%d: %s not implemented" __MODULE__ __LINE__ __FUNCTION__

    let to_code _ =
      Fmt.failwith "%s:%d: %s not implemented" __MODULE__ __LINE__ __FUNCTION__

    let of_code _ =
      Fmt.failwith "%s:%d: %s not implemented" __MODULE__ __LINE__ __FUNCTION__

    let to_int _ =
      Fmt.failwith "%s:%d: %s not implemented" __MODULE__ __LINE__ __FUNCTION__

    let of_int _ =
      Fmt.failwith "%s:%d: %s not implemented" __MODULE__ __LINE__ __FUNCTION__

    let to_re _ =
      Fmt.failwith "%s:%d: %s not implemented" __MODULE__ __LINE__ __FUNCTION__

    let in_re _ =
      Fmt.failwith "%s:%d: %s not implemented" __MODULE__ __LINE__ __FUNCTION__

    let at _ =
      Fmt.failwith "%s:%d: %s not implemented" __MODULE__ __LINE__ __FUNCTION__

    let concat _ =
      Fmt.failwith "%s:%d: %s not implemented" __MODULE__ __LINE__ __FUNCTION__

    let contains _ ~sub:_ =
      Fmt.failwith "%s:%d: %s not implemented" __MODULE__ __LINE__ __FUNCTION__

    let is_prefix _ ~prefix:_ =
      Fmt.failwith "%s:%d: %s not implemented" __MODULE__ __LINE__ __FUNCTION__

    let is_suffix _ ~suffix:_ =
      Fmt.failwith "%s:%d: %s not implemented" __MODULE__ __LINE__ __FUNCTION__

    let lt _ =
      Fmt.failwith "%s:%d: %s not implemented" __MODULE__ __LINE__ __FUNCTION__

    let le _ =
      Fmt.failwith "%s:%d: %s not implemented" __MODULE__ __LINE__ __FUNCTION__

    let sub _ ~pos:_ ~len:_ =
      Fmt.failwith "%s:%d: %s not implemented" __MODULE__ __LINE__ __FUNCTION__

    let index_of _ ~sub:_ ~pos:_ =
      Fmt.failwith "%s:%d: %s not implemented" __MODULE__ __LINE__ __FUNCTION__

    let last_index_of _ ~sub:_ =
      Fmt.failwith "%s:%d: %s not implemented" __MODULE__ __LINE__ __FUNCTION__

    let replace _ ~pattern:_ ~with_:_ =
      Fmt.failwith "%s:%d: %s not implemented" __MODULE__ __LINE__ __FUNCTION__

    let replace_all _ ~pattern:_ ~with_:_ =
      Fmt.failwith "%s:%d: %s not implemented" __MODULE__ __LINE__ __FUNCTION__

    let replace_re _ ~pattern:_ ~with_:_ =
      Fmt.failwith "%s:%d: %s not implemented" __MODULE__ __LINE__ __FUNCTION__

    let replace_re_all _ ~pattern:_ ~with_:_ =
      Fmt.failwith "%s:%d: %s not implemented" __MODULE__ __LINE__ __FUNCTION__
  end

  module Re = struct
    let allchar _ =
      Fmt.failwith "%s:%d: %s not implemented" __MODULE__ __LINE__ __FUNCTION__

    let all _ =
      Fmt.failwith "%s:%d: %s not implemented" __MODULE__ __LINE__ __FUNCTION__

    let none _ =
      Fmt.failwith "%s:%d: %s not implemented" __MODULE__ __LINE__ __FUNCTION__

    let star _ =
      Fmt.failwith "%s:%d: %s not implemented" __MODULE__ __LINE__ __FUNCTION__

    let plus _ =
      Fmt.failwith "%s:%d: %s not implemented" __MODULE__ __LINE__ __FUNCTION__

    let opt _ =
      Fmt.failwith "%s:%d: %s not implemented" __MODULE__ __LINE__ __FUNCTION__

    let comp _ =
      Fmt.failwith "%s:%d: %s not implemented" __MODULE__ __LINE__ __FUNCTION__

    let range _ =
      Fmt.failwith "%s:%d: %s not implemented" __MODULE__ __LINE__ __FUNCTION__

    let diff _ =
      Fmt.failwith "%s:%d: %s not implemented" __MODULE__ __LINE__ __FUNCTION__

    let inter _ =
      Fmt.failwith "%s:%d: %s not implemented" __MODULE__ __LINE__ __FUNCTION__

    let loop ~min:_ ~max:_ _ =
      Fmt.failwith "%s:%d: %s not implemented" __MODULE__ __LINE__ __FUNCTION__

    let union _ =
      Fmt.failwith "%s:%d: %s not implemented" __MODULE__ __LINE__ __FUNCTION__

    let concat _ =
      Fmt.failwith "%s:%d: %s not implemented" __MODULE__ __LINE__ __FUNCTION__
  end

  module Bitv = struct
    let v str bitwidth = mk_bv_value (Types.bitv bitwidth) str 10

    let of_z v bitwidth =
      let ty = Types.bitv bitwidth in
      if Z.fits_int v then mk_bv_value_int ty (Z.to_int v)
      else if Z.fits_int64 v then mk_bv_value_int64 ty (Z.to_int64 v)
      else mk_bv_value ty (Z.to_string v) 10

    let neg t = mk_term1 Kind.Bv_neg t

    let lognot t = mk_term1 Kind.Bv_not t

    let to_int ~signed:_ _t =
      Fmt.failwith "%s:%d: %s not implemented" __MODULE__ __LINE__ __FUNCTION__

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

    let smod t1 t2 = mk_term2 Kind.Bv_smod t1 t2

    let rem t1 t2 = mk_term2 Kind.Bv_srem t1 t2

    let rem_u t1 t2 = mk_term2 Kind.Bv_urem t1 t2

    let rotate_left t1 t2 = mk_term2 Kind.Bv_rol t1 t2

    let rotate_right t1 t2 = mk_term2 Kind.Bv_ror t1 t2

    let nego t = mk_term1 Kind.Bv_nego t

    let addo ~signed t1 t2 =
      mk_term2 (if signed then Kind.Bv_saddo else Kind.Bv_uaddo) t1 t2

    let subo ~signed t1 t2 =
      mk_term2 (if signed then Kind.Bv_ssubo else Kind.Bv_usubo) t1 t2

    let mulo ~signed t1 t2 =
      mk_term2 (if signed then Kind.Bv_smulo else Kind.Bv_umulo) t1 t2

    let divo t1 t2 = mk_term2 Kind.Bv_sdivo t1 t2

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

  module Adt = struct
    module Cons = struct
      type t = [ `Adt_constructor ]

      let make _ ~fields:_ =
        Fmt.failwith "%s:%d: %s not implemented" __MODULE__ __LINE__
          __FUNCTION__
    end

    type t = [ `Adt ]

    let make _ _cons = `Adt

    let ty _ = assert false

    let constructor _ =
      Fmt.failwith "%s:%d: %s not implemented" __MODULE__ __LINE__ __FUNCTION__

    let selector _ =
      Fmt.failwith "%s:%d: %s not implemented" __MODULE__ __LINE__ __FUNCTION__

    let tester _ =
      Fmt.failwith "%s:%d: %s not implemented" __MODULE__ __LINE__ __FUNCTION__
  end

  module Model = struct
    let get_symbols _ =
      Fmt.failwith "%s:%d: %s not implemented" __MODULE__ __LINE__ __FUNCTION__

    let eval ?ctx:_ ?completion:_ solver term =
      Some (Solver.get_value solver term)
  end

  module Solver = struct
    let update_options _params options =
      Bitwuzla_cxx.Options.(set options Produce_models true);
      options

    let make ?params ?logic:_ () =
      Bitwuzla_cxx.Options.default () |> update_options params |> Solver.create

    let clone _solver =
      Fmt.failwith "%s:%d: %s not implemented" __MODULE__ __LINE__ __FUNCTION__

    let push solver = Solver.push solver 1

    let pop solver n = Solver.pop solver n

    let reset _ =
      Fmt.failwith "%s:%d: %s not implemented" __MODULE__ __LINE__ __FUNCTION__

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

    let get_statistics _ = Statistics.Map.empty

    let pp_statistics fmt solver = Solver.pp_statistics fmt solver
  end

  module Optimizer = struct
    let make _ =
      Fmt.failwith "%s:%d: %s not implemented" __MODULE__ __LINE__ __FUNCTION__

    let push _ =
      Fmt.failwith "%s:%d: %s not implemented" __MODULE__ __LINE__ __FUNCTION__

    let pop _ =
      Fmt.failwith "%s:%d: %s not implemented" __MODULE__ __LINE__ __FUNCTION__

    let add _ =
      Fmt.failwith "%s:%d: %s not implemented" __MODULE__ __LINE__ __FUNCTION__

    let check _ =
      Fmt.failwith "%s:%d: %s not implemented" __MODULE__ __LINE__ __FUNCTION__

    let model _ =
      Fmt.failwith "%s:%d: %s not implemented" __MODULE__ __LINE__ __FUNCTION__

    let maximize _ =
      Fmt.failwith "%s:%d: %s not implemented" __MODULE__ __LINE__ __FUNCTION__

    let minimize _ =
      Fmt.failwith "%s:%d: %s not implemented" __MODULE__ __LINE__ __FUNCTION__

    let interrupt _ =
      Fmt.failwith "%s:%d: %s not implemented" __MODULE__ __LINE__ __FUNCTION__

    let get_statistics _ =
      Fmt.failwith "%s:%d: %s not implemented" __MODULE__ __LINE__ __FUNCTION__

    let pp_statistics _ =
      Fmt.failwith "%s:%d: %s not implemented" __MODULE__ __LINE__ __FUNCTION__
  end

  module Smtlib = struct
    let pp ?name:_ ?logic:_ ?status:_ _fmt _ =
      Fmt.failwith "%s:%d: %s not implemented" __MODULE__ __LINE__ __FUNCTION__
  end
end

include (
  Mappings.Make (struct
    module Make () = Fresh_bitwuzla (Bitwuzla_cxx.Make ())
    include Fresh_bitwuzla (Bitwuzla_cxx)

    let is_available = Internals.is_available
  end) :
    S_with_fresh )

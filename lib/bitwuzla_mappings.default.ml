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

let _debug = false

module Fresh_bitwuzla (B : Bitwuzla_cxx.S) : M = struct
  open B

  type ty = Sort.t

  type term = Term.t

  type interp = Term.t

  type model = Solver.t

  type solver = Solver.t

  (* Not supported *)
  type handle = unit

  (* Not supported *)
  type optimizer = unit

  let true_ = mk_true ()

  let false_ = mk_false ()

  let int _ = failwith "Bitwuzla_mappings: int not implemented"

  let real _ = failwith "Bitwuzla_mappings: real not implemented"

  let const symbol ty = mk_const ~symbol ty

  let not_ t = mk_term1 Kind.Not t

  let and_ t1 t2 = mk_term2 Kind.And t1 t2

  let or_ t1 t2 = mk_term2 Kind.Or t1 t2

  let xor t1 t2 = mk_term2 Kind.Xor t1 t2

  let eq t1 t2 = mk_term2 Kind.Equal t1 t2

  let distinct ts = mk_term Kind.Distinct (Array.of_list ts)

  let ite cond t1 t2 = mk_term3 Kind.Ite cond t1 t2

  module Types = struct
    let int = Obj.magic 0xdeadc0de

    let real = Obj.magic 0xdeadbeef

    let bool = mk_bool_sort ()

    let string = Obj.magic 0xbadcafe

    let bitv bitwidth = mk_bv_sort bitwidth

    let float ebits sbits = mk_fp_sort ebits sbits

    let ty t = Term.sort t

    let to_ety _ = failwith "Bitwuzla_mappings: to_ety not implemented"
  end

  module Interp = struct
    let to_int _ = failwith "Bitwuzla_mappings: to_int not implemented"

    let to_real _ = failwith "Bitwuzla_mappings: to_real not implemented"

    let to_bool t = Term.value Term.Bool t

    let to_string _ = failwith "Bitwuzla_mappings: to_string not implemented"

    let to_bitv t _bitwidth = Z.to_int64 @@ Term.value Term.Z t

    let to_float t ebits sbits =
      let fp_size = ebits + sbits in
      let sign, exp, significant = Term.value Term.IEEE_754 t in
      let bs = sign ^ exp ^ significant in
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
    let neg _ = failwith "Bitwuzla_mappings: Int.neg not implemented"

    let to_real _ = failwith "Bitwuzla_mappings: Int.to_real not implemented"

    let add _ = failwith "Bitwuzla_mappings: Int.add not implemented"

    let sub _ = failwith "Bitwuzla_mappings: Int.sub not implemented"

    let mul _ = failwith "Bitwuzla_mappings: Int.mul not implemented"

    let div _ = failwith "Bitwuzla_mappings: Int.div not implemented"

    let rem _ = failwith "Bitwuzla_mappings: Int.rem not implemented"

    let pow _ = failwith "Bitwuzla_mappings: Int.pow not implemented"

    let lt _ = failwith "Bitwuzla_mappings: Int.lt not implemented"

    let le _ = failwith "Bitwuzla_mappings: Int.le not implemented"

    let gt _ = failwith "Bitwuzla_mappings: Int.gt not implemented"

    let ge _ = failwith "Bitwuzla_mappings: Int.ge not implemented"
  end

  module Real = struct
    let neg _ = assert false

    let to_int _ = assert false

    let add _ = assert false

    let sub _ = assert false

    let mul _ = assert false

    let div _ = assert false

    let pow _ = assert false

    let lt _ = assert false

    let le _ = assert false

    let gt _ = assert false

    let ge _ = assert false
  end

  module String = struct
    let v _ = assert false

    let length _ = assert false

    let to_code _ = assert false

    let of_code _ = assert false

    let to_int _ = assert false

    let of_int _ = assert false

    let at _ = assert false

    let concat _ = assert false

    let contains _ ~sub:_ = assert false

    let is_prefix _ ~prefix:_ = assert false

    let is_suffix _ ~suffix:_ = assert false

    let sub _ ~pos:_ ~len:_ = assert false

    let index_of _ ~sub:_ ~pos:_ = assert false

    let replace _ ~pattern:_ ~with_:_ = assert false
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

    let v real ebits sbits =
      let real_s = string_of_float real in
      mk_fp_value_from_real (Types.float ebits sbits) Rounding_mode.rne real_s

    let neg t = mk_term1 Kind.Fp_neg t

    let abs t = mk_term1 Kind.Fp_abs t

    let sqrt ~rm t = mk_term2 Kind.Fp_sqrt rm t

    let is_nan t = mk_term1 Kind.Fp_is_nan t

    let round_to_integral ~rm t = mk_term2 Kind.Fp_rti rm t

    let add ~rm lhs rhs = mk_term3 Kind.Fp_add rm lhs rhs

    let sub ~rm lhs rhs = mk_term3 Kind.Fp_sub rm lhs rhs

    let mul ~rm lhs rhs = mk_term3 Kind.Fp_mul rm lhs rhs

    let div ~rm lhs rhs = mk_term3 Kind.Fp_div rm lhs rhs

    let min t1 t2 = mk_term2 Kind.Fp_min t1 t2

    let max t1 t2 = mk_term2 Kind.Fp_max t1 t2

    let rem t1 t2 = mk_term2 Kind.Fp_rem t1 t2

    let eq t1 t2 = mk_term2 Kind.Fp_equal t1 t2

    let lt t1 t2 = mk_term2 Kind.Fp_lt t1 t2

    let le t1 t2 = mk_term2 Kind.Fp_leq t1 t2

    let gt t1 t2 = mk_term2 Kind.Fp_gt t1 t2

    let ge t1 t2 = mk_term2 Kind.Fp_geq t1 t2

    (* TODO *)
    let to_fp _eb _sb ~rm:_ _t =
      (* match rm with *)
      (* | Some rm -> mk_term2_indexed2 Kind.Fp_to_fp_from_fp rm t eb sb *)
      (* | None -> mk_term1_indexed2 Kind.Fp_to_fp_from_bv t eb sb *)
      failwith "Bitwuzla_mappings: to_fp not implemented"

    let sbv_to_fp eb sb ~rm bv =
      mk_term2_indexed2 Kind.Fp_to_fp_from_sbv rm bv eb sb

    let ubv_to_fp eb sb ~rm bv =
      mk_term2_indexed2 Kind.Fp_to_fp_from_ubv rm bv eb sb

    let to_ubv m ~rm t = mk_term2_indexed1 Kind.Fp_to_ubv rm t m

    let to_sbv m ~rm t = mk_term2_indexed1 Kind.Fp_to_sbv rm t m

    (* TODO *)
    let of_ieee_bv _ = failwith "Bitwuzla_mappings: of_ieee_bv not implemented"

    (* TODO *)
    let to_ieee_bv _ = failwith "Bitwuzla_mappings: to_ieee_bv not implemented"
  end

  module Model = struct
    let get_symbols _ =
      failwith "Bitwuzla_mappings: get_symbols not implemented"

    let eval ?completion:_ solver term = Some (Solver.get_value solver term)
  end

  module Solver = struct
    let update_options _params options =
      Bitwuzla_cxx.Options.(set options Produce_models true);
      options

    let make ?params ?logic:_ () =
      Bitwuzla_cxx.Options.default () |> update_options params |> Solver.create

    let clone _solver = failwith "Bitwuzla_mappings: clone not implemented"

    let push solver = Solver.push solver 1

    let pop solver n = Solver.pop solver n

    let reset _ = failwith "Bitwuzla_mappings: reset not implemented"

    let add solver ts = List.iter (Solver.assert_formula solver) ts

    let check solver ~assumptions =
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

    let pp_statistics fmt solver = Solver.pp_statistics fmt solver
  end

  module Optimizer = struct
    let make _ = assert false

    let push _ = assert false

    let pop _ = assert false

    let add _ = assert false

    let check _ = assert false

    let model _ = assert false

    let maximize _ = assert false

    let minimize _ = assert false

    let interrupt _ = assert false

    let pp_statistics _ = assert false
  end
end

include (
  Mappings.Make (struct
    module Make () = Fresh_bitwuzla (Bitwuzla_cxx.Make ())
    include Fresh_bitwuzla (Bitwuzla_cxx)
  end) :
    S_with_fresh )

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

module Fresh = struct
  module Make () = struct
    open Cvc5
    include Mappings_intf

    module Impl = struct
      type ty = Sort.sort

      type term = Term.term

      type interp = Term.term

      type model = unit (* TODO *)

      type solver = Solver.solver

      type handle = unit

      type optimizer = unit (* Not supported *)

      type cont = unit

      type 'a t = 'a

      let tm = TermManager.mk_tm ()

      let make_cont () = ()

      module Cont = struct
        let return v = v [@@inline]

        let bind v f = f v [@@inline]

        let ( let* ) v f = bind v f [@@inline]

        let map v f = f v [@@inline]

        let ( let+ ) v f = map v f [@@inline]

        let run v () = v [@@inline]
      end

      let true_ = Term.mk_true tm

      let false_ = Term.mk_false tm

      let int i = Term.mk_int tm i

      let real r = Term.mk_real_s tm (Float.to_string r)

      let cache = Hashtbl.create 100

      let const symbol ty =
        match Hashtbl.find_opt cache symbol with
        | Some t -> t
        | None ->
          let t = Term.mk_const_s tm ty symbol in
          Hashtbl.add cache symbol t;
          t

      let not_ t = Term.mk_term tm Kind.Not [| t |]

      let and_ t1 t2 = Term.mk_term tm Kind.And [| t1; t2 |]

      let or_ t1 t2 = Term.mk_term tm Kind.Or [| t1; t2 |]

      let xor t1 t2 = Term.mk_term tm Kind.Xor [| t1; t2 |]

      let eq t1 t2 = Term.mk_term tm Kind.Equal [| t1; t2 |]

      let distinct ts = Term.mk_term tm Kind.Distinct (Array.of_list ts)

      let ite cond t1 t2 = Term.mk_term tm Kind.Ite [| cond; t1; t2 |]

      module Types = struct
        let int = Sort.mk_int_sort tm

        let real = Sort.mk_real_sort tm

        let bool = Sort.mk_bool_sort tm

        let string = Sort.mk_string_sort tm

        let bitv bitwidth = Sort.mk_bv_sort tm bitwidth

        let float ebits sbits = Sort.mk_fp_sort tm ebits sbits

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
          Int64.of_string (set (Term.get_bv t bitwidth) 0 '0')

        let to_float _t _ebits _sbits = assert false
      end

      module Int = struct
        let neg t = Term.mk_term tm Kind.Neg [| t |]

        let to_real t = Term.mk_term tm Kind.To_real [| t |]

        let add t1 t2 = Term.mk_term tm Kind.Add [| t1; t2 |]

        let sub t1 t2 = Term.mk_term tm Kind.Sub [| t1; t2 |]

        let mul t1 t2 = Term.mk_term tm Kind.Mult [| t1; t2 |]

        let div t1 t2 = Term.mk_term tm Kind.Ints_division [| t1; t2 |]

        let rem t1 t2 = Term.mk_term tm Kind.Ints_modulus [| t1; t2 |]

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

        let at t ~pos =
          let one = Term.mk_int tm 1 in
          Term.mk_term tm Kind.String_substr [| t; pos; one |]

        let concat t1 t2 = Term.mk_term tm Kind.String_concat [| t1; t2 |]

        let contains t1 ~sub =
          Term.mk_term tm Kind.String_contains [| t1; sub |]

        let is_prefix t1 ~prefix =
          Term.mk_term tm Kind.String_prefix [| t1; prefix |]

        let is_suffix t1 ~suffix =
          Term.mk_term tm Kind.String_suffix [| t1; suffix |]

        let sub s ~pos ~len =
          Term.mk_term tm Kind.String_substr [| s; pos; len |]

        let index_of t1 ~sub ~pos =
          Term.mk_term tm Kind.String_indexof [| t1; sub; pos |]

        let replace t1 ~pattern ~with_ =
          Term.mk_term tm Kind.String_replace [| t1; pattern; with_ |]
      end

      module Bitv = struct
        let v v_str bitwidth = Term.mk_bv_s tm bitwidth v_str 10

        let neg t = Term.mk_term tm Kind.Bitvector_neg [| t |]

        let lognot t = Term.mk_term tm Kind.Bitvector_not [| t |]

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

        let v f es eb =
          match Float.is_nan f with
          | true -> Term.mk_fp_nan tm es eb
          | _ ->
            let b = int_of_float f in
            let bt = Term.mk_bv tm (es + eb) b in
            Term.mk_fp tm es eb bt

        let neg t = Term.mk_term tm Kind.Floatingpoint_neg [| t |]

        let abs t = Term.mk_term tm Kind.Floatingpoint_abs [| t |]

        let sqrt ~rm t = Term.mk_term tm Kind.Floatingpoint_sqrt [| rm; t |]

        let is_nan t = Term.mk_term tm Kind.Floatingpoint_is_nan [| t |]

        let round_to_integral ~rm t =
          Term.mk_term tm Kind.Floatingpoint_rti [| rm; t |]

        let add ~rm t1 t2 =
          Term.mk_term tm Kind.Floatingpoint_add [| rm; t1; t2 |]

        let sub ~rm t1 t2 =
          Term.mk_term tm Kind.Floatingpoint_sub [| rm; t1; t2 |]

        let mul ~rm t1 t2 =
          Term.mk_term tm Kind.Floatingpoint_mult [| rm; t1; t2 |]

        let div ~rm t1 t2 =
          Term.mk_term tm Kind.Floatingpoint_div [| rm; t1; t2 |]

        let min t1 t2 = Term.mk_term tm Kind.Floatingpoint_min [| t1; t2 |]

        let max t1 t2 = Term.mk_term tm Kind.Floatingpoint_max [| t1; t2 |]

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
          let op =
            Op.mk_op tm Kind.Floatingpoint_to_fp_from_ieee_bv [| i1; i2 |]
          in
          Term.mk_term_op tm op [| t |]

        let to_ieee_bv _ = assert false
      end

      (* TODO *)
      module Model = struct
        let get_symbols _ = assert false

        let eval ?completion:_ _ = assert false
      end

      module Solver = struct
        let set_params slv params =
          Solver.set_option slv "e-matching"
            (string_of_bool @@ Params.get params Ematching);
          Solver.set_option slv "tlimit"
            (string_of_int @@ Params.get params Timeout);
          Solver.set_option slv "produce-models"
            (string_of_bool @@ Params.get params Model);
          Solver.set_option slv "produce-unsat-cores"
            (string_of_bool @@ Params.get params Unsat_core)

        let make ?params ?logic () =
          let logic =
            Option.map (fun l -> Format.asprintf "%a" Ty.pp_logic l) logic
          in
          let slv = Solver.mk_solver ?logic tm in
          Option.iter (set_params slv) params;
          slv

        let clone _ = assert false

        let push solver = Solver.push solver 1

        let pop solver n = Solver.pop solver n

        let reset solver = Solver.reset solver

        let add solver ts = List.iter (Solver.assert_formula solver) ts

        (* FIXME: refactor Result class to only include
           SAT/UNSAT/UNKNOWN types? *)
        let check solver ~assumptions =
          let assumptions = Array.of_list assumptions in
          let result = Solver.check_sat_assuming solver assumptions in
          match Result.is_sat result with
          | true -> `Sat
          | false -> (
            match Result.is_unsat result with
            | true -> `Unsat
            | false -> `Unknown )

        let model _ = assert false

        let add_simplifier solver = solver

        let interrupt _ = ()

        let pp_statistics _ = assert false
      end

      (* Not supported *)
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

    module Impl' : M = Impl

    include Mappings.Make (Impl)
  end
end

include Fresh.Make ()

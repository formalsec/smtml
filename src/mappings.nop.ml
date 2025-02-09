(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2024 formalsec *)
(* Written by the Smtml programmers *)

module Nop = struct
  module Make () = struct
    type ty = unit

    type term = unit

    type interp

    type model

    type solver

    type handle

    type optimizer

    type func_decl = unit

    let caches_consts = false

    let true_ = ()

    let false_ = ()

    let int _ = assert false

    let real _ = assert false

    let const _ = assert false

    let not_ _ = assert false

    let and_ _ = assert false

    let or_ _ = assert false

    let logand _ = assert false

    let logor _ = assert false

    let xor _ = assert false

    let eq _ = assert false

    let distinct _ = assert false

    let ite _ = assert false

    let forall _ _ = assert false

    let exists _ _ = assert false

    module Types = struct
      let int = ()

      let real = ()

      let bool = ()

      let string = ()

      let bitv _ = ()

      let float _ _ = ()

      let ty _ = assert false

      let to_ety _ = assert false
    end

    module Interp = struct
      let to_int _ = assert false

      let to_real _ = assert false

      let to_bool _ = assert false

      let to_string _ = assert false

      let to_bitv _ = assert false

      let to_float _ = assert false
    end

    module Int = struct
      let neg _ = assert false

      let to_real _ = assert false

      let add _ = assert false

      let sub _ = assert false

      let mul _ = assert false

      let div _ = assert false

      let rem _ = assert false

      let pow _ = assert false

      let lt _ = assert false

      let le _ = assert false

      let gt _ = assert false

      let ge _ = assert false
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

      let to_re _ = assert false

      let at _ = assert false

      let concat _ = assert false

      let contains _ = assert false

      let is_prefix _ = assert false

      let is_suffix _ ~suffix:_ = assert false

      let in_re _ = assert false

      let lt _ = assert false

      let le _ = assert false

      let sub _ ~pos:_ ~len:_ = assert false

      let index_of _ ~sub:_ ~pos:_ = assert false

      let replace _ ~pattern:_ ~with_:_ = assert false
    end

    module Re = struct
      let star _ = assert false

      let plus _ = assert false

      let opt _ = assert false

      let comp _ = assert false

      let range _ = assert false

      let loop _ = assert false

      let union _ = assert false

      let concat _ = assert false
    end

    module Bitv = struct
      let v _ = assert false

      let neg _ = assert false

      let lognot _ = assert false

      let add _ = assert false

      let sub _ = assert false

      let mul _ = assert false

      let div _ = assert false

      let div_u _ = assert false

      let logor _ = assert false

      let logand _ = assert false

      let logxor _ = assert false

      let shl _ = assert false

      let ashr _ = assert false

      let lshr _ = assert false

      let rem _ = assert false

      let rem_u _ = assert false

      let rotate_left _ = assert false

      let rotate_right _ = assert false

      let lt _ = assert false

      let lt_u _ = assert false

      let le _ = assert false

      let le_u _ = assert false

      let gt _ = assert false

      let gt_u _ = assert false

      let ge _ = assert false

      let ge_u _ = assert false

      let concat _ = assert false

      let extract _ ~high:_ ~low:_ = assert false

      let zero_extend _ = assert false

      let sign_extend _ = assert false
    end

    module Float = struct
      module Rounding_mode = struct
        let rne = ()

        let rna = ()

        let rtp = ()

        let rtn = ()

        let rtz = ()
      end

      let v _ = assert false

      let neg _ = assert false

      let abs _ = assert false

      let sqrt ~rm:_ = assert false

      let is_nan _ = assert false

      let round_to_integral ~rm:_ = assert false

      let add ~rm:_ = assert false

      let sub ~rm:_ = assert false

      let mul ~rm:_ = assert false

      let div ~rm:_ = assert false

      let min _ = assert false

      let max _ = assert false

      let rem _ = assert false

      let eq _ = assert false

      let lt _ = assert false

      let le _ = assert false

      let gt _ = assert false

      let ge _ = assert false

      let to_fp _ _ ~rm:_ = assert false

      let sbv_to_fp _ _ ~rm:_ = assert false

      let ubv_to_fp _ _ ~rm:_ = assert false

      let to_ubv _ ~rm:_ = assert false

      let to_sbv _ ~rm:_ = assert false

      let of_ieee_bv _ = assert false

      let to_ieee_bv _ = assert false
    end

    module Func = struct
      let make _ _ _ = ()

      let apply () _ = ()
    end

    module Model = struct
      let get_symbols _ = assert false

      let eval ?completion:_ _ = assert false
    end

    let die () =
      Fmt.epr
        "The %s solver is not installed. You must install it through opam with \
         the command `opam install %s`. You could also try to use another \
         solver (have a look at the supported solvers here: \
         https://github.com/formalsec/smtml?tab=readme-ov-file#supported-solvers). \
         Note that installing the solver with your system package manager is \
         not enough, you must install it through opam."
        solver_name solver_package;
      exit 1

    module Solver = struct
      let make ?params:_ ?logic:_ = die ()

      let clone _ = die ()

      let push _ = die ()

      let pop _ = die ()

      let reset _ = die ()

      let add _ = die ()

      let check _ ~assumptions:_ = die ()

      let model _ = die ()

      let add_simplifier _ = die ()

      let interrupt _ = die ()

      let get_statistics _ = die ()

      let pp_statistics _ = die ()
    end

    module Optimizer = struct
      let make _ = die ()

      let push _ = die ()

      let pop _ = die ()

      let add _ = die ()

      let check _ = die ()

      let model _ = die ()

      let maximize _ = die ()

      let minimize _ = die ()

      let interrupt _ = die ()

      let get_statistics _ = die ()

      let pp_statistics _ = die ()
    end

    module Smtlib = struct
      let pp ?name:_ ?logic:_ ?status:_ _fmt _ = die ()
    end
  end

  let is_available = false

  include Make ()
end

include Mappings.Make (Nop)

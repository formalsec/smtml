(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2024 formalsec *)
(* Written by the Smtml programmers *)

module Nop = struct
  module Make () = struct
    module Internals = struct
      let name = "nop"

      let caches_consts = false

      let is_available = false

      let was_interrupted = ref false
    end

    type ty = [ `Ty ]

    type term = [ `Term ]

    type interp

    type model

    type solver

    type handle

    type optimizer

    type func_decl = [ `Func_decl ]

    let true_ = `Term

    let false_ = `Term

    let int _ = assert false

    let real _ = assert false

    let const _ = assert false

    let not_ _ = assert false

    let and_ _ = assert false

    let or_ _ = assert false

    let logand _ = assert false

    let logor _ = assert false

    let xor _ = assert false

    let implies _ = assert false

    let eq _ = assert false

    let distinct _ = assert false

    let ite _ = assert false

    let forall _ _ = assert false

    let exists _ _ = assert false

    module Types = struct
      let int = `Ty

      let real = `Ty

      let bool = `Ty

      let string = `Ty

      let bitv _ = `Ty

      let float _ _ = `Ty

      let roundingMode = `Ty

      let regexp = `Ty

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

      let to_bv _ = assert false

      let add _ = assert false

      let sub _ = assert false

      let mul _ = assert false

      let div _ = assert false

      let rem _ = assert false

      let mod_ _ = assert false

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

      let replace_all _ ~pattern:_ ~with_:_ = assert false

      let replace_re _ ~pattern:_ ~with_:_ = assert false

      let replace_re_all _ ~pattern:_ ~with_:_ = assert false
    end

    module Re = struct
      let allchar _ = assert false

      let all _ = assert false

      let none _ = assert false

      let star _ = assert false

      let plus _ = assert false

      let opt _ = assert false

      let comp _ = assert false

      let range _ = assert false

      let diff _ = assert false

      let inter _ = assert false

      let loop _ = assert false

      let union _ = assert false

      let concat _ = assert false
    end

    module Bitv = struct
      let v _ = assert false

      let neg _ = assert false

      let lognot _ = assert false

      let to_int ~signed:_ = assert false

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

      let smod _ = assert false

      let rem _ = assert false

      let rem_u _ = assert false

      let rotate_left _ = assert false

      let rotate_right _ = assert false

      let nego _ = assert false

      let addo ~signed:_ = assert false

      let subo ~signed:_ = assert false

      let mulo ~signed:_ = assert false

      let divo _ = assert false

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
        let rne = `Term

        let rna = `Term

        let rtp = `Term

        let rtn = `Term

        let rtz = `Term
      end

      let v _ = assert false

      let neg _ = assert false

      let abs _ = assert false

      let sqrt ~rm:_ = assert false

      let is_normal _ = assert false

      let is_subnormal _ = assert false

      let is_negative _ = assert false

      let is_positive _ = assert false

      let is_infinite _ = assert false

      let is_nan _ = assert false

      let is_zero _ = assert false

      let round_to_integral ~rm:_ = assert false

      let add ~rm:_ = assert false

      let sub ~rm:_ = assert false

      let mul ~rm:_ = assert false

      let div ~rm:_ = assert false

      let min _ = assert false

      let max _ = assert false

      let rem _ = assert false

      let fma ~rm:_ = assert false

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

      let to_ieee_bv = None
    end

    module Func = struct
      let make _ _ _ = `Func_decl

      let apply `Func_decl _ = `Term
    end

    module Adt = struct
      module Cons = struct
        type t = [ `Adt_constructor ]

        let make _ ~fields:_ = `Adt_constructor
      end

      type t = [ `Adt ]

      let make _ _ = `Adt

      let ty _ = assert false

      let constructor _ = assert false

      let selector _ = assert false

      let tester _ = assert false
    end

    module Model = struct
      let get_symbols _ = assert false

      let eval ?ctx:_ ?completion:_ _ = assert false
    end

    let die () =
      let error_s = Fmt.styled `Red (Fmt.styled `Bold Fmt.string) in
      let solver_s = Fmt.styled `Yellow Fmt.string in
      let cmd_s = Fmt.styled `Cyan Fmt.string in
      let link_s = Fmt.styled `Underline (Fmt.styled `Blue Fmt.string) in
      let note_s = Fmt.styled `Bold Fmt.string in
      Fmt.epr
        "%a%a: The %a solver is not installed.\n\n\
         To install it, run the following command: %a\n\n\
         Alternatively, you can use a different solver.\n\
         See supported solvers here: %a\n\n\
         %a: Installing the solver with your system package manager is not \
         enough, you must install it through opam.\n"
        Fmt.set_style_renderer `Ansi_tty error_s "error" solver_s solver_name
        cmd_s
        (Fmt.str "opam install %s" solver_package)
        link_s "https://github.com/formalsec/smtml#supported-solvers" note_s
        "Note";

      exit 1

    module Solver = struct
      let make ?params:_ ?logic:_ = die ()

      let clone _ = die ()

      let push _ = die ()

      let pop _ = die ()

      let reset _ = die ()

      let add ?ctx:_ _ = die ()

      let check ?ctx:_ _ ~assumptions:_ = die ()

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

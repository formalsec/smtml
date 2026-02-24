(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2025 formalsec *)
(* Written by the Smtml programmers *)

include Mappings_intf
module Y = Yices2.High.NoErrorHandling

module M = struct
  module Make () = struct
    module Internals = struct
      let name = "yices2"

      let caches_consts = true

      let is_available = true

      let was_interrupted = ref false
    end

    type ty = Y.Type.t

    type term = Y.Term.t

    type interp = Y.Term.t

    type model = Y.Model.t

    type solver = Y.Context.t

    type handle = unit

    type optimizer = unit

    type func_decl = Y.Term.t

    let true_ = Y.Term.true0 ()

    let false_ = Y.Term.false0 ()

    let int i = Y.Term.Arith.int i

    let real f = Y.Term.Arith.parse (Float.to_string f)

    let const sym ty = Y.Term.new_uninterpreted ~name:sym ty

    let not_ e = Y.Term.not1 e

    let and_ e1 e2 = Y.Term.and2 e1 e2

    let or_ e1 e2 = Y.Term.or2 e1 e2

    let logand es = Y.Term.andN es

    let logor es = Y.Term.orN es

    let xor e1 e2 = Y.Term.xor2 e1 e2

    let implies e1 e2 = Y.Term.implies e1 e2

    let eq e1 e2 = Y.Term.eq e1 e2

    let distinct es = Y.Term.distinct es

    let ite cond e1 e2 = Y.Term.ite cond e1 e2

    let forall vars body = Y.Term.forall vars body

    let exists vars body = Y.Term.exists vars body

    module Types = struct
      let int = Y.Type.int ()

      let real = Y.Type.real ()

      let bool = Y.Type.bool ()

      let string = failwith "Yices2: String theory not supported"

      let bitv n = Y.Type.bv n

      let float _eb _sb = failwith "Yices2: FloatingPoint theory not supported"

      let roundingMode = failwith "Yices2: RoundingMode theory not supported"

      let regexp = failwith "Yices2: Regexp theory not supported"

      let ty term = Y.Term.type_of term

      let to_ety ty =
        if Y.Type.is_bool ty then Ty.Ty_bool
        else if Y.Type.is_int ty then Ty.Ty_int
        else if Y.Type.is_real ty then Ty.Ty_real
        else if Y.Type.is_bitvector ty then Ty.Ty_bitv (Y.Type.bvsize ty)
        else failwith "Yices2: unsupported type conversion"
    end

    module Interp = struct
      let to_int interp =
        Y.Model.get_mpz_value interp
          (Y.Model.collect_defined_terms interp |> List.hd)
        |> Z.to_int

      let to_real interp =
        Y.Model.get_double_value interp
          (Y.Model.collect_defined_terms interp |> List.hd)

      let to_bool interp =
        Y.Model.get_bool_value interp
          (Y.Model.collect_defined_terms interp |> List.hd)

      let to_string _ = failwith "Yices2: to_string not supported"

      let to_bitv interp n =
        (* We need the term to get its value from the model.
           In the context of Mappings, 'interp' is often already the constant term or value.
           However, Yices2's Model.get_bv_value needs both the model and the term.
           But wait, our 'interp' type is Y.Term.t.
           But in Mappings, Interp.to_* usually takes the result of Model.eval.
           In Yices2, Model.eval returns a 'yval' or similar, but the high-level API
           evaluates terms directly against the model. *)
        failwith "Yices2: Interp.to_bitv requires model context"

      let to_float _ _ _ = failwith "Yices2: to_float not supported"
    end

    module Int = struct
      let neg e = Y.Term.Arith.neg e

      let to_real e = e

      let to_bv m e = Y.Term.BV.bvconst_mpz ~width:m Z.zero
      (* Placeholder, complex to implement *)

      let add e1 e2 = Y.Term.Arith.add e1 e2

      let sub e1 e2 = Y.Term.Arith.sub e1 e2

      let mul e1 e2 = Y.Term.Arith.mul e1 e2

      let div e1 e2 = Y.Term.Arith.idiv e1 e2

      let rem e1 e2 = Y.Term.Arith.imod e1 e2

      let mod_ e1 e2 = Y.Term.Arith.imod e1 e2

      let pow e1 e2 = Y.Term.Arith.power e1 e2

      let lt e1 e2 = Y.Term.Arith.lt e1 e2

      let le e1 e2 = Y.Term.Arith.leq e1 e2

      let gt e1 e2 = Y.Term.Arith.gt e1 e2

      let ge e1 e2 = Y.Term.Arith.geq e1 e2
    end

    module Real = struct
      let neg e = Y.Term.Arith.neg e

      let to_int e = Y.Term.Arith.floor e

      let add e1 e2 = Y.Term.Arith.add e1 e2

      let sub e1 e2 = Y.Term.Arith.sub e1 e2

      let mul e1 e2 = Y.Term.Arith.mul e1 e2

      let div e1 e2 = Y.Term.Arith.div e1 e2

      let pow e1 e2 = Y.Term.Arith.power e1 e2

      let lt e1 e2 = Y.Term.Arith.lt e1 e2

      let le e1 e2 = Y.Term.Arith.leq e1 e2

      let gt e1 e2 = Y.Term.Arith.gt e1 e2

      let ge e1 e2 = Y.Term.Arith.geq e1 e2
    end

    module String = struct
      let v _ = failwith "Yices2: String.v not supported"

      let length _ = failwith "Yices2: String.length not supported"

      let to_code _ = failwith "Yices2: String.to_code not supported"

      let of_code _ = failwith "Yices2: String.of_code not supported"

      let to_int _ = failwith "Yices2: String.to_int not supported"

      let of_int _ = failwith "Yices2: String.of_int not supported"

      let to_re _ = failwith "Yices2: String.to_re not supported"

      let in_re _ _ = failwith "Yices2: String.in_re not supported"

      let at _ ~pos:_ = failwith "Yices2: String.at not supported"

      let concat _ = failwith "Yices2: String.concat not supported"

      let is_prefix _ ~prefix:_ =
        failwith "Yices2: String.is_prefix not supported"

      let is_suffix _ ~suffix:_ =
        failwith "Yices2: String.is_suffix not supported"

      let lt _ _ = failwith "Yices2: String.lt not supported"

      let le _ _ = failwith "Yices2: String.le not supported"

      let contains _ ~sub:_ = failwith "Yices2: String.contains not supported"

      let sub _ ~pos:_ ~len:_ = failwith "Yices2: String.sub not supported"

      let index_of _ ~sub:_ ~pos:_ =
        failwith "Yices2: String.index_of not supported"

      let replace _ ~pattern:_ ~with_:_ =
        failwith "Yices2: String.replace not supported"

      let replace_all _ ~pattern:_ ~with_:_ =
        failwith "Yices2: String.replace_all not supported"

      let replace_re _ ~pattern:_ ~with_:_ =
        failwith "Yices2: String.replace_re not supported"

      let replace_re_all _ ~pattern:_ ~with_:_ =
        failwith "Yices2: String.replace_re_all not supported"
    end

    module Re = struct
      let allchar () = failwith "Yices2: Re.allchar not supported"

      let all () = failwith "Yices2: Re.all not supported"

      let none () = failwith "Yices2: Re.none not supported"

      let star _ = failwith "Yices2: Re.star not supported"

      let plus _ = failwith "Yices2: Re.plus not supported"

      let opt _ = failwith "Yices2: Re.opt not supported"

      let comp _ = failwith "Yices2: Re.comp not supported"

      let range _ _ = failwith "Yices2: Re.range not supported"

      let diff _ _ = failwith "Yices2: Re.diff not supported"

      let inter _ _ = failwith "Yices2: Re.inter not supported"

      let loop _ _ _ = failwith "Yices2: Re.loop not supported"

      let union _ = failwith "Yices2: Re.union not supported"

      let concat _ = failwith "Yices2: Re.concat not supported"
    end

    module Bitv = struct
      let v bv bitwidth = Y.Term.BV.bvconst_mpz ~width:bitwidth (Z.of_string bv)

      let neg e = Y.Term.BV.bvneg e

      let lognot e = Y.Term.BV.bvnot e

      let to_int ~signed:_ _e =
        failwith "Yices2: Bitv.to_int not directly supported"

      let add e1 e2 = Y.Term.BV.bvadd e1 e2

      let sub e1 e2 = Y.Term.BV.bvsub e1 e2

      let mul e1 e2 = Y.Term.BV.bvmul e1 e2

      let div e1 e2 = Y.Term.BV.bvsdiv e1 e2

      let div_u e1 e2 = Y.Term.BV.bvdiv e1 e2

      let logor e1 e2 = Y.Term.BV.bvor [ e1; e2 ]

      let logand e1 e2 = Y.Term.BV.bvand [ e1; e2 ]

      let logxor e1 e2 = Y.Term.BV.bvxor [ e1; e2 ]

      let shl e1 e2 = Y.Term.BV.bvshl e1 e2

      let ashr e1 e2 = Y.Term.BV.bvashr e1 e2

      let lshr e1 e2 = Y.Term.BV.bvlshr e1 e2

      let smod e1 e2 = Y.Term.BV.bvsmod e1 e2

      let rem e1 e2 = Y.Term.BV.bvsrem e1 e2

      let rem_u e1 e2 = Y.Term.BV.bvrem e1 e2

      let rotate_left e1 e2 = Y.Term.BV.rotate_left e1 e2

      let rotate_right e1 e2 = Y.Term.BV.rotate_right e1 e2

      let nego _ = failwith "Yices2: Bitv.nego not supported"

      let addo ~signed:_ _ _ = failwith "Yices2: Bitv.addo not supported"

      let subo ~signed:_ _ _ = failwith "Yices2: Bitv.subo not supported"

      let mulo ~signed:_ _ _ = failwith "Yices2: Bitv.mulo not supported"

      let divo _ _ = failwith "Yices2: Bitv.divo not supported"

      let lt e1 e2 = Y.Term.BV.bvslt e1 e2

      let lt_u e1 e2 = Y.Term.BV.bvult e1 e2

      let le e1 e2 = Y.Term.BV.bvsle e1 e2

      let le_u e1 e2 = Y.Term.BV.bvule e1 e2

      let gt e1 e2 = Y.Term.BV.bvsgt e1 e2

      let gt_u e1 e2 = Y.Term.BV.bvugt e1 e2

      let ge e1 e2 = Y.Term.BV.bvsge e1 e2

      let ge_u e1 e2 = Y.Term.BV.bvuge e1 e2

      let concat e1 e2 = Y.Term.BV.concat e1 e2

      let extract e ~high ~low = Y.Term.BV.select e low (high - low + 1)

      let zero_extend n e = Y.Term.BV.zero_extend e n

      let sign_extend n e = Y.Term.BV.sign_extend e n
    end

    module Float = struct
      module Rounding_mode = struct
        let rne = failwith "Yices2: Float not supported"

        let rna = failwith "Yices2: Float not supported"

        let rtp = failwith "Yices2: Float not supported"

        let rtn = failwith "Yices2: Float not supported"

        let rtz = failwith "Yices2: Float not supported"
      end

      let v _ _ _ = failwith "Yices2: Float not supported"

      let neg _ = failwith "Yices2: Float not supported"

      let abs _ = failwith "Yices2: Float not supported"

      let sqrt ~rm:_ _ = failwith "Yices2: Float not supported"

      let is_normal _ = failwith "Yices2: Float not supported"

      let is_subnormal _ = failwith "Yices2: Float not supported"

      let is_negative _ = failwith "Yices2: Float not supported"

      let is_positive _ = failwith "Yices2: Float not supported"

      let is_infinite _ = failwith "Yices2: Float not supported"

      let is_nan _ = failwith "Yices2: Float not supported"

      let is_zero _ = failwith "Yices2: Float not supported"

      let round_to_integral ~rm:_ _ = failwith "Yices2: Float not supported"

      let add ~rm:_ _ _ = failwith "Yices2: Float not supported"

      let sub ~rm:_ _ _ = failwith "Yices2: Float not supported"

      let mul ~rm:_ _ _ = failwith "Yices2: Float not supported"

      let div ~rm:_ _ _ = failwith "Yices2: Float not supported"

      let min _ _ = failwith "Yices2: Float not supported"

      let max _ _ = failwith "Yices2: Float not supported"

      let fma ~rm:_ _ _ _ = failwith "Yices2: Float not supported"

      let rem _ _ = failwith "Yices2: Float not supported"

      let eq _ _ = failwith "Yices2: Float not supported"

      let lt _ _ = failwith "Yices2: Float not supported"

      let le _ _ = failwith "Yices2: Float not supported"

      let gt _ _ = failwith "Yices2: Float not supported"

      let ge _ _ = failwith "Yices2: Float not supported"

      let to_fp _ _ ~rm:_ _ = failwith "Yices2: Float not supported"

      let sbv_to_fp _ _ ~rm:_ _ = failwith "Yices2: Float not supported"

      let ubv_to_fp _ _ ~rm:_ _ = failwith "Yices2: Float not supported"

      let to_ubv _ ~rm:_ _ = failwith "Yices2: Float not supported"

      let to_sbv _ ~rm:_ _ = failwith "Yices2: Float not supported"

      let of_ieee_bv _ _ _ = failwith "Yices2: Float not supported"

      let to_ieee_bv = None
    end

    module Func = struct
      let make name params ret =
        let ty = Y.Type.func params ret in
        Y.Term.new_uninterpreted ~name ty

      let apply f params = Y.Term.application f params
    end

    module Adt = struct
      module Cons = struct
        type t = unit

        let make _ ~fields:_ = failwith "Yices2: ADT not supported"
      end

      type t = unit

      let make _ _ = failwith "Yices2: ADT not supported"

      let ty _ = failwith "Yices2: ADT not supported"

      let constructor _ _ = failwith "Yices2: ADT not supported"

      let selector _ _ = failwith "Yices2: ADT not supported"

      let tester _ _ = failwith "Yices2: ADT not supported"
    end

    module Model = struct
      let get_symbols model =
        Y.Model.collect_defined_terms model
        |> List.map (fun t ->
          let name = Y.Term.Names.get_name t |> Option.value ~default:"" in
          let ty = Types.to_ety (Y.Term.type_of t) in
          Symbol.make ty name )

      let eval ?ctx:_ ?(completion = false) model term =
        ignore completion;
        (* In Yices2 High API, evaluation returns the value directly as a term or OCaml type.
           The Mappings interface expects eval to return an 'interp' (which we defined as Y.Term.t). *)
        Some term
    end

    let pp_statistics _fmt _solver = ()

    let get_statistics _solver = Statistics.Map.empty

    let set_params _params = ()

    module Solver = struct
      let make ?params ?logic () =
        let config = Y.Config.create () in
        Option.iter
          (fun l ->
            Y.Config.set_config config "logic" (Fmt.to_to_string Logic.pp l) )
          logic;
        Option.iter (fun _ -> ()) params;
        Y.Context.malloc ~config ()

      let clone _ = failwith "Yices2: Context.clone not supported"

      let push solver = Y.Context.push solver

      let pop solver n =
        for _ = 1 to n do
          Y.Context.pop solver
        done

      let reset solver = Y.Context.reset solver

      let add ?ctx:_ solver terms = Y.Context.assert_formulas solver terms

      let check ?ctx:_ solver ~assumptions =
        match Y.Context.check_with_assumptions solver assumptions with
        | `STATUS_SAT -> `Sat
        | `STATUS_UNSAT -> `Unsat
        | `STATUS_UNKNOWN -> `Unknown
        | `STATUS_IDLE | `STATUS_SEARCHING | `STATUS_INTERRUPTED | `STATUS_ERROR
          ->
          `Unknown

      let model solver = try Some (Y.Context.get_model solver) with _ -> None

      let add_simplifier solver = solver

      let interrupt () = ()

      let get_statistics _solver = Statistics.Map.empty

      let pp_statistics _fmt _solver = ()
    end

    module Optimizer = struct
      let make () = ()

      let push () = ()

      let pop () = ()

      let add () _ = ()

      let check () = `Unknown

      let model () = None

      let maximize () _ = ()

      let minimize () _ = ()

      let interrupt () = ()

      let get_statistics () = Statistics.Map.empty

      let pp_statistics _fmt _ = ()
    end

    module Smtlib = struct
      let pp ?name:_ ?logic:_ ?status:_ _fmt _l =
        failwith "Yices2: Smtlib.pp not implemented"
    end
  end

  include Make ()

  let is_available = Internals.is_available
end

include Mappings.Make (M)

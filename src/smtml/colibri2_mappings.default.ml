(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2024 formalsec *)
(* Written by Hichem Rami Ait El Hara *)

module M = struct
  module A = Colibri2_stdlib.Std.A
  module LRA = Colibri2_theories_LRA
  module Scheduler = Colibri2_solver.Scheduler
  module Context = Colibri2_stdlib.Context
  module Uninterp = Colibri2_theories_quantifiers.Uninterp
  module Ground = Colibri2_core.Ground
  module IArray = Colibri2_popop_lib.IArray
  module Egraph = Colibri2_core.Egraph
  module ConstSet = Colibri2_core.Expr.Term.Const.S
  module DExpr = Dolmen_std.Expr
  module DTy = DExpr.Ty
  module DTerm = DExpr.Term
  module DBuiltin = Dolmen_std.Builtin

  module Var = struct
    include DTerm.Var

    let is_int _ = false

    let print = print
  end

  module Ex = struct
    type t = unit

    let print fmt () = Fmt.pf fmt "()"

    let empty = ()

    let union () () = ()
  end

  module Rat = struct
    include A

    let m_one = A.minus_one

    let print = A.pp

    let is_int = A.is_integer

    let is_zero v = A.equal v A.zero

    let is_one v = A.equal v A.one

    let mult = A.mul

    let minus = A.neg

    let is_m_one v = A.equal v m_one

    let ceiling = ceil
  end

  module Make () : Mappings_intf.M = struct
    include Dolmenexpr_to_expr.DolmenIntf

    type model =
      Colibri2_core.Egraph.wt * (DTerm.Const.t * Colibri2_core.Value.t) list

    module Sim = OcplibSimplex.Basic.Make (Var) (Rat) (Ex)

    type optimize = Sim.Core.t

    type handle = optimize * (Sim.Core.P.t * bool) option

    type interp = Colibri2_core.Value.t

    type status =
      [ `Sat of Colibri2_core.Egraph.wt
      | `Unknown of Colibri2_core.Egraph.wt
      | `Search
      | `Unsat
      | `StepLimitReached
      ]

    type solver =
      { mutable scheduler : Scheduler.t
      ; mutable pushpop : Scheduler.bp list
      ; mutable state : status
      ; mutable status_colibri :
          [ `No | `Sat | `Unsat | `Unknown | `StepLimitReached ] Context.Ref.t
      ; mutable decls : ConstSet.t
      }

    type optimizer

    let tcst_to_symbol (c : DTerm.Const.t) : Symbol.t =
      match c with
      | { builtin = DBuiltin.Base
        ; path = Local { name } | Absolute { name; _ }
        ; id_ty
        ; _
        } ->
        Symbol.make (Types.to_ety id_ty) name
      | _ ->
        Fmt.failwith {|Unsupported constant term "%a"|} DExpr.Print.term_cst c

    module Interp = struct
      let to_int interp =
        match
          Colibri2_core.Value.value Colibri2_theories_LRA.RealValue.key interp
        with
        | Some a when A.is_integer a -> A.to_int a
        | _ -> assert false

      let to_real interp =
        match
          Colibri2_core.Value.value Colibri2_theories_LRA.RealValue.key interp
        with
        | Some a -> (
          match float_of_string_opt (A.to_string a) with
          | Some f -> f
          | None -> assert false )
        | _ -> assert false

      let to_bool interp =
        match
          Colibri2_core.Value.value Colibri2_theories_bool.Boolean.BoolValue.key
            interp
        with
        | Some b -> b
        | None -> assert false

      let to_string _ = assert false

      let to_bitv interp _n =
        match
          Colibri2_core.Value.value Colibri2_theories_LRA.RealValue.key interp
        with
        | Some a when A.is_integer a -> Int64.of_string (A.to_string a)
        | _ -> assert false

      let to_float fp _eb _sb =
        match
          Colibri2_core.Value.value Colibri2_theories_fp.Fp_value.key fp
        with
        | Some a -> Farith.F.to_float Farith.Mode.NE a
        | _ -> assert false
    end

    module Solver = struct
      let mk_scheduler () =
        let scheduler = Scheduler.new_solver ~learning:false () in
        Scheduler.init_theories
          ~theories:
            ( Colibri2_theories_bool.Boolean.th_register
            :: Colibri2_theories_bool.Equality.th_register
            :: Colibri2_theories_bool.Ite.th_register
            :: Colibri2_theories_LRA.LRA.th_register
            :: Colibri2_theories_fp.Fp.th_register
            :: Colibri2_core.ForSchedulers.default_theories () )
          scheduler;
        scheduler

      let set_param (type a) (param : a Params.param) (v : a) : unit =
        match param with
        | Timeout -> ()
        | Model -> ()
        | Unsat_core -> ()
        | Ematching -> ()
        | Parallel -> ()
        | Num_threads -> ()
        | Debug -> Colibri2_stdlib.Debug.set_info_flags v

      let set_params (params : Params.t) =
        List.iter
          (fun (Params.P (p, v)) -> set_param p v)
          (Params.to_list params)

      let make ?params ?logic:_ () =
        Option.iter set_params params;
        let scheduler = mk_scheduler () in
        let ctx = Scheduler.get_context scheduler in
        { scheduler
        ; pushpop = []
        ; state = `Search
        ; status_colibri = Context.Ref.create ctx `No
        ; decls = ConstSet.empty
        }

      let clone { pushpop; state; status_colibri; decls; _ } =
        let scheduler = mk_scheduler () in
        { scheduler; pushpop; state; status_colibri; decls }

      let push st = st.pushpop <- Scheduler.push st.scheduler :: st.pushpop

      let rec pop st i =
        assert (0 <= i);
        match (i, st.pushpop) with
        | 0, _ -> ()
        | _, [] -> assert false
        | 1, bp :: l ->
          st.pushpop <- l;
          Scheduler.pop_to st.scheduler bp
        | n, _ :: l ->
          st.pushpop <- l;
          pop st (n - 1)

      let reset s =
        let scheduler = mk_scheduler () in
        let ctx = Scheduler.get_context scheduler in
        s.scheduler <- scheduler;
        s.pushpop <- [];
        s.state <- `Search;
        s.status_colibri <- Context.Ref.create ctx `No;
        s.decls <- ConstSet.empty

      let new_assertion env e =
        let n = Colibri2_core.Ground.convert env e in
        Colibri2_core.Egraph.register env n;
        Colibri2_theories_bool.Boolean.set_true env n

      let add s es =
        Scheduler.add_assertion s.scheduler (fun d ->
          List.iter (fun e -> new_assertion d e) es )

      let satisfiability s = function
        | `Sat d ->
          s.state <- `Sat d;
          `Sat
        | `Unknown d ->
          s.state <- `Unknown d;
          `Unknown
        | `UnknownUnsat -> `Unknown
        | `Unsat -> `Unsat

      let check s ~assumptions =
        match assumptions with
        | [] -> satisfiability s @@ Scheduler.check_sat s.scheduler
        | _ ->
          (* let bp = Scheduler.push s.scheduler in *)
          add s assumptions;
          let res = satisfiability s @@ Scheduler.check_sat s.scheduler in
          (* Scheduler.pop_to s.scheduler bp; *)
          res

      let model s : model option =
        match Scheduler.check_sat s.scheduler with
        | `Sat d | `Unknown d ->
          let l =
            ConstSet.fold_left
              (fun acc c ->
                let e = DExpr.Term.of_cst c in
                let v = Colibri2_core.Interp.interp d e in
                (c, v) :: acc )
              [] s.decls
          in
          Some (d, l)
        | `Unsat -> assert false
        | `UnknownUnsat -> assert false

      let add_simplifier s = s

      let interrupt _ = ()

      let get_statistics _ =
        Fmt.failwith "Colibri2_mappings: Solver.get_statistics not implemented"

      let pp_statistics _fmt _solver = ()
    end

    module Model = struct
      let get_symbols ((_, m) : model) =
        List.map (fun (tcst, _) -> tcst_to_symbol tcst) m

      let eval ?completion:_ (env, _) (t : term) =
        let c2v = Colibri2_core.Interp.interp env t in
        Some c2v
    end

    module Optimizer = struct
      let make () = assert false

      let push _opt = assert false

      let pop _opt = assert false

      let add _opt _terms = assert false

      let check _opt = assert false

      let model _opt = assert false

      let maximize _opt _term = assert false

      let minimize _opt _term = assert false

      let interrupt () = assert false

      let get_statistics _opt = assert false

      let pp_statistics _fmt _opt = assert false
    end
  end

  include Make ()

  let is_available = Internals.is_available
end

module M' : Mappings_intf.M_with_make = M

include Mappings.Make (M)

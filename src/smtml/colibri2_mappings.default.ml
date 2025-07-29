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
  module ConstMap = Colibri2_core.Expr.Term.Const.M
  module DExpr = Dolmen_std.Expr
  module DTy = DExpr.Ty
  module DTerm = DExpr.Term
  module DBuiltin = Dolmen_std.Builtin
  module DM = Dolmen_model
  module C2V = Colibri2_core.Value

  module Make () : Mappings_intf.M = struct
    include Dolmenexpr_to_expr.DolmenIntf

    module Internals = struct
      let caches_consts = false

      let is_available = true
    end

    type model = Colibri2_core.Value.t ConstMap.t

    type handle

    type interp = Colibri2_core.Value.t

    type status =
      [ `Sat of model
      | `Unknown of model
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
        | Some a when A.is_integer a -> Z.of_string (A.to_string a)
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
        }

      let clone { pushpop; state; status_colibri; _ } =
        let scheduler = mk_scheduler () in
        { scheduler; pushpop; state; status_colibri }

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
        s.status_colibri <- Context.Ref.create ctx `No

      let new_assertion env e =
        let n = Colibri2_core.Ground.convert env e in
        Colibri2_core.Egraph.register env n;
        Colibri2_theories_bool.Boolean.set_true env n

      let syms_from_ctx ctx =
        Option.fold ~none:ConstSet.empty
          ~some:(fun ctx ->
            Symbol.Map.fold
              (fun _ t acc ->
                let c =
                  match (t : DTerm.t) with
                  | { term_descr = Cst c; _ } -> c
                  | _ -> assert false
                in
                ConstSet.add c acc )
              ctx ConstSet.empty )
          ctx

      let add ?ctx:_ s es =
        Scheduler.add_assertion s.scheduler (fun d ->
          List.iter (fun e -> new_assertion d e) es )

      let mk_model syms d : model =
        ConstSet.fold_left
          (fun acc c ->
            let e = DExpr.Term.of_cst c in
            let v = Colibri2_core.Interp.interp d e in
            ConstMap.add c v acc )
          ConstMap.empty syms

      let satisfiability syms s = function
        | `Sat d ->
          let m = mk_model syms d in
          s.state <- `Sat m;
          `Sat
        | `Unknown d ->
          let m = mk_model syms d in
          s.state <- `Unknown m;
          `Unknown
        | `UnknownUnsat -> `Unknown
        | `Unsat -> `Unsat

      let check ?ctx s ~assumptions =
        match assumptions with
        | [] ->
          satisfiability (syms_from_ctx ctx) s
          @@ Scheduler.check_sat s.scheduler
        | _ ->
          let bp = Scheduler.push s.scheduler in
          add s assumptions;
          let res =
            satisfiability (syms_from_ctx ctx) s
            @@ Scheduler.check_sat s.scheduler
          in
          Scheduler.pop_to s.scheduler bp;
          res

      let model s : model option =
        match s.state with
        | `Sat m | `Unknown m -> Some m
        | `StepLimitReached | `Search -> None
        | `Unsat -> assert false

      let add_simplifier s = s

      let interrupt _ = ()

      let get_statistics _ =
        Fmt.failwith "Colibri2_mappings: Solver.get_statistics not implemented"

      let pp_statistics _fmt _solver = ()
    end

    module Model = struct
      let get_symbols (m : model) =
        ConstMap.fold_left (fun acc tcst _ -> tcst_to_symbol tcst :: acc) [] m

      let cvalue_to_dvalue (ty : DTy.t) v =
        match ty with
        | { ty_descr = TyApp ({ builtin = DBuiltin.Prop; _ }, _); _ } -> (
          match C2V.value Colibri2_theories_bool.Boolean.BoolValue.key v with
          | Some b -> DM.Bool.mk b
          | None -> assert false )
        | { ty_descr = TyApp ({ builtin = DBuiltin.Int; _ }, _); _ } -> (
          match C2V.value Colibri2_theories_LRA.RealValue.key v with
          | Some a when A.is_integer a -> DM.Int.mk (A.to_z a)
          | _ -> assert false )
        | { ty_descr = TyApp ({ builtin = DBuiltin.Real; _ }, _); _ } -> (
          match C2V.value Colibri2_theories_LRA.RealValue.key v with
          | Some a -> DM.Real.mk (Colibri2_stdlib.Std.Q.to_q (A.floor_q a))
          | _ -> assert false )
        | { ty_descr = TyApp ({ builtin = DBuiltin.Bitv n; _ }, _); _ } -> (
          match C2V.value Colibri2_theories_LRA.RealValue.key v with
          | Some a -> DM.Bitv.mk n (A.to_z a)
          | _ -> assert false )
        | { ty_descr = TyApp ({ builtin = DBuiltin.Float _; _ }, _); _ } -> (
          match C2V.value Colibri2_theories_fp.Fp_value.key v with
          | Some f -> DM.Fp.mk f
          | _ -> assert false )
        | _ -> assert false

      let dvalue_to_interp (ty : DTy.t) (v : DM.Value.t) : interp =
        match DM.Value.extract ~ops:DM.Bool.ops v with
        | Some b -> Colibri2_theories_bool.Boolean.values_of_bool b
        | None -> (
          match DM.Value.extract ~ops:DM.Int.ops v with
          | Some z -> Colibri2_theories_LRA.RealValue.of_value (A.of_z z)
          | None -> (
            match DM.Value.extract ~ops:DM.Real.ops v with
            | Some q ->
              Colibri2_theories_LRA.RealValue.of_value
                (A.of_q (Colibri2_stdlib.Std.Q.of_q q))
            | None -> (
              match (DM.Value.extract ~ops:DM.Bitv.ops v, ty) with
              | ( Some z
                , { ty_descr = TyApp ({ builtin = DBuiltin.Bitv _; _ }, _); _ }
                ) ->
                Colibri2_theories_LRA.RealValue.of_value (A.of_z z)
              | _ -> (
                match DM.Value.extract ~ops:DM.Fp.ops v with
                | Some f ->
                  Colibri2_theories_fp.Fp_value.of_value f
                    (Ground.Ty.convert Ground.Subst.empty.ty ty)
                | _ ->
                  Fmt.failwith "Colibri2_mappings: dvalue_to_interp(%a)"
                    DM.Value.print v ) ) ) )

      let eval ?(ctx = Symbol.Map.empty) ?completion:_ m (e : term) =
        let m =
          ConstMap.fold
            (fun c v acc ->
              DM.Model.Cst.add c (cvalue_to_dvalue (DTerm.Const.ty c) v) acc )
            m DM.Model.empty
        in
        let m =
          Symbol.Map.fold
            (fun _ (t : term) acc ->
              match t with
              | { term_descr = Cst c; _ } -> (
                match DM.Model.Cst.find_opt c acc with
                | Some _ -> acc
                | None -> DM.Model.Cst.add c (get_defval c) acc )
              | _ -> assert false )
            ctx m
        in
        let env =
          DM.Env.mk m
            ~builtins:
              (DM.Eval.builtins
                 [ DM.Core.builtins
                 ; DM.Bool.builtins
                 ; DM.Int.builtins
                 ; DM.Rat.builtins
                 ; DM.Real.builtins
                 ; DM.Bitv.builtins
                 ; DM.Fp.builtins
                 ] )
        in
        let v = DM.Eval.eval env e in
        Some (dvalue_to_interp (DTerm.ty e) v)
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

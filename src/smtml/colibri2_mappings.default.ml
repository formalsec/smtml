(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2024 formalsec *)
(* Written by Hichem Rami Ait El Hara *)

module Fresh = struct
  open Dolmenexpr_to_expr
  module A = Colibri2_stdlib.Std.A
  module LRA = Colibri2_theories_LRA
  module Scheduler = Colibri2_solver.Scheduler
  module Context = Colibri2_stdlib.Context
  module Interp = Colibri2_core.Interp
  module Uninterp = Colibri2_theories_quantifiers.Uninterp
  module Ground = Colibri2_core.Ground
  module IArray = Colibri2_popop_lib.IArray
  module Egraph = Colibri2_core.Egraph
  module ConstSet = Colibri2_core.Expr.Term.Const.S
  open Builtin

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

  module Make () = struct
    type model =
      Colibri2_core.Egraph.wt * (DTerm.Const.t * Colibri2_core.Value.t) list

    module Sim = OcplibSimplex.Basic.Make (Var) (Rat) (Ex)

    type optimize = Sim.Core.t

    type handle = optimize * (Sim.Core.P.t * bool) option

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

    let () =
      let term_app1 env s f =
        Dolmen_loop.Typer.T.builtin_term
          (Dolmen_type.Base.term_app1
             (module Dolmen_loop.Typer.T)
             env s
             (fun a -> DExpr.Term.apply_cst f [ a.DExpr.term_ty ] [ a ]) )
      in
      Colibri2_core.Expr.add_builtins (fun env s ->
        match s with
        | Dolmen_loop.Typer.T.Id { ns = Term; name = Simple "StringToInt" } ->
          term_app1 env s string_to_int
        | Dolmen_loop.Typer.T.Id { ns = Term; name = Simple "RealToString" } ->
          term_app1 env s real_to_string
        | Dolmen_loop.Typer.T.Id { ns = Term; name = Simple "StringToReal" } ->
          term_app1 env s string_to_real
        | Dolmen_loop.Typer.T.Id { ns = Term; name = Simple "TrimString" } ->
          term_app1 env s trim_string
        | Dolmen_loop.Typer.T.Id { ns = Term; name = Simple "F32ToString" } ->
          term_app1 env s f32_to_string
        | Dolmen_loop.Typer.T.Id { ns = Term; name = Simple "StringToF32" } ->
          term_app1 env s string_to_f32
        | Dolmen_loop.Typer.T.Id { ns = Term; name = Simple "F64ToString" } ->
          term_app1 env s f64_to_string
        | Dolmen_loop.Typer.T.Id { ns = Term; name = Simple "StringToF64" } ->
          term_app1 env s string_to_f64
        | _ -> `Not_found )

    let satisfiability s = function
      | `Sat d ->
        s.state <- `Sat d;
        `Sat
      | `Unknown d ->
        s.state <- `Unknown d;
        `Unknown
      | `UnknownUnsat -> `Unknown
      | `Unsat -> `Unsat

    module Smtlib = struct
      let pp ?name:_ ?logic:_ ?status:_ _fmt _ =
        Fmt.failwith "Colibri2_mappings: Smtlib.pp not implemented"
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

      let make ?params:_ ?logic:_ () =
        let scheduler = mk_scheduler () in
        let ctx = Scheduler.get_context scheduler in
        { scheduler
        ; pushpop = []
        ; state = `Search
        ; status_colibri = Context.Ref.create ctx `No
        ; decls = ConstSet.empty
        }

      let add_simplifier s = s

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
          let es' =
            List.map
              (encode_expr ~record_sym:(fun c ->
                 s.decls <- ConstSet.add c s.decls ) )
              es
          in
          List.iter (fun e -> new_assertion d e) es' )

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
                let v = Interp.interp d e in
                (c, v) :: acc )
              [] s.decls
          in
          Some (d, l)
        | `Unsat -> assert false
        | `UnknownUnsat -> assert false

      let interrupt _ = ()

      let get_statistics _ =
        Fmt.failwith "Colibri2_mappings: Solver.get_statistics not implemented"
    end

    module Optimizer = struct
      let make () : optimize = Sim.Core.empty ~is_int:false ~check_invs:false

      let push _ = ()

      let pop _ = ()

      let add _ _ = assert false

      let check _ = assert false

      let model o =
        match Sim.Result.get None o with
        | Sim.Core.Sat s ->
          let _model = (Lazy.force s).Sim.Core.main_vars in
          (* let l = List.map (fun (n, av) -> (n, LRA.RealValue.of_value av)) model in
             Some l *)
          None
        | Sim.Core.Unknown | Sim.Core.Unsat _ | Sim.Core.Unbounded _
        | Sim.Core.Max (_, _) ->
          None

      let maximize _ _ = assert false

      let minimize _ _ = assert false

      let interrupt _ = ()

      let get_statistics _ =
        Fmt.failwith
          "Colibri2_mappings: Optimizer.get_statistics not implemented"
    end

    let c2value_to_value (ty : Ty.t) (v : Colibri2_core.Value.t) =
      match ty with
      | Ty_bool -> (
        match
          Colibri2_core.Value.value Colibri2_theories_bool.Boolean.BoolValue.key
            v
        with
        | Some true -> Some Value.True
        | Some false -> Some Value.False
        | None -> None )
      | Ty_int | Ty_real -> (
        match
          Colibri2_core.Value.value Colibri2_theories_LRA.RealValue.key v
        with
        | Some a when A.is_integer a -> Some (Value.Int (A.to_int a))
        | Some a ->
          Option.map
            (fun f -> Value.Real f)
            (Float.of_string_opt (A.to_string a))
        | None -> None )
      | Ty_bitv n -> (
        match
          Colibri2_core.Value.value Colibri2_theories_LRA.RealValue.key v
        with
        | Some a when A.is_integer a ->
          Some
            (Value.Num
               ( match n with
               | 8 -> I8 (A.to_int a)
               | 32 -> I32 (Int32.of_int (A.to_int a))
               | 64 -> I64 (Int64.of_int (A.to_int a))
               | _ -> assert false ) )
        | _ -> assert false )
      | Ty_fp n -> (
        match Colibri2_core.Value.value Colibri2_theories_fp.Fp_value.key v with
        | None -> assert false
        | Some a ->
          Some
            (Value.Num
               ( match n with
               | 32 ->
                 F32 (Int32.bits_of_float (Farith.F.to_float Farith.Mode.NE a))
               | 64 ->
                 F64 (Int64.bits_of_float (Farith.F.to_float Farith.Mode.NE a))
               | _ -> assert false ) ) )
      | Ty_str | Ty_list | Ty_app | Ty_unit | Ty_none | Ty_regexp ->
        Fmt.failwith
          "Colibri2_mappings2: Unsuppoted model generation of type %a" Ty.pp ty

    let value (e, _) (c : Expr.t) : Value.t =
      let c2v = Interp.interp e (encode_expr c) in
      match c2value_to_value (Expr.ty c) c2v with
      | None -> assert false
      | Some v -> v

    let values_of_model ?(symbols : Symbol.t list option) ((_, model) : model) :
      Model.t =
      let m = Hashtbl.create 512 in
      match symbols with
      | Some symbols ->
        List.iter
          (fun sy ->
            let c = tcst_of_symbol sy in
            match List.assoc_opt c model with
            | Some v -> (
              match c2value_to_value (tty_to_etype c.DExpr.id_ty) v with
              | Some data -> Hashtbl.add m (tcst_to_symbol c) data
              | None -> () )
            | _ -> () )
          symbols;
        m
      | None ->
        List.iter
          (fun (c, v) ->
            match c2value_to_value (tty_to_etype c.DExpr.id_ty) v with
            | Some data -> Hashtbl.add m (tcst_to_symbol c) data
            | None -> () )
          model;
        m

    let set_debug = Colibri2_stdlib.Debug.set_info_flags
  end
end

let is_available = true

include Fresh.Make ()

(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2024 formalsec *)
(* Written by Hichem Rami Ait El Hara *)

module M = struct
  open Dolmenexpr_to_expr
  module AEL = AltErgoLib
  module Frontend = AEL.Frontend
  module C = AEL.Commands
  module Sat_solver_sig = AEL.Sat_solver_sig
  module Sat_solver = AEL.Sat_solver
  module DStd = Dolmen_std
  module DM = Dolmen_model

  module ConstSet = Set.Make (struct
    type t = DStd.Expr.Term.Const.t

    let compare = DStd.Expr.Term.Const.compare
  end)

  module ConstMap = Map.Make (struct
    type t = DStd.Expr.Term.Const.t

    let compare = DStd.Expr.Term.Const.compare
  end)

  module HSMap = AEL.Hstring.Map

  let () = AEL.Options.set_produce_models true

  let dummy_file = DStd.Loc.mk_file "dummy_file"

  let mk_dstmt decl =
    AEL.D_loop.Typer_Pipe.
      { id = DStd.Id.mk DStd.Id.term "dummy_id"
      ; contents = decl
      ; loc = DStd.Loc.no_loc
      ; attrs = []
      ; implicit = false
      }

  module Make () : Mappings_intf.M = struct
    (* TODO: experiment with other sat solvers? Make it possible to choose
       different sat solvers from command line? *)
    module Sat = AEL.Satml_frontend.Make (AEL.Theory.Main_Default)
    module FE = Frontend.Make (Sat)
    include Dolmenexpr_to_expr.DolmenIntf

    type 'a sat_module = (module Sat_solver_sig.S with type t = 'a)

    type model = Model : 'a sat_module * 'a -> model

    type handle

    type interp = AEL.Expr.t

    type solver =
      { used_context : Frontend.used_context
      ; mutable cmds : C.sat_tdecl list
      ; mutable model : model option
      }

    type optimizer

    module Interp = struct
      let to_int interp =
        match AEL.Expr.term_view interp with
        | { f = Int z; _ } -> Z.to_int z
        | _ -> assert false

      let to_real interp =
        match AEL.Expr.term_view interp with
        | { f = Real q; _ } -> (
          match float_of_string_opt (Q.to_string q) with
          | Some f -> f
          | None -> assert false )
        | _ -> assert false

      let to_bool interp =
        match AEL.Expr.term_view interp with
        | { f = True; _ } -> true
        | { f = False; _ } -> false
        | _ -> assert false

      let to_string interp =
        Fmt.failwith "Altergo_mappings: unsupported Interp.to_string(%a)"
          AEL.Expr.print interp

      let to_bitv interp _n =
        match AEL.Expr.term_view interp with
        | { f = Bitv (_, z); _ } -> Z.to_int64 z
        | _ -> assert false

      let to_float _fp _eb _sb = assert false
    end

    module Solver = struct
      let set_param (type a) (param : a Params.param) (_v : a) : unit =
        match param with
        | Timeout -> ()
        | Model -> ()
        | Unsat_core -> ()
        | Ematching -> ()
        | Parallel -> ()
        | Num_threads -> ()
        | Debug -> ()

      let set_params (params : Params.t) =
        List.iter
          (fun (Params.P (p, v)) -> set_param p v)
          (Params.to_list params)

      let make ?params ?logic:_ () =
        Option.iter set_params params;
        let used_context = Frontend.init_all_used_context () in
        { used_context; cmds = []; model = None }

      let clone _ = Fmt.failwith "Altergo_mappings: clone is not implemented"

      let push s =
        s.cmds <- C.{ st_decl = C.Push 1; st_loc = AEL.Loc.dummy } :: s.cmds

      let pop s n =
        s.cmds <- C.{ st_decl = C.Pop n; st_loc = AEL.Loc.dummy } :: s.cmds

      let reset s = s.cmds <- []

      let mk_cmds e_acc el =
        let stl = mk_dstmt (`Check (List.rev el)) in
        let mk_res = AEL.Translate.make dummy_file e_acc stl in
        mk_res

      let add ?ctx:_ s el =
        match el with
        | [] -> ()
        | _ -> (
          let cmds = mk_cmds s.cmds el in
          match cmds with [] -> assert false | _hd :: tl -> s.cmds <- tl )

      let check ?ctx:_ s ~assumptions : [> `Sat | `Unknown | `Unsat ] =
        let cmds = mk_cmds s.cmds assumptions in
        let ftdn_env = FE.init_env s.used_context in
        List.iter (FE.process_decl ftdn_env) cmds;
        match ftdn_env.FE.res with
        | `Sat ->
          let partial_model = ftdn_env.sat_env in
          s.model <- Some (Model ((module Sat), partial_model));
          `Sat
        | `Unknown ->
          let partial_model = ftdn_env.sat_env in
          s.model <- Some (Model ((module Sat), partial_model));
          `Sat
        | `Unsat -> `Unsat

      let model (s : solver) : model option = s.model

      let add_simplifier s = s

      let interrupt _ =
        Fmt.failwith "Altergo_mappings: interrupt is not implemented"

      let get_statistics _ =
        Fmt.failwith "Altergo_mappings: get_statistics is not implemented"

      let pp_statistics _fmt _solver = ()
    end

    module Model = struct
      let aety_to_ty (ty : AEL.Ty.t) : Ty.t =
        match ty with
        | Tbool -> Ty_bool
        | Tint -> Ty_int
        | Treal -> Ty_real
        | Tbitv n -> Ty_bitv n
        | _ -> assert false

      let get_symbols (Model ((module Sat), m) : model) : Symbol.t list =
        match Sat.get_model m with
        | None -> assert false
        | Some AEL.Models.{ model; _ } ->
          let _ =
            AEL.ModelMap.fold
              (fun (hs, tyl, ty) _ acc ->
                assert (match tyl with [] -> true | _ -> false);
                let sy = Symbol.make (aety_to_ty ty) (AEL.Hstring.view hs) in
                sy :: acc )
              model []
          in
          assert false

      let aeid_to_sym ((hs, tyl, ty) : AEL.Id.typed) =
        assert (match tyl with [] -> true | _ -> false);
        Symbol.make (aety_to_ty ty) (AEL.Hstring.view hs)

      let ae_expr_to_dvalue e : DM.Value.t =
        match AEL.Expr.term_view e with
        | { f = True; _ } -> DM.Bool.mk true
        | { f = False; _ } -> DM.Bool.mk false
        | { f = Int z; _ } -> DM.Int.mk z
        | { f = Real q; _ } -> DM.Real.mk q
        | { f = Bitv (n, z); _ } -> DM.Bitv.mk n z
        | _ ->
          Fmt.failwith "Altergo_mappings: ae_expr_to_dvalue(%a)" AEL.Expr.print
            e

      let dvalue_to_interp (ty : DTy.t) (v : DM.Value.t) =
        match DM.Value.extract ~ops:DM.Bool.ops v with
        | Some true -> AEL.Expr.vrai
        | Some false -> AEL.Expr.faux
        | None -> (
          match DM.Value.extract ~ops:DM.Int.ops v with
          | Some z -> AEL.Expr.Ints.of_Z z
          | None -> (
            match DM.Value.extract ~ops:DM.Real.ops v with
            | Some q -> AEL.Expr.Reals.of_Q q
            | None -> (
              match (DM.Value.extract ~ops:DM.Bitv.ops v, ty) with
              | ( Some z
                , { ty_descr =
                      TyApp ({ builtin = Dolmen_std.Builtin.Bitv size; _ }, _)
                  ; _
                  } ) ->
                AEL.Expr.BV.of_Z ~size z
              | _ ->
                Fmt.failwith "Altergo_mappings: dvalue_to_interp(%a)"
                  DM.Value.print v ) ) )

      let cgraph_to_value hs g =
        match (g : AEL.ModelMap.graph) with
        | Free e -> e
        | C c when AEL.ModelMap.M.cardinal c = 1 -> (
          match AEL.ModelMap.M.min_binding c with
          | [], e -> e
          | _ -> assert false )
        | C c ->
          (* Currently, there are no uninterpred functions in the tests/benchs,
             therefore this is ok, but it should be fixed in the future *)
          Fmt.failwith "Altergo_mappings: no value for %a (%a)" AEL.Id.pp hs
            (fun fmt m ->
              AEL.ModelMap.M.iter
                (fun k v ->
                  Fmt.pf fmt "[%a] -> %a; "
                    (Fmt.list ~sep:Fmt.comma AEL.Expr.print)
                    k AEL.Expr.print v )
                m )
            c

      let eval ?ctx:_ ?completion:_ (Model ((module Sat), m) : model) (e : term)
        : interp option =
        match Sat.get_model m with
        | None ->
          Fmt.failwith "Altergo_mappings: no value for (%a)" DTerm.print e
        | Some AEL.Models.{ model; _ } ->
          let m =
            AEL.ModelMap.fold
              (fun ((hs, _, _) as id) g acc ->
                let e = cgraph_to_value hs g in
                let tcst = Dolmenexpr_to_expr.tcst_of_symbol (aeid_to_sym id) in
                DM.Model.Cst.add tcst (ae_expr_to_dvalue e) acc )
              model DM.Model.empty
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
                     (* ; Array.builtins
                      ; Fp.builtins *)
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

  let is_available = true

  include Make ()
end

module M' : Mappings_intf.M_with_make = M

include Mappings.Make (M)

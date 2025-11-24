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
  module DBuiltin = DStd.Builtin
  module DM = Dolmen_model

  module ConstSet = Set.Make (struct
    type t = DStd.Expr.Term.Const.t

    let compare = DStd.Expr.Term.Const.compare
  end)

  module ConstMap = Map.Make (struct
    type t = DStd.Expr.Term.Const.t

    let compare = DStd.Expr.Term.Const.compare
  end)

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

    module Internals = struct
      let name = "Alt-Ergo"
      let caches_consts = false

      let is_available = true
    end

    type handle

    type solver =
      { used_context : Frontend.used_context
      ; mutable syms : C.sat_tdecl ConstMap.t
      ; mutable cmds : C.sat_tdecl list
      ; mutable model : model option
      }

    type optimizer

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
        | Random_seed -> ()

      let set_params (params : Params.t) =
        List.iter
          (fun (Params.P (p, v)) -> set_param p v)
          (Params.to_list params)

      let make ?params ?logic:_ () =
        Option.iter set_params params;
        let used_context = Frontend.init_all_used_context () in
        { used_context; syms = ConstMap.empty; cmds = []; model = None }

      let clone _ = Fmt.failwith "Altergo_mappings: clone is not implemented"

      let push s =
        s.cmds <- C.{ st_decl = C.Push 1; st_loc = AEL.Loc.dummy } :: s.cmds

      let pop s n =
        s.cmds <- C.{ st_decl = C.Pop n; st_loc = AEL.Loc.dummy } :: s.cmds

      let reset s = s.cmds <- []

      let mk_decls new_syms sym_acc =
        ConstSet.fold
          (fun c sym_acc ->
            if ConstMap.mem c sym_acc then sym_acc
            else
              let mk_res =
                AEL.Translate.make dummy_file []
                  (mk_dstmt (`Decls [ `Term_decl c ]))
              in
              match mk_res with
              | [ d ] -> ConstMap.add c d sym_acc
              | _ -> assert false )
          new_syms sym_acc

      let mk_cmds new_syms sym_acc e_acc (el : term list) :
        C.sat_tdecl ConstMap.t * C.sat_tdecl list =
        let stl = mk_dstmt (`Check (List.rev el)) in
        let sym_acc = mk_decls new_syms sym_acc in
        let mk_res = AEL.Translate.make dummy_file e_acc stl in
        (sym_acc, mk_res)

      let get_new_syms ctx =
        Symbol.Map.fold
          (fun _ t acc ->
            let c =
              match (t : DTerm.t) with
              | { term_descr = Cst c; _ } -> c
              | _ -> assert false
            in
            ConstSet.add c acc )
          ctx ConstSet.empty

      let add ?(ctx = Symbol.Map.empty) (s : solver) (el : term list) : unit =
        match el with
        | [] -> ()
        | _ -> (
          let new_syms = get_new_syms ctx in
          let syms, cmds = mk_cmds new_syms s.syms s.cmds el in
          match cmds with
          | [] -> assert false
          | _ :: cmds ->
            s.cmds <- cmds;
            s.syms <- syms )

      let add_decls sym_decls cmds =
        ConstMap.fold (fun _ d acc -> d :: acc) sym_decls cmds

      let aety_to_ty (ty : AEL.Ty.t) : Ty.t =
        match ty with
        | Tbool -> Ty_bool
        | Tint -> Ty_int
        | Treal -> Ty_real
        | Tbitv n -> Ty_bitv n
        | _ -> assert false

      let aeid_to_sym ((hs, tyl, ty) : AEL.Id.typed) =
        assert (match tyl with [] -> true | _ -> false);
        Symbol.make (aety_to_ty ty) (AEL.Hstring.view hs)

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

      let mk_model ~ctx model =
        AEL.ModelMap.fold
          (fun ((hs, _, _) as id) g acc ->
            let e = cgraph_to_value hs g in
            let sym = aeid_to_sym id in
            let tcst =
              match (Symbol.Map.find_opt sym ctx : DTerm.t option) with
              | Some { term_descr = Cst c; _ } -> c
              | _ -> assert false
            in
            ConstMap.add tcst (ae_expr_to_dvalue e) acc )
          model ConstMap.empty

      let check ?(ctx = Symbol.Map.empty) (s : solver) ~(assumptions : term list)
        : [> `Sat | `Unknown | `Unsat ] =
        let new_syms = get_new_syms ctx in
        let syms, cmds = mk_cmds new_syms s.syms s.cmds assumptions in
        (* s.cmds <- cmds;
            s.syms <- syms; *)
        let cmds = add_decls syms (List.rev cmds) in
        let ftdn_env = FE.init_env s.used_context in
        List.iter (FE.process_decl ftdn_env) cmds;
        match ftdn_env.FE.res with
        | `Unknown | `Sat ->
          let model =
            match Sat.get_model ftdn_env.sat_env with
            | None -> Fmt.failwith "Alt-Ergo mappings: no model found"
            | Some AEL.Models.{ model; _ } -> model
          in
          let model = mk_model ~ctx model in
          s.model <- Some model;
          `Sat
        | `Unsat -> `Unsat

      let model (s : solver) : model option = s.model

      let add_simplifier s = s

      let interrupt _ =
        Fmt.failwith "Altergo_mappings: interrupt is not implemented"

      let get_statistics _ = Statistics.Map.empty

      let pp_statistics _fmt _solver = ()
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

include Mappings.Make (M)

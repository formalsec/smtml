(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2024 formalsec *)
(* Written by Hichem Rami Ait El Hara *)

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

(* TODO: make it possible to choose through an option? *)
let () = AEL.Options.set_interpretation ILast

let dummy_file = DStd.Loc.mk_file "dummy_file"

let mk_dstmt decl =
  AEL.D_loop.Typer_Pipe.
    { id = DStd.Id.mk DStd.Id.term "dummy_id"
    ; contents = decl
    ; loc = DStd.Loc.no_loc
    ; attrs = []
    ; implicit = false
    }

module Fresh = struct
  module Make () = struct
    (* TODO: experiment with other sat solvers? Make it possible to choose
       different sat solvers from command line? *)
    module Sat = AEL.Satml_frontend.Make (AEL.Theory.Main_Default)
    module FE = Frontend.Make (Sat)

    type 'a sat_module = (module Sat_solver_sig.S with type t = 'a)

    type model = Model : 'a sat_module * 'a -> model

    type solver =
      { used_context : Frontend.used_context
      ; mutable syms : C.sat_tdecl ConstMap.t
      ; mutable cmds : C.sat_tdecl list
      ; mutable model : model option
      }

    type optimize = |

    type handle = |

    module Smtlib = struct
      let pp ?name:_ ?logic:_ ?status:_ _fmt _ = assert false
    end

    module Solver = struct
      let make ?params:_ ?logic:_ () =
        let used_context = Frontend.init_all_used_context () in
        { used_context; syms = ConstMap.empty; cmds = []; model = None }

      let add_simplifier s : solver = s

      let clone _ = Fmt.failwith "Altergo_mappings: clone is not implemented"

      let push s =
        s.cmds <- C.{ st_decl = C.Push 1; st_loc = AEL.Loc.dummy } :: s.cmds

      let pop s n =
        s.cmds <- C.{ st_decl = C.Pop n; st_loc = AEL.Loc.dummy } :: s.cmds

      let reset s = s.cmds <- []

      let exprl_stmtl el =
        let sym_acc, dtl =
          List.fold_left
            (fun (acc, l) e ->
              let acc, e =
                encode_expr_acc
                  ~record_sym:(fun acc c -> ConstSet.add c acc)
                  acc e
              in
              (acc, e :: l) )
            (ConstSet.empty, []) el
        in
        let decl = `Check (List.rev dtl) in
        (sym_acc, mk_dstmt decl)

      let mk_decls new_syms sym_acc =
        ConstSet.fold
          (fun c sym_acc ->
            if ConstMap.mem c sym_acc then sym_acc
            else
              let mk_res =
                AEL.D_cnf.make dummy_file []
                  (mk_dstmt (`Decls [ `Term_decl c ]))
              in
              match mk_res with
              | [ d ] -> ConstMap.add c d sym_acc
              | _ -> assert false )
          new_syms sym_acc

      let mk_cmds sym_acc e_acc el =
        let new_syms, stl = exprl_stmtl el in
        let sym_acc = mk_decls new_syms sym_acc in
        let mk_res = AEL.D_cnf.make dummy_file e_acc stl in
        (sym_acc, mk_res)

      let add s el =
        match el with
        | [] -> ()
        | _ -> (
          let syms, cmds = mk_cmds s.syms s.cmds el in
          match cmds with
          | [] -> assert false
          | _hd :: tl ->
            s.cmds <- tl;
            s.syms <- syms )

      let add_decls sym_decls cmds =
        ConstMap.fold (fun _ d acc -> d :: acc) sym_decls cmds

      let check s ~assumptions =
        let syms, cmds = mk_cmds s.syms s.cmds assumptions in
        let cmds = add_decls syms (List.rev cmds) in
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

      let interrupt _ =
        Fmt.failwith "Altergo_mappings: interrupt is not implemented"

      let get_statistics _ =
        Fmt.failwith "Altergo_mappings: get_statistics is not implemented"
    end

    module Optimizer = struct
      let make () = assert false

      let push _ = ()

      let pop _ = ()

      let add _ _ = assert false

      let check _ = assert false

      let model _ = assert false

      let maximize _ _ = assert false

      let minimize _ _ = assert false

      let interrupt _ = ()

      let get_statistics _ = assert false
    end

    let ty_to_aety (ty : Ty.t) : AEL.Ty.t =
      match ty with
      | Ty_bool -> Tbool
      | Ty_int -> Tint
      | Ty_real -> Treal
      | Ty_bitv n -> Tbitv n
      | Ty_list | Ty_none | Ty_str | Ty_app | Ty_unit | Ty_fp _ | Ty_regexp ->
        assert false

    let aety_to_ty (ty : AEL.Ty.t) : Ty.t =
      match ty with
      | Tbool -> Ty_bool
      | Tint -> Ty_int
      | Treal -> Ty_real
      | Tbitv n -> Ty_bitv n
      | _ -> assert false

    let _sym_to_aeid Symbol.{ ty; name; _ } : AEL.Id.typed =
      match name with
      | Simple s ->
        let name = AEL.Hstring.make s in
        (name, [], ty_to_aety ty)
      | Indexed _ -> assert false

    let _aeid_to_sym ((hs, tyl, ty) : AEL.Id.typed) =
      assert (match tyl with [] -> true | _ -> false);
      Symbol.make (aety_to_ty ty) (AEL.Hstring.view hs)

    let _ae_expr_to_value e : Value.t =
      match AEL.Expr.term_view e with
      | { f = True; _ } -> True
      | { f = False; _ } -> False
      | { f = Int z; _ } -> Int (Z.to_int z)
      | { f = Real q; _ } -> Real (Q.to_float q)
      | { f = Bitv (8, z); _ } -> Num (I8 (Z.to_int z))
      | { f = Bitv (32, z); _ } -> Num (I32 (Z.to_int32 z))
      | { f = Bitv (64, z); _ } -> Num (I64 (Z.to_int64 z))
      | _ ->
        Fmt.failwith "Altergo_mappings: ae_expr_to_value(%a)" AEL.Expr.print e

    let _ae_expr_to_dvalue e : DM.Value.t =
      match AEL.Expr.term_view e with
      | { f = True; _ } -> DM.Bool.mk true
      | { f = False; _ } -> DM.Bool.mk false
      | { f = Int z; _ } -> DM.Int.mk z
      | { f = Real q; _ } -> DM.Real.mk q
      | { f = Bitv (n, z); _ } -> DM.Bitv.mk n z
      | _ ->
        Fmt.failwith "Altergo_mappings: ae_expr_to_dvalue(%a)" AEL.Expr.print e

    let _dvalue_to_value (ty : Ty.t) (v : DM.Value.t) : Value.t =
      match DM.Value.extract ~ops:DM.Bool.ops v with
      | Some true -> True
      | Some false -> False
      | None -> (
        match DM.Value.extract ~ops:DM.Int.ops v with
        | Some z -> Int (Z.to_int z)
        | None -> (
          match DM.Value.extract ~ops:DM.Real.ops v with
          | Some q -> Real (Q.to_float q)
          | None -> (
            match (DM.Value.extract ~ops:DM.Bitv.ops v, ty) with
            | Some z, Ty_bitv 8 -> Num (I8 (Z.to_int z))
            | Some z, Ty_bitv 32 -> Num (I32 (Z.to_int32 z))
            | Some z, Ty_bitv 64 -> Num (I64 (Z.to_int64 z))
            | _ ->
              Fmt.failwith "Altergo_mappings: dvalue_to_value(%a)"
                DM.Value.print v ) ) )

    let _cgraph_to_value _hs _g =
      (* match (g : AEL.ModelMap.graph) with *)
      (* | Free e -> e *)
      (* | C c when AEL.ModelMap.M.cardinal c = 1 -> ( *)
      (*   match AEL.ModelMap.M.min_binding c with [], e -> e | _ -> assert false ) *)
      (* | C c -> *)
      (*   (1* Currently, there are no uninterpred functions in the tests/benchs, *)
      (*      therefore this is ok, but it should be fixed in the future *1) *)
      (*   Fmt.failwith "Altergo_mappings: no value for %a (%a)" AEL.Id.pp hs *)
      (*     (fun fmt m -> *)
      (*       AEL.ModelMap.M.iter *)
      (*         (fun k v -> *)
      (*           Fmt.pf fmt "[%a] -> %a; " *)
      (*             (Fmt.list ~sep:Fmt.comma AEL.Expr.print) *)
      (*             k AEL.Expr.print v ) *)
      (*         m ) *)
      (*     c *)
      assert false (* This function should never be reached *)

    let value (Model ((module Sat), _m) : model) (_e : Expr.t) : Value.t =
      (* match Sat.get_model m with *)
      (* | None -> Fmt.failwith "Altergo_mappings: no value for (%a)" Expr.pp e *)
      (* | Some AEL.Models.{ model; _ } -> *)
      (*   let m = *)
      (*     AEL.ModelMap.fold *)
      (*       (fun ((hs, _, _) as id) g acc -> *)
      (*         let e = cgraph_to_value hs g in *)
      (*         let tcst = Dolmenexpr_to_expr.tcst_of_symbol (aeid_to_sym id) in *)
      (*         DM.Model.Cst.add tcst (ae_expr_to_dvalue e) acc ) *)
      (*       model DM.Model.empty *)
      (*   in *)
      (*   let env = *)
      (*     DM.Env.mk m *)
      (*       ~builtins: *)
      (*         (DM.Eval.builtins *)
      (*            [ DM.Core.builtins *)
      (*            ; DM.Bool.builtins *)
      (*            ; DM.Int.builtins *)
      (*            ; DM.Rat.builtins *)
      (*            ; DM.Real.builtins *)
      (*            ; DM.Bitv.builtins *)
      (*              (1* ; Array.builtins *)
      (*                 ; Fp.builtins *1) *)
      (*            ] ) *)
      (*   in *)
      (*   let v = DM.Eval.eval env (encode_expr e) in *)
      (*   dvalue_to_value (Expr.ty e) v *)
      Fmt.failwith
        "Altergo_mappings: model generation is currently unsupported!"

    let values_of_model ?symbols:_ (Model ((module Sat), _m)) : Model.t =
      (* match Sat.get_model m with *)
      (* | None -> assert false *)
      (* | Some AEL.Models.{ model; _ } -> ( *)
      (*   let r = Hashtbl.create 17 in *)
      (*   match symbols with *)
      (*   | None -> *)
      (*     AEL.ModelMap.fold *)
      (*       (fun ((hs, _, _) as aeid) g () -> *)
      (*         let sym = aeid_to_sym aeid in *)
      (*         Hashtbl.add r sym (ae_expr_to_value (cgraph_to_value hs g)) ) *)
      (*       model (); *)
      (*     r *)
      (*   | Some syml -> *)
      (*     List.iter *)
      (*       (fun sym -> *)
      (*         let ((hs, _, _) as aeid) = sym_to_aeid sym in *)
      (*         let e = *)
      (*           try cgraph_to_value hs (AEL.ModelMap.find aeid model) *)
      (*           with Not_found -> *)
      (*             Fmt.failwith "Altergo_mappings: no value for %a" AEL.Id.pp hs *)
      (*         in *)
      (*         Hashtbl.add r sym (ae_expr_to_value e) ) *)
      (*       syml; *)
      (*     r ) *)
      Fmt.failwith
        "Altergo_mappings: model generation is currently unsupported!"

    let set_debug _ = ()
  end
end

let is_available = true

include Fresh.Make ()

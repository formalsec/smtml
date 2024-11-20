(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2024 formalsec *)
(* Written by Hichem Rami Ait El Hara *)

[@@@ocaml.warning "-37"]

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

module Var = struct
  include Dolmen_std.Expr.Term.Var

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

module Make0 () = struct
  include Dolmenexpr_to_expr
  module Sim = OcplibSimplex.Basic.Make (Var) (Rat) (Ex)

  type nonrec ty = ty

  type nonrec term = term

  type interp = Interp

  type model =
    Colibri2_core.Egraph.wt * (DTerm.Const.t * Colibri2_core.Value.t) list

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

  type optimizer = Sim.Core.t

  type handle = optimizer * (Sim.Core.P.t * bool) option

  type func_decl = Func_decl

  let caches_consts = false

  module Interp = struct
    let to_int _ = assert false

    let to_real _ = assert false

    let to_bool _ = assert false

    let to_string _ = assert false

    let to_bitv _ = assert false

    let to_float _ = assert false
  end

  module Model = struct
    let get_symbols _ =
      Fmt.failwith "Colibri2_mappings: get_symbols not implemented"

    let eval ?completion:_ _ = assert false
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

    let reset _ = assert false

    let add _ = assert false

    let check _ ~assumptions:_ = assert false

    let model _ = assert false

    let add_simplifier solver =
      (* does nothing *)
      solver

    let interrupt _ =
      (* does nothing *)
      ()

    let get_statistics _ =
      Fmt.failwith "Colibri2_mappings: Solver.get_statistics not implemented"

    let pp_statistics _ = assert false
  end

  module Optimizer = struct
    let make _ =
      Fmt.failwith "Colibri2_mappings: Optimizer.make not implemented"

    let push _ =
      Fmt.failwith "Colibri2_mappings: Optimizer.push not implemented"

    let pop _ = Fmt.failwith "Colibri2_mappings: Optimizer.pop not implemented"

    let add _ = Fmt.failwith "Colibri2_mappings: Optimizer.add not implemented"

    let check _ =
      Fmt.failwith "Colibri2_mappings: Optimizer.check not implemented"

    let model _ =
      Fmt.failwith "Colibri2_mappings: Optimizer.model not implemented"

    let maximize _ =
      Fmt.failwith "Colibri2_mappings: Optimizer.maximize not implemented"

    let minimize _ =
      Fmt.failwith "Colibri2_mappings: Optimizer.minimize not implemented"

    let interrupt _ =
      Fmt.failwith "Colibri2_mappings: Optimizer.interrupt not implemented"

    let get_statistics _ =
      Fmt.failwith "Colibri2_mappings: Solver.get_statistics not implemented"

    let pp_statistics _ =
      Fmt.failwith "Colibri2_mappings: Optimizer.pp_statistics not implemented"
  end

  module Smtlib = struct
    let pp ?name:_ ?logic:_ ?status:_ _fmt _ =
      Fmt.failwith "Colibri2_mappings: Smtlib.pp not implemented"
  end
end

include Mappings.Make (struct
  module Make () = Make0 ()

  let is_available = true

  include Make ()
end)

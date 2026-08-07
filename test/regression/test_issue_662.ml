(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2026 formalsec *)
(* Written by the Smtml programmers *)

module CVC5 = Smtml.Solver.Batch (Smtml.Cvc5_mappings)
module Bitwulza = Smtml.Solver.Batch (Smtml.Bitwuzla_mappings)
module Z3 = Smtml.Solver.Batch (Smtml.Z3_mappings)

let cvc5 = CVC5.create ()

let bitwuzla = Bitwulza.create ()

let z3 = Z3.create ()

(* Ex. x != 0 is sat *)
let () =
  let open Smtml in
  let x = Typed.var Typed.Types.bitv32 "x" in
  let x_is_not_zero = Typed.Bool.not (Typed.Bitv32.eq x Typed.Bitv32.zero) in
  let expr = Typed.Bool.exists [ x ] x_is_not_zero in

  let answer_cvc5 =
    match CVC5.check cvc5 [ (expr :> Expr.t) ] with
    | `Sat -> true
    | `Unsat | `Unknown -> false
  in

  let answer_bitwuzla =
    match Bitwulza.check bitwuzla [ (expr :> Expr.t) ] with
    | `Sat -> true
    | `Unsat | `Unknown -> false
  in

  assert answer_cvc5;
  assert answer_bitwuzla

(* Vx. 1 < x is unsat *)
let () =
  let open Smtml in
  let x = Typed.var Typed.Types.bitv32 "x" in
  let exprx = Typed.Bitv32.lt Smtml.Typed.Bitv32.one x in
  let expr = Typed.Bool.forall [ x ] exprx in

  let answer_cvc5 =
    match CVC5.check cvc5 [ (expr :> Expr.t) ] with
    | `Sat -> true
    | `Unsat | `Unknown -> false
  in

  let answer_bitwuzla =
    match Bitwulza.check bitwuzla [ (expr :> Expr.t) ] with
    | `Sat -> true
    | `Unsat | `Unknown -> false
  in

  let answer_z3 =
    match Z3.check z3 [ (expr :> Expr.t) ] with
    | `Sat -> true
    | `Unsat | `Unknown -> false
  in

  assert (not answer_cvc5);
  assert (not answer_bitwuzla);
  assert (not answer_z3)

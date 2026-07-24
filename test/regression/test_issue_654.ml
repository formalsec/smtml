(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2026 formalsec *)
(* Written by the Smtml programmers *)

open Smtml

let y = Typed.Float32.symbol (Symbol.make_const (Smtml.Ty.Ty_fp 32) "y")

let expr = Typed.Float32.neg (Typed.Float32.add (Typed.Float32.of_float 42.) y)

let expr = Typed.Bitv32.reinterpret_f32 expr

let expr = Typed.Bitv32.lt expr Smtml.Typed.Bitv32.one

module CVC5 = Solver.Batch (Smtml.Cvc5_mappings)

let solver = CVC5.create ()

let () =
  assert (
    match CVC5.check solver [ (expr :> Expr.t) ] with
    | `Sat -> true
    | `Unsat | `Unknown -> false )

(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2026 formalsec *)
(* Written by the Smtml programmers *)

(** Regression test: [Expr.let_in] expressions should be encodable by the solver
    backends. Before the fix, encoding a [Let_in] binder raised
    ["Cannot encode expression"]. *)

open Smtml
module Z3 = Solver.Batch (Smtml.Z3_mappings)

let z3 = Z3.create ()

let symbol_of_expr (e : Expr.t) : Symbol.t =
  match Expr.view e with Expr.Symbol sym -> sym | _ -> assert false

(* let x = 1 in x = 1  ==> sat, with a [Var]-namespace symbol *)
let () =
  let x = Typed.var Typed.Types.int "x" in
  let x_sym = symbol_of_expr (Typed.Unsafe.unwrap x) in
  let one = Typed.Unsafe.unwrap (Typed.Int.v Z.one) in
  let body = Typed.Unsafe.unwrap (Typed.Int.eq x (Typed.Int.v Z.one)) in
  let let_expr = Expr.let_in [ (x_sym, one) ] body in

  match Z3.check z3 [ let_expr ] with
  | `Sat -> ()
  | `Unsat | `Unknown -> assert false

(* let x = 1 in x = 2  ==> unsat, with a [Term]-namespace symbol *)
let () =
  let x = Typed.const Typed.Types.int "x" in
  let x_sym = symbol_of_expr (Typed.Unsafe.unwrap x) in
  let one = Typed.Unsafe.unwrap (Typed.Int.v Z.one) in
  let body = Typed.Unsafe.unwrap (Typed.Int.eq x (Typed.Int.v (Z.of_int 2))) in
  let let_expr = Expr.let_in [ (x_sym, one) ] body in

  match Z3.check z3 [ let_expr ] with
  | `Unsat -> ()
  | `Sat | `Unknown -> assert false

(* let x = 1 in x + y < 10  ==> sat, bitvectors as in the original issue *)
let () =
  let x = Typed.const Typed.Types.bitv32 "x" in
  let y = Typed.const Typed.Types.bitv32 "y" in
  let x_sym = symbol_of_expr (Typed.Unsafe.unwrap x) in
  let one = Typed.Unsafe.unwrap Typed.Bitv32.one in
  let body =
    Typed.Unsafe.unwrap
      (Typed.Bitv32.lt (Typed.Bitv32.add x y) (Typed.Bitv32.of_int 10))
  in
  let let_expr = Expr.let_in [ (x_sym, one) ] body in

  match Z3.check z3 [ let_expr ] with
  | `Sat -> ()
  | `Unsat | `Unknown -> assert false

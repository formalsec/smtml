(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2026 formalsec *)
(* Written by the Smtml programmers *)

let test_serialization () =
  let open Smtml in
  let z = Typed.Bitv32.symbol (Symbol.make (Ty_bitv 32) "z") in
  let expr = Typed.Bitv32.popcnt (Typed.Bitv32.rotate_right 91 z) in
  let expr = Typed.Bitv32.le_u Typed.Bitv32.one expr in
  let parse =
    Format.asprintf "%a" Expr.Printer.pp_expr (Typed.Unsafe.unwrap expr)
  in
  let reparse = Parse.Smtml.Expr.from_string parse in
  match reparse with
  | Result.Error (`Msg msg) -> Alcotest.failf "parsing error: %s" msg
  | Result.Ok _ -> ()

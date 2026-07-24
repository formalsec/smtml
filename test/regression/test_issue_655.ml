(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2026 formalsec *)
(* Written by the Smtml programmers *)

let z = Smtml.Typed.Bitv32.symbol (Smtml.Symbol.make (Smtml.Ty.Ty_bitv 32) "z")

let expr = Smtml.Typed.Bitv32.popcnt (Smtml.Typed.Bitv32.rotate_right 91 z)

let expr = Smtml.Typed.Bitv32.le_u Smtml.Typed.Bitv32.one expr

let parse =
  Format.asprintf "%a" Smtml.Expr.Printer.pp_expr
    (Smtml.Typed.Unsafe.unwrap expr)

let reparse = Smtml.Parse.Smtml.Expr.from_string parse

let () =
  assert (
    match reparse with
    | Result.Error (`Msg msg) -> begin
      Format.eprintf "parsing error: %s@." msg;
      false
      end
    | Result.Ok _ -> true )

(***************************************************************************)
(* This file is part of the third-party OCaml library `smtml`.             *)
(* Copyright (C) 2023-2024 formalsec                                       *)
(*                                                                         *)
(* This program is free software: you can redistribute it and/or modify    *)
(* it under the terms of the GNU General Public License as published by    *)
(* the Free Software Foundation, either version 3 of the License, or       *)
(* (at your option) any later version.                                     *)
(*                                                                         *)
(* This program is distributed in the hope that it will be useful,         *)
(* but WITHOUT ANY WARRANTY; without even the implied warranty of          *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the           *)
(* GNU General Public License for more details.                            *)
(*                                                                         *)
(* You should have received a copy of the GNU General Public License       *)
(* along with this program.  If not, see <https://www.gnu.org/licenses/>.  *)
(***************************************************************************)

open Lexer
open Lexing
open Format

let pp_pos fmt lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf fmt "%s:%d:%d" pos.pos_fname pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
  try Parser.script Lexer.token lexbuf with
  | SyntaxError msg ->
    fprintf err_formatter "%a: %s\n" pp_pos lexbuf msg;
    []
  | Parser.Error ->
    fprintf err_formatter "%a: syntax error\n" pp_pos lexbuf;
    exit 1

let from_file ~filename =
  let chan = match filename with "-" -> stdin | _ -> open_in filename in
  Fun.protect
    ~finally:(fun () -> close_in chan)
    (fun () ->
      let lexbuf = Lexing.from_channel chan in
      lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
      parse_with_error lexbuf )

let from_string contents = parse_with_error (Lexing.from_string contents)

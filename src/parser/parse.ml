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

let pp_pos fmt lexbuf =
  let pos = lexbuf.lex_curr_p in
  Fmt.pf fmt "%s:%d:%d" pos.pos_fname pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
  try Parser.script Lexer.token lexbuf with
  | SyntaxError msg ->
    Fmt.epr "%a: %s\n" pp_pos lexbuf msg;
    []
  | Parser.Error ->
    Fmt.epr "%a: syntax error\n" pp_pos lexbuf;
    exit 1

let from_file filename =
  let res =
    Bos.OS.File.with_ic filename
      (fun chan () ->
        let lexbuf = Lexing.from_channel chan in
        lexbuf.lex_curr_p <-
          { lexbuf.lex_curr_p with pos_fname = Fpath.to_string filename };
        Ok (parse_with_error lexbuf) )
      ()
  in
  match res with
  | Error (`Msg e) -> Fmt.failwith "%s" e
  | Ok (Error (`Msg e)) -> Fmt.failwith "%s" e
  | Ok (Ok v) -> v

let from_string contents = parse_with_error (Lexing.from_string contents)

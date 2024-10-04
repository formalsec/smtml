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

exception Syntax_error of string

module Smtml = struct
  open Lexer
  open Lexing

  let pp_pos fmt lexbuf =
    let pos = lexbuf.lex_curr_p in
    Fmt.pf fmt "%s:%d:%d" pos.pos_fname pos.pos_lnum
      (pos.pos_cnum - pos.pos_bol + 1)

  let parse_with_error lexbuf =
    try Parser.script Lexer.token lexbuf with
    | SyntaxError msg ->
      raise (Syntax_error (Fmt.str "%a: %s" pp_pos lexbuf msg))
    | Parser.Error ->
      raise (Syntax_error (Fmt.str "%a: syntax error" pp_pos lexbuf))

  let from_file filename =
    let res =
      Bos.OS.File.with_ic filename
        (fun chan () ->
          let lexbuf = Lexing.from_channel chan in
          lexbuf.lex_curr_p <-
            { lexbuf.lex_curr_p with pos_fname = Fpath.to_string filename };
          parse_with_error lexbuf )
        ()
    in
    match res with Error (`Msg e) -> Fmt.failwith "%s" e | Ok v -> v

  let from_string contents = parse_with_error (Lexing.from_string contents)
end

module Smtlib = struct
  let from_file filename =
    try
      let _, st = Smtlib.parse_all (`File (Fpath.to_string filename)) in
      Lazy.force st
    with
    | Dolmen.Std.Loc.Syntax_error (loc, `Regular msg) ->
      raise
        (Syntax_error
           (Fmt.str "%a: syntax error: %t" Dolmen.Std.Loc.print_compact loc msg)
        )
    | Dolmen.Std.Loc.Syntax_error (loc, `Advanced (x, _, _, _)) ->
      raise
        (Syntax_error
           (Fmt.str "%a: syntax error: %s" Dolmen.Std.Loc.print_compact loc x)
        )
end

let from_file filename =
  match Fpath.split_ext filename with
  | _, ".smtml" -> Smtml.from_file filename
  | _, ".smt2" -> Smtlib.from_file filename
  | fname, ext -> (
    (* FIXME: I don't like this *)
    match Fpath.to_string fname with
    | "-" -> Smtml.from_file filename
    | _ -> Fmt.failwith "Unsupported script type with extension '%s'" ext )

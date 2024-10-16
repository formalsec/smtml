(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2024 formalsec *)
(* Written by the Smtml programmers *)

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

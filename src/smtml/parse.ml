(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2026 formalsec *)
(* Written by the Smtml programmers *)

module Smtml = struct
  open Lexer
  open Lexing

  let pp_pos fmt lexbuf =
    let pos = lexbuf.lex_curr_p in
    Fmt.pf fmt "%s:%d:%d" pos.pos_fname pos.pos_lnum
      (pos.pos_cnum - pos.pos_bol + 1)

  let parse_with_error parser lexbuf =
    try Ok (parser Lexer.token lexbuf) with
    | SyntaxError msg -> Fmt.error_msg "%a: %s" pp_pos lexbuf msg
    | Parser.Error -> Fmt.error_msg "%a: syntax error" pp_pos lexbuf

  module Script = struct
    let from_file filename =
      let open Result.Syntax in
      let* result =
        Bos.OS.File.with_ic filename
          (fun chan () ->
            let lexbuf = Lexing.from_channel chan in
            lexbuf.lex_curr_p <-
              { lexbuf.lex_curr_p with pos_fname = Fpath.to_string filename };
            parse_with_error Parser.script lexbuf )
          ()
      in
      result

    let from_string contents =
      parse_with_error Parser.script (Lexing.from_string contents)
  end

  module Expr = struct
    let from_file filename =
      let open Result.Syntax in
      let* result =
        Bos.OS.File.with_ic filename
          (fun chan () ->
            let lexbuf = Lexing.from_channel chan in
            lexbuf.lex_curr_p <-
              { lexbuf.lex_curr_p with pos_fname = Fpath.to_string filename };
            parse_with_error Parser.expression lexbuf )
          ()
      in
      result

    let from_string contents =
      parse_with_error Parser.expression (Lexing.from_string contents)
  end
end

module Smtlib = struct
  let from_file filename =
    try
      let _, st = Smtlib.parse_all (`File (Fpath.to_string filename)) in
      Ok (Lazy.force st)
    with
    | Dolmen.Std.Loc.Syntax_error (loc, `Regular msg) ->
      Fmt.error_msg "%a: syntax error: %t" Dolmen.Std.Loc.print_compact loc msg
    | Dolmen.Std.Loc.Syntax_error (loc, `Advanced (x, _, _, _)) ->
      Fmt.error_msg "%a: syntax error: %s" Dolmen.Std.Loc.print_compact loc x
end

let from_file filename =
  match Fpath.split_ext filename with
  | _, ".smtml" -> Smtml.Script.from_file filename
  | _, ".smt2" -> Smtlib.from_file filename
  | fname, ext -> (
    (* FIXME: I don't like this *)
    match Fpath.to_string fname with
    | "-" -> Smtml.Script.from_file filename
    | _ -> Fmt.failwith "Unsupported script type with extension '%s'" ext )

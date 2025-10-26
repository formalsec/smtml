(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2025 formalsec *)
(* Written by the Smtml programmers *)

(** Compilation Module. This module provides functionality for parsing and
    processing abstract syntax trees (ASTs) from files, with support for
    transformations and rewrites. *)

(** [until_rewrite path] parses and processes the AST from the given file path
    [path] and applies standard formula rewriting.

    [path] is the file path to read and process. The function returns a list of
    AST nodes resulting from the rewrite process. *)
val until_rewrite : Fpath.t -> no_simpls:bool -> Ast.t list

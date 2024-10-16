(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2024 formalsec *)
(* Written by the Smtml programmers *)

module type S = sig
  type v

  type t

  type unop

  type binop

  type relop

  type cvtop

  type triop

  val encode_val : v -> t

  val encode_unop : unop -> t -> t

  val encode_binop : binop -> t -> t -> t

  val encode_relop : relop -> t -> t -> t

  val encode_cvtop : cvtop -> t -> t

  val encode_triop : triop -> t -> t -> t -> t
end

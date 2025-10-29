(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2025 formalsec *)
(* Written by the Smtml programmers *)

(** @inline *)
include Expr_intf.S

module Smtlib : sig
  val pp : t Fmt.t
end

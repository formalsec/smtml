(***************************************************************************)
(* This file is part of the third-party OCaml library `smtml`.             *)
(* Copyright (C) 2023-2024 formalsec                                       *)
(* Written by Hichem Rami Ait El Hara                                      *)
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

module DExpr = Dolmen_std.Expr
module DTerm = DExpr.Term

module Builtin : sig
  val string_ty_cst : DExpr.ty_cst

  val string_ty : DExpr.ty

  val float32_ty : DExpr.ty

  val float64_ty : DExpr.ty

  val int_to_string : DExpr.term_cst

  val string_to_int : DExpr.term_cst

  val real_to_string : DExpr.term_cst

  val string_to_real : DExpr.term_cst

  val real_to_uint32 : DExpr.term_cst

  val trim_string : DExpr.term_cst

  val f32_to_string : DExpr.term_cst

  val string_to_f32 : DExpr.term_cst

  val f64_to_string : DExpr.term_cst

  val string_to_f64 : DExpr.term_cst
end

val tty_of_etype : Ty.t -> DTerm.ty

val tty_to_etype : DTerm.ty -> Ty.t

module SHT : Hashtbl.S with type key = Symbol.t

val sym_cache : DTerm.Const.t SHT.t

val tcst_of_symbol : Symbol.t -> DTerm.Const.t

val tcst_to_symbol : DExpr.term_cst -> Symbol.t

type expr = DExpr.term

val encode_val : Value.t -> expr

val encode_unop : Ty.t -> Ty.unop -> expr -> expr

val encode_binop : Ty.t -> Ty.binop -> expr -> expr -> expr

val encode_triop : Ty.t -> Ty.triop -> expr -> expr -> expr -> expr

val encode_relop : Ty.t -> Ty.relop -> expr -> expr -> expr

val encode_cvtop : Ty.t -> Ty.cvtop -> expr -> expr

val encode_expr_aux : ?record_sym:(DTerm.Const.t -> unit) -> Expr.t -> expr

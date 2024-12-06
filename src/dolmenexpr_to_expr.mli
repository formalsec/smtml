(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2024 formalsec *)
(* Written by Hichem Rami Ait El Hara *)

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

val encode_unop : Ty.t -> Ty.Unop.t -> expr -> expr

val encode_binop : Ty.t -> Ty.Binop.t -> expr -> expr -> expr

val encode_triop : Ty.t -> Ty.Triop.t -> expr -> expr -> expr -> expr

val encode_relop : Ty.t -> Ty.Relop.t -> expr -> expr -> expr

val encode_cvtop : Ty.t -> Ty.Cvtop.t -> expr -> expr

val encode_expr_acc :
  ?record_sym:('a -> DTerm.Const.t -> 'a) -> 'a -> Expr.t -> 'a * expr

val encode_expr : ?record_sym:(DExpr.term_cst -> unit) -> Expr.t -> expr

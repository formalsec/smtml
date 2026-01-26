(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2025 formalsec *)
(* Written by the Smtml programmers *)

include Expr

let unop = raw_unop

let binop = raw_binop

let triop = raw_triop

let relop = raw_relop

let cvtop = raw_cvtop

let naryop = raw_naryop

let extract t ~high ~low = raw_extract t ~high ~low

let concat = raw_concat

let simplify = Fun.id

module Bool = struct
  let true_ = Bool.true_

  let false_ = Bool.false_

  let v = Bool.v

  let not a = raw_unop Ty_bool Not a

  let equal a b = raw_relop Ty_bool Eq a b

  let distinct a b = raw_relop Ty_bool Ne a b

  let and_ a b = raw_binop Ty_bool And a b

  let or_ a b = raw_binop Ty_bool Or a b

  let ite a b c = raw_triop Ty_bool Ite a b c
end

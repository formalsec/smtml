(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2024 formalsec *)
(* Written by the Smtml programmers *)

let axioms =
  let x = Symbol.mk_symbol Ty.Ty_str "x" in
  [ Expression.Quantifier
      ( Expression.Forall
      , [ x ]
      , Strings.mk_eq
          (Integer.mk_to_string (Integer.mk_of_string (Expression.mk_symbol x)))
          (Expression.mk_symbol x)
      , [ [ Integer.mk_of_string (Expression.mk_symbol x) ] ] )
  ]

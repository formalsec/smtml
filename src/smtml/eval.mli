(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2024 formalsec *)
(* Written by the Smtml programmers *)

type op_type =
  [ `Unop of Ty.Unop.t
  | `Binop of Ty.Binop.t
  | `Relop of Ty.Relop.t
  | `Triop of Ty.Triop.t
  | `Cvtop of Ty.Cvtop.t
  | `Naryop of Ty.Naryop.t
  ]

exception DivideByZero

exception Value of Ty.t

exception
  TypeError of
    { index : int
    ; value : Value.t
    ; ty : Ty.t
    ; op : op_type
    }

val unop : Ty.t -> Ty.Unop.t -> Value.t -> Value.t

val binop : Ty.t -> Ty.Binop.t -> Value.t -> Value.t -> Value.t

val triop : Ty.t -> Ty.Triop.t -> Value.t -> Value.t -> Value.t -> Value.t

val relop : Ty.t -> Ty.Relop.t -> Value.t -> Value.t -> bool

val cvtop : Ty.t -> Ty.Cvtop.t -> Value.t -> Value.t

val naryop : Ty.t -> Ty.Naryop.t -> Value.t list -> Value.t

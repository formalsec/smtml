(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2024 formalsec *)
(* Written by the Smtml programmers *)

open Ty

type op_type =
  [ `Unop of Ty.unop
  | `Binop of Ty.binop
  | `Relop of Ty.relop
  | `Triop of Ty.triop
  | `Cvtop of Ty.cvtop
  | `Naryop of Ty.naryop
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

val unop : Ty.t -> unop -> Value.t -> Value.t

val binop : Ty.t -> binop -> Value.t -> Value.t -> Value.t

val triop : Ty.t -> triop -> Value.t -> Value.t -> Value.t -> Value.t

val relop : Ty.t -> relop -> Value.t -> Value.t -> bool

val cvtop : Ty.t -> cvtop -> Value.t -> Value.t

val naryop : Ty.t -> naryop -> Value.t list -> Value.t

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

module Make (T : sig
  type elt

  val ty : Ty.t

  val value : elt -> Value.t
end) =
struct
  open Ty

  let v i = value (T.value i)

  let sym x = symbol Symbol.(x @: T.ty)

  let ( ~- ) e = unop T.ty Neg e

  let ( = ) e1 e2 = relop Ty_bool Eq e1 e2

  let ( != ) e1 e2 = relop Ty_bool Ne e1 e2

  let ( > ) e1 e2 = relop T.ty Gt e1 e2

  let ( >= ) e1 e2 = relop T.ty Ge e1 e2

  let ( < ) e1 e2 = relop T.ty Lt e1 e2

  let ( <= ) e1 e2 = relop T.ty Le e1 e2
end

module Bitv = struct
  open Ty

  module I8 = Make (struct
    type elt = int

    let ty = Ty_bitv 8

    let value i = Value.Bitv (Bitvector.of_int8 i)
  end)

  module I32 = Make (struct
    type elt = int32

    let ty = Ty_bitv 32

    let value i = Value.Bitv (Bitvector.of_int32 i)
  end)

  module I64 = Make (struct
    type elt = int64

    let ty = Ty_bitv 64

    let value i = Value.Bitv (Bitvector.of_int64 i)
  end)
end

module Fpa = struct
  open Ty

  module F32 = struct
    include Make (struct
      type elt = float

      let ty = Ty_fp 32

      let value f = Value.Num (F32 (Int32.bits_of_float f))
    end)

    (* Redeclare equality due to incorrect theory annotation *)
    let ( = ) e1 e2 = relop (Ty_fp 32) Eq e1 e2

    let ( != ) e1 e2 = relop (Ty_fp 32) Ne e1 e2
  end

  module F64 = struct
    include Make (struct
      type elt = float

      let ty = Ty_fp 64

      let value f = Value.Num (F64 (Int64.bits_of_float f))
    end)

    (* Redeclare equality due to incorrect theory annotation *)
    let ( = ) e1 e2 = relop (Ty_fp 64) Eq e1 e2

    let ( != ) e1 e2 = relop (Ty_fp 64) Ne e1 e2
  end
end

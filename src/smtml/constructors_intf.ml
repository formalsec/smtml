(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2024 formalsec *)
(* Written by the Smtml programmers *)

module type Infix = sig
  type t

  type elt

  val v : elt -> t

  val sym : string -> t

  val ( ~- ) : t -> t

  val ( = ) : t -> t -> t

  val ( != ) : t -> t -> t

  val ( > ) : t -> t -> t

  val ( >= ) : t -> t -> t

  val ( < ) : t -> t -> t

  val ( <= ) : t -> t -> t
end

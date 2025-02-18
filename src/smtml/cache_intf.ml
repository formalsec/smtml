(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2024 formalsec *)
(* Written by the Smtml programmers *)

module type S = sig
  include Hashtbl.S with type key = Expr.Set.t
end

module type Intf = sig
  module type S = S

  module Strong : S
end

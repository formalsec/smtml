(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2024 formalsec *)
(* Written by the Smtml programmers *)

include Cache_intf

module Strong : S = struct
  include Hashtbl.Make (struct
    type t = Expr.Set.t

    let equal s1 s2 = Expr.Set.equal s1 s2

    let hash s = Expr.Set.to_int s
  end)
end

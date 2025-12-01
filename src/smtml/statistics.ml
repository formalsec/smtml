(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2024 formalsec *)
(* Written by the Smtml programmers *)

type entry =
  [ `Int of int
  | `Float of float
  ]

module Map = Map.Make (String)

type t = entry Map.t

let sum_entries entry1 entry2 =
  match (entry1, entry2) with
  | `Int i1, `Int i2 -> `Int (i1 + i2)
  | `Float f1, `Float f2 -> `Float (f1 +. f2)
  | _ -> Fmt.failwith "Statistics: entry type mismatch"

let merge s1 s2 =
  Map.merge
    (fun _ left right ->
      match (left, right) with
      | Some left, Some right -> Some (sum_entries left right)
      | (Some _ as v), None | None, (Some _ as v) -> v
      | None, None -> None )
    s1 s2

let pp_entry fmt = function
  | `Int i -> Fmt.int fmt i
  | `Float f -> Fmt.float fmt f

let pp =
  Fmt.vbox ~indent:1
    (Fmt.parens
       (Fmt.iter
          ~sep:(fun fmt () -> Fmt.pf fmt "@\n")
          (fun f m -> Map.iter (fun a b -> f (a, b)) m)
          (Fmt.parens (Fmt.pair ~sep:Fmt.sp Fmt.string pp_entry)) ) )

let to_json (s : t) : Yojson.Basic.t =
  let jsoned = Map.map (function `Int v -> `Int v | `Float v -> `Float v) s in
  `Assoc (Map.bindings jsoned)

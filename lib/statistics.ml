type entry =
  [ `Int of int
  | `Float of float
  ]

module Map = Map.Make (String)

type t = entry Map.t

let combine e1 e2 =
  match (e1, e2) with
  | `Int i1, `Int i2 -> `Int (i1 + i2)
  | `Float f1, `Float f2 -> `Float (f1 +. f2)
  | _ -> failwith "Statistics: entry type mismatch"

let merge s1 s2 =
  Map.merge
    (fun _ left right ->
      match (left, right) with
      | Some v1, Some v2 -> Some (combine v1 v2)
      | (Some _ as v), None | None, (Some _ as v) -> v
      | None, None -> None )
    s1 s2

let pp_entry fmt = function
  | `Int i -> Format.pp_print_int fmt i
  | `Float f -> Format.pp_print_float fmt f

let pp fmt m =
  let iter f v = Map.iter (fun a b -> f (a, b)) v in
  let pp_v fmt (k, v) = Format.fprintf fmt "(%s, %a)" k pp_entry v in
  let is_first = ref true in
  let pp_v v =
    if !is_first then is_first := false else Format.pp_print_newline fmt ();
    pp_v fmt v
  in
  iter pp_v m

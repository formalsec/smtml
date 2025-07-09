(* Dolmen's binders *)
type t =
  | Forall
  | Exists
  | Let_in

let equal a b =
  match (a, b) with
  | Forall, Forall | Exists, Exists | Let_in, Let_in -> true
  | (Forall | Exists | Let_in), _ -> false

let compare (e1 : t) (e2 : t) =
  match (e1, e2) with
  | x, y when equal x y -> 0
  | Forall, _ -> 1
  | _, Forall -> -1
  | Exists, _ -> 1
  | _, Exists -> -1
  | Let_in, _ -> 1

let pp fmt = function
  | Forall -> Fmt.string fmt "forall"
  | Exists -> Fmt.string fmt "exists"
  | Let_in -> Fmt.string fmt "let"

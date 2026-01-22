(* Dolmen's binders *)
type t =
  | Forall
  | Exists
  | Let_in
[@@deriving ord]

let equal a b =
  match (a, b) with
  | Forall, Forall | Exists, Exists | Let_in, Let_in -> true
  | (Forall | Exists | Let_in), _ -> false

let hash = function Forall -> 0 | Exists -> 1 | Let_in -> 2

let pp fmt = function
  | Forall -> Fmt.string fmt "forall"
  | Exists -> Fmt.string fmt "exists"
  | Let_in -> Fmt.string fmt "let"

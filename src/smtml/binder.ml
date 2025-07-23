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

let pp fmt = function
  | Forall -> Fmt.string fmt "forall"
  | Exists -> Fmt.string fmt "exists"
  | Let_in -> Fmt.string fmt "let"

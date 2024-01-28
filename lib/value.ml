open Ty

type t =
  | True
  | False
  | Int of int
  | Real of float
  | Str of string
  | Num of Num.t

let equal v1 v2 =
  match (v1, v2) with
  | True, True | False, False -> true
  | Int x1, Int x2 -> Int.equal x1 x2
  | Real x1, Real x2 -> Float.equal x1 x2
  | Str x1, Str x2 -> String.equal x1 x2
  | Num x1, Num x2 -> Num.(x1 = x2)
  | _ -> false

let compare v1 v2 =
  match (v1, v2) with
  | True, True | False, False -> 0
  | Int x1, Int x2 -> Int.compare x1 x2
  | Real x1, Real x2 -> Float.compare x1 x2
  | Str x1, Str x2 -> String.compare x1 x2
  | Num x1, Num x2 -> Num.compare x1 x2
  | _ -> compare v1 v2

let ty = function
  | True | False -> Ty_bool
  | Int _ -> Ty_int
  | Real _ -> Ty_real
  | Str _ -> Ty_str
  | Num n -> Num.ty n

let pp fmt v =
  let open Format in
  match v with
  | True -> pp_print_string fmt "true"
  | False -> pp_print_string fmt "false"
  | Int x -> pp_print_int fmt x
  | Real x -> fprintf fmt "%F" x
  | Num x -> Num.pp fmt x
  | Str x -> Format.fprintf fmt "%S" x

let pp_num fmt (v : t) =
  match v with Num x -> Num.pp_hex fmt x | _ -> pp fmt v

let to_string v = Format.asprintf "%a" pp v

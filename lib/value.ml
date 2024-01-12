open Ty

type t =
  | True
  | False
  | Int of int
  | Real of float
  | Str of string
  | Num of Num.t

let equal (v1 : t) (v2 : t) : Bool.t =
  match (v1, v2) with
  | True, True | False, False -> true
  | Int x1, Int x2 -> Int.equal x1 x2
  | Real x1, Real x2 -> Float.equal x1 x2
  | Str x1, Str x2 -> String.equal x1 x2
  | Num x1, Num x2 -> Num.(x1 = x2)
  | _ -> false

let type_of (v : t) : Ty.t =
  match v with
  | True | False -> Ty_bool
  | Int _ -> Ty_int
  | Real _ -> Ty_real
  | Str _ -> Ty_str
  | Num n -> Num.type_of n

let pp fmt (v : t) =
  let pp_string = Format.pp_print_string in
  match v with
  | True -> pp_string fmt "true"
  | False -> pp_string fmt "false"
  | Int x -> pp_string fmt @@ Int.to_string x
  | Real x -> pp_string fmt @@ Float.to_string x
  | Num x -> Num.pp fmt x
  | Str x -> Format.fprintf fmt {|"%s"|} x

let pp_num fmt (v : t) =
  match v with Num x -> Num.pp_hex fmt x | _ -> pp fmt v

let to_string v = Format.asprintf "%a" pp v

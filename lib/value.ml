open Types

type t =
  | Int of int
  | Real of float
  | Bool of bool
  | Str of string
  | Num of Num.t

let equal (v1 : t) (v2 : t) : Bool.t =
  match (v1, v2) with
  | Int x1, Int x2 -> Int.equal x1 x2
  | Real x1, Real x2 -> Float.equal x1 x2
  | Bool x1, Bool x2 -> Bool.equal x1 x2
  | Str x1, Str x2 -> String.equal x1 x2
  | Num x1, Num x2 -> Num.(x1 = x2)
  | _ -> false

let type_of (v : t) : expr_type =
  match v with
  | Int _ -> `IntType
  | Real _ -> `RealType
  | Bool _ -> `BoolType
  | Num n -> Num.type_of n
  | Str _ -> `StrType

let pp fmt (v : t) =
  match v with
  | Int x -> Format.pp_print_string fmt @@ Int.to_string x
  | Real x -> Format.pp_print_string fmt @@ Float.to_string x
  | Bool x -> Format.pp_print_string fmt @@ Bool.to_string x
  | Num x -> Format.fprintf fmt "%a" Num.pp x
  | Str x -> Format.fprintf fmt {|"%s"|} x

let pp_num fmt (v : t) =
  match v with Num x -> Num.pp_hex fmt x | _ -> pp fmt v

let to_string v = Format.asprintf "%a" pp v

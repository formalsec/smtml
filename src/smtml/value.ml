(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2024 formalsec *)
(* Written by the Smtml programmers *)

open Ty

type t =
  | True
  | False
  | Unit
  | Int of int
  | Real of float
  | Str of string
  | Num of Num.t
  | Bitv of Bitvector.t
  | List of t list
  | App : [> `Op of string ] * t list -> t
  | Nothing

let type_of (v : t) : Ty.t =
  match v with
  | True | False -> Ty_bool
  | Unit -> Ty_unit
  | Int _ -> Ty_int
  | Real _ -> Ty_real
  | Str _ -> Ty_str
  | Num n -> Num.type_of n
  | Bitv bv -> Ty_bitv (Bitvector.numbits bv)
  | List _ -> Ty_list
  | App _ -> Ty_app
  | Nothing -> Ty_none

let discr = function
  | True -> 0
  | False -> 1
  | Unit -> 2
  | Int _ -> 3
  | Real _ -> 4
  | Str _ -> 5
  | Num _ -> 6
  | Bitv _ -> 7
  | List _ -> 8
  | App _ -> 9
  | Nothing -> 10

(* Optimized mixer (DJB2 variant). Inlines to simple arithmetic. *)
let[@inline] combine h v = (h * 33) + v

let rec hash v =
  match v with
  | True -> 1
  | False -> 2
  | Unit -> 3
  | Int i -> combine 4 i
  | Real f -> combine 5 (Hashtbl.hash f)
  | Str s -> combine 6 (Hashtbl.hash s)
  | Num n -> combine 7 (Hashtbl.hash n)
  | Bitv b -> combine 8 (Bitvector.hash b)
  | List l -> List.fold_left (fun acc v -> combine acc (hash v)) 9 l
  | App (`Op s, args) ->
    let h = combine 10 (Hashtbl.hash s) in
    List.fold_left (fun acc v -> combine acc (hash v)) h args
  | Nothing -> 11
  | App _ -> assert false

let rec compare (a : t) (b : t) : int =
  match (a, b) with
  | True, True | False, False | Unit, Unit | Nothing, Nothing -> 0
  | False, True -> -1
  | True, False -> 1
  | Int a, Int b -> Int.compare a b
  | Real a, Real b -> Float.compare a b
  | Str a, Str b -> String.compare a b
  | Num a, Num b -> Num.compare a b
  | Bitv a, Bitv b -> Bitvector.compare a b
  | List a, List b -> List.compare compare a b
  | App (`Op op1, vs1), App (`Op op2, vs2) ->
    let c = String.compare op1 op2 in
    if c = 0 then List.compare compare vs1 vs2 else c
  | ( ( True | False | Unit | Int _ | Real _ | Str _ | Num _ | Bitv _ | List _
      | App _ | Nothing )
    , _ ) ->
    (* TODO: I don't know if this is always semantically correct *)
    Int.compare (discr a) (discr b)

let rec equal (v1 : t) (v2 : t) : bool =
  match (v1, v2) with
  | True, True | False, False | Unit, Unit | Nothing, Nothing -> true
  | Int a, Int b -> Int.equal a b
  | Real a, Real b -> Float.equal a b
  | Str a, Str b -> String.equal a b
  | Num a, Num b -> Num.equal a b
  | Bitv a, Bitv b -> Bitvector.equal a b
  | List l1, List l2 -> List.equal equal l1 l2
  | App (`Op op1, vs1), App (`Op op2, vs2) ->
    String.equal op1 op2 && List.equal equal vs1 vs2
  | ( ( True | False | Unit | Int _ | Real _ | Str _ | Num _ | Bitv _ | List _
      | App _ | Nothing )
    , _ ) ->
    false

let map v f = match v with Nothing -> Nothing | _ -> f v

let ( let+ ) = map

let default_of_type = function
  | Ty.Ty_bool -> False
  | Ty_int -> Int 0
  | Ty_real -> Real 0.0
  | Ty_str -> Str ""
  | Ty_bitv m -> Bitv (Bitvector.make Z.zero m)
  | Ty_fp 32 -> Num (F32 0l)
  | Ty_fp 64 -> Num (F64 0L)
  | Ty_list -> List []
  | Ty_unit -> Unit
  | Ty_none -> Nothing
  | (Ty_fp _ | Ty_app | Ty_regexp | Ty_roundingMode) as ty ->
    Fmt.failwith "No default value for type %a" Ty.pp ty

let rec pp fmt = function
  | True -> Fmt.string fmt "true"
  | False -> Fmt.string fmt "false"
  | Unit -> Fmt.string fmt "unit"
  | Int x -> Fmt.int fmt x
  | Real x -> Fmt.pf fmt "%F" x
  | Num x -> Num.pp fmt x
  | Bitv bv -> Bitvector.pp fmt bv
  | Str x -> Fmt.pf fmt "%S" x
  | List l -> (Fmt.hovbox ~indent:1 (Fmt.list ~sep:Fmt.comma pp)) fmt l
  | App (`Op op, vs) ->
    Fmt.pf fmt "@[<hov 1>%s(%a)@]" op (Fmt.list ~sep:Fmt.comma pp) vs
  | Nothing -> Fmt.string fmt "none"
  | App _ -> assert false

let to_string (v : t) : string = Fmt.str "%a" pp v

let of_string (cast : Ty.t) v =
  let open Result in
  match cast with
  | Ty_bitv m -> Ok (Bitv (Bitvector.make (Z.of_string v) m))
  | Ty_fp _ ->
    let+ n = Num.of_string cast v in
    Num n
  | Ty_bool -> (
    match v with
    | "true" -> Ok True
    | "false" -> Ok False
    | _ -> Fmt.error_msg "invalid value %s, expected boolean" v )
  | Ty_int -> (
    match int_of_string_opt v with
    | None -> Fmt.error_msg "invalid value %s, expected integer" v
    | Some n -> Ok (Int n) )
  | Ty_real -> (
    match float_of_string_opt v with
    | None -> Fmt.error_msg "invalid value %s, expected real" v
    | Some n -> Ok (Real n) )
  | Ty_str -> Ok (Str v)
  | Ty_app | Ty_list | Ty_none | Ty_unit | Ty_regexp | Ty_roundingMode ->
    Fmt.error_msg "unsupported parsing values of type %a" Ty.pp cast

let rec to_json (v : t) : Yojson.Basic.t =
  match v with
  | True -> `Bool true
  | False -> `Bool false
  | Unit -> `String "unit"
  | Int int -> `Int int
  | Real real -> `Float real
  | Str str -> `String str
  | Num n -> Num.to_json n
  | Bitv bv -> Bitvector.to_json bv
  | List l -> `List (List.map to_json l)
  | Nothing -> `Null
  | App _ -> assert false

module Smtlib = struct
  let pp fmt = function
    | True -> Fmt.string fmt "true"
    | False -> Fmt.string fmt "false"
    | Int x -> Fmt.int fmt x
    | Real x -> Fmt.pf fmt "%F" x
    | Num x -> Num.pp fmt x
    | Bitv bv -> Bitvector.pp fmt bv
    | Str x -> Fmt.pf fmt "%S" x
    | Unit -> assert false
    | List _ -> assert false
    | App _ -> assert false
    | Nothing -> assert false
end

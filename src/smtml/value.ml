(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2026 formalsec *)
(* Written by the Smtml programmers *)

open Ty

type t =
  | True
  | False
  | Unit
  | Int of Z.t
  | Real of float
  | Str of string
  | Num of Num.t
  | Bitv of Bitvector.t
  | List of t list
  | App : [> `Op of string ] * t list -> t
  | Array of
      { ty : Ty.t
      ; default : t
      ; entries : (t * t) list
      }
  | Re_none
  | Re_all
  | Re_allchar
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
  | Array { ty; _ } -> ty
  | Re_none | Re_all | Re_allchar -> Ty_regexp
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
  | Re_none -> 10
  | Re_all -> 11
  | Re_allchar -> 12
  | Nothing -> 13
  | Array _ -> 14

(* Optimized mixer (DJB2 variant). Inlines to simple arithmetic. *)
let[@inline] combine h v = (h * 33) + v

let rec hash v =
  match v with
  | True -> 1
  | False -> 2
  | Unit -> 3
  | Int i -> combine 4 (Z.hash i)
  | Real f -> combine 5 (Float.hash f)
  | Str s -> combine 6 (String.hash s)
  | Num n -> combine 7 (Num.hash n)
  | Bitv b -> combine 8 (Bitvector.hash b)
  | List l -> List.fold_left (fun acc v -> combine acc (hash v)) 9 l
  | App (`Op s, args) ->
    let h = combine 10 (String.hash s) in
    List.fold_left (fun acc v -> combine acc (hash v)) h args
  | Re_none -> 11
  | Re_all -> 12
  | Re_allchar -> 13
  | Nothing -> 14
  | Array { ty; default; entries } ->
    let h = combine (combine 15 (Ty.hash ty)) (hash default) in
    List.fold_left
      (fun acc (i, v) -> combine acc (combine (hash i) (hash v)))
      h entries
  | App _ -> assert false

let rec compare_entry (i1, v1) (i2, v2) =
  let c = compare i1 i2 in
  if c <> 0 then c else compare v1 v2

and compare (a : t) (b : t) : int =
  match (a, b) with
  | True, True | False, False | Unit, Unit | Nothing, Nothing -> 0
  | Re_none, Re_none | Re_all, Re_all | Re_allchar, Re_allchar -> 0
  | False, True -> -1
  | True, False -> 1
  | Int a, Int b -> Z.compare a b
  | Real a, Real b -> Float.compare a b
  | Str a, Str b -> String.compare a b
  | Num a, Num b -> Num.compare a b
  | Bitv a, Bitv b -> Bitvector.compare a b
  | List a, List b -> List.compare compare a b
  | App (`Op op1, vs1), App (`Op op2, vs2) ->
    let c = String.compare op1 op2 in
    if c = 0 then List.compare compare vs1 vs2 else c
  | ( Array { ty = ty1; default = d1; entries = e1 }
    , Array { ty = ty2; default = d2; entries = e2 } ) ->
    let c = Ty.compare ty1 ty2 in
    if c <> 0 then c
    else
      let c = compare d1 d2 in
      if c <> 0 then c else List.compare compare_entry e1 e2
  | ( ( True | False | Unit | Int _ | Real _ | Str _ | Num _ | Bitv _ | List _
      | App _ | Array _ | Re_none | Re_all | Re_allchar | Nothing )
    , _ ) ->
    (* TODO: I don't know if this is always semantically correct *)
    Int.compare (discr a) (discr b)

let rec equal (v1 : t) (v2 : t) : bool =
  match (v1, v2) with
  | True, True | False, False | Unit, Unit | Nothing, Nothing -> true
  | Re_none, Re_none | Re_all, Re_all | Re_allchar, Re_allchar -> true
  | Int a, Int b -> Z.equal a b
  | Real a, Real b -> Float.equal a b
  | Str a, Str b -> String.equal a b
  | Num a, Num b -> Num.equal a b
  | Bitv a, Bitv b -> Bitvector.equal a b
  | List l1, List l2 -> List.equal equal l1 l2
  | App (`Op op1, vs1), App (`Op op2, vs2) ->
    String.equal op1 op2 && List.equal equal vs1 vs2
  | ( Array { ty = ty1; default = d1; entries = e1 }
    , Array { ty = ty2; default = d2; entries = e2 } ) ->
    Ty.equal ty1 ty2 && equal d1 d2
    && List.equal (fun (i1, v1) (i2, v2) -> equal i1 i2 && equal v1 v2) e1 e2
  | ( ( True | False | Unit | Int _ | Real _ | Str _ | Num _ | Bitv _ | List _
      | App _ | Array _ | Re_none | Re_all | Re_allchar | Nothing )
    , _ ) ->
    false

let array ty ~default entries =
  (* Keep the first binding of each index (since the outer stores appear first,
     the first binding of an index shadows/replaces the other ones), drop
     bindings that are equal to the default, and sort the rest by index so that
     semantically equivalent arrays have the same representation. *)
  let entries =
    List.fold_left
      (fun acc (i, v) ->
        if List.exists (fun (i', _) -> equal i i') acc then acc
        else (i, v) :: acc )
      [] entries
    |> List.filter (fun (_, v) -> not (equal v default))
    |> List.sort compare_entry
  in
  Array { ty; default; entries }

let map v f = match v with Nothing -> Nothing | _ -> f v

let ( let+ ) = map

let rec default_of_type = function
  | Ty.Ty_bool -> False
  | Ty_int -> Int Z.zero
  | Ty_real -> Real 0.0
  | Ty_str -> Str ""
  | Ty_bitv m -> Bitv (Bitvector.make Z.zero m)
  | Ty_fp 32 -> Num (F32 0l)
  | Ty_fp 64 -> Num (F64 0L)
  | Ty_list -> List []
  | Ty_unit -> Unit
  | Ty_none -> Nothing
  | Ty_regexp -> Re_none
  | Ty_array (_, elem) as ty -> array ty ~default:(default_of_type elem) []
  | (Ty_fp _ | Ty_app | Ty_roundingMode) as ty ->
    Fmt.failwith "No default value for type %a" Ty.pp ty

let rec pp_with ~printer fmt = function
  | True -> Fmt.string fmt "true"
  | False -> Fmt.string fmt "false"
  | Unit -> Fmt.string fmt "unit"
  | Int x -> Z.pp_print fmt x
  | Real x -> Fmt.pf fmt "%F" x
  | Num x -> Num.pp_with ~printer fmt x
  | Bitv bv -> Bitvector.pp_with ~printer fmt bv
  | Str x -> Fmt.pf fmt "%S" x
  | List l ->
    (Fmt.hovbox ~indent:1 (Fmt.list ~sep:Fmt.comma (pp_with ~printer))) fmt l
  | App (`Op op, vs) ->
    Fmt.pf fmt "@[<hov 1>%s(%a)@]" op
      (Fmt.list ~sep:Fmt.comma (pp_with ~printer))
      vs
  | Array { default; entries; _ } ->
    let pp_entry fmt (i, v) =
      Fmt.pf fmt "%a -> %a;@ " (pp_with ~printer) i (pp_with ~printer) v
    in
    Fmt.pf fmt "@[<hov 1>[%a_ -> %a]@]"
      (Fmt.list ~sep:Fmt.nop pp_entry)
      entries (pp_with ~printer) default
  | Re_none -> Fmt.string fmt "re.none"
  | Re_all -> Fmt.string fmt "re.all"
  | Re_allchar -> Fmt.string fmt "re.allchar"
  | Nothing -> Fmt.string fmt "none"
  | App _ -> assert false

let pp fmt v = pp_with ~printer:Without_type fmt v

let pp_safe fmt v = pp_with ~printer:With_type_and_hexa_float fmt v

let to_string (v : t) : string = Fmt.str "%a" pp v

let of_string (cast : Ty.t) v =
  let open Result.Syntax in
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
    try Ok (Int (Z.of_string v))
    with _ -> Fmt.error_msg "invalid value %s, expected integer" v )
  | Ty_real -> (
    match float_of_string_opt v with
    | None -> Fmt.error_msg "invalid value %s, expected real" v
    | Some n -> Ok (Real n) )
  | Ty_str -> Ok (Str v)
  | Ty_regexp -> (
    match v with
    | "re.none" -> Ok Re_none
    | "re.all" -> Ok Re_all
    | "re.allchar" -> Ok Re_allchar
    | _ -> Fmt.error_msg "invalid value %s, expected regular expression" v )
  | Ty_app | Ty_array _ | Ty_list | Ty_none | Ty_unit | Ty_roundingMode ->
    Fmt.error_msg "unsupported parsing values of type %a" Ty.pp cast

let rec to_json (v : t) : Yojson.Safe.t =
  match v with
  | True -> `Bool true
  | False -> `Bool false
  | Unit -> `String "unit"
  | Int z -> `Intlit (Z.to_string z)
  | Real real -> `Float real
  | Str str -> `String str
  | Num n -> Num.to_json n
  | Bitv bv -> Bitvector.to_json bv
  | List l -> `List (List.map to_json l)
  | Array { default; entries; _ } ->
    `Assoc
      [ ("default", to_json default)
      ; ( "entries"
        , `List
            (List.map (fun (i, v) -> `List [ to_json i; to_json v ]) entries) )
      ]
  | Re_none -> `String "re.none"
  | Re_all -> `String "re.all"
  | Re_allchar -> `String "re.allchar"
  | Nothing -> `Null
  | App _ -> assert false

module Smtlib = struct
  let rec pp fmt = function
    | True -> Fmt.string fmt "true"
    | False -> Fmt.string fmt "false"
    | Int x -> Z.pp_print fmt x
    | Real x -> Fmt.pf fmt "%F" x
    | Num x -> Num.pp_safe fmt x
    | Bitv bv -> Bitvector.pp_safe fmt bv
    | Str x -> Fmt.pf fmt "%S" x
    | Re_none -> Fmt.string fmt "re.none"
    | Re_all -> Fmt.string fmt "re.all"
    | Re_allchar -> Fmt.string fmt "re.allchar"
    | Array { ty; default; entries } ->
      let rec pp_stores fmt = function
        | [] -> Fmt.pf fmt "((as const %a) %a)" Ty.Smtlib.pp ty pp default
        | (i, v) :: entries ->
          Fmt.pf fmt "(store %a %a %a)" pp_stores entries pp i pp v
      in
      pp_stores fmt entries
    | Unit -> assert false
    | List _ -> assert false
    | App _ -> assert false
    | Nothing -> assert false
end

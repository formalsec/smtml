(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2026 formalsec *)
(* Written by the Smtml programmers *)

open Smtml

module Make (M : Mappings_intf.S_with_fresh) = struct
  open Expr

  let sym ty name = symbol (Symbol.make_const ty name)

  let bv v = value (Bitv (Bitvector.make (Z.of_int v) 32))

  let int v = value (Int (Z.of_int v))

  let f32 v = value (Num (F32 (Int32.bits_of_float v)))

  let bv32, fp32 = (Ty.Ty_bitv 32, Ty.Ty_fp 32)

  let x, y, z = (sym Ty_bool "x", sym Ty_bool "y", sym Ty_bool "z")

  let a, b, c, d = (sym bv32 "a", sym bv32 "b", sym bv32 "c", sym bv32 "d")

  let i, j = (sym Ty_int "i", sym Ty_int "j")

  let f, g = (sym fp32 "f", sym fp32 "g")

  let s1, s2, s3 = (sym Ty_str "s1", sym Ty_str "s2", sym Ty_str "s3")

  let w = sym (Ty_bitv 8) "w"

  let not_ = raw_unop Ty_bool Not

  let ite = raw_triop Ty_bool Ite

  let concat = raw_naryop Ty_str Concat

  (* Terms that a rule rewrites *)
  let simplified =
    [ not_ (not_ x)
    ; raw_unop bv32 Neg (raw_unop bv32 Neg a)
    ; raw_unop fp32 Neg (raw_unop fp32 Neg f)
    ; not_ (raw_relop bv32 Ne a b)
    ; not_ (raw_relop fp32 Eq f g)
    ; not_ (raw_relop Ty_bool Eq f g)
    ; not_ (raw_relop bv32 Lt a b)
    ; not_ (raw_relop Ty_int Le i j)
    ; raw_binop Ty_bool And (value True) x
    ; raw_binop Ty_bool And x (value True)
    ; raw_binop bv32 Add (bv 0) a
    ; raw_binop bv32 Or a (bv 0)
    ; raw_binop bv32 And (bv 0) a
    ; raw_binop bv32 Mul a (bv 0)
    ; raw_binop bv32 Mul (bv 1) a
    ; raw_binop bv32 Mul a (bv 1)
    ; raw_binop bv32 Add (raw_binop bv32 Add a (bv 1)) (bv 2)
    ; raw_binop Ty_int Sub (raw_binop Ty_int Sub i (int 1)) (int 2)
    ; raw_binop bv32 Mul (raw_binop bv32 Mul a (bv 3)) (bv 5)
    ; raw_binop Ty_int Add (int 1) (raw_binop Ty_int Add i (int 2))
    ; raw_binop bv32 Mul (bv 3) (raw_binop bv32 Mul a (bv 5))
    ; ite (value True) a b
    ; ite (value False) a b
    ; ite x (ite y a b) (ite z c d)
    ; raw_extract w ~high:7 ~low:0
    ; concat [ concat [ s1; s2 ]; s3 ]
    ; concat [ s1; concat [ s2; s3 ] ]
    ]

  let not_simplified =
    [ not_ (raw_relop fp32 Lt f g)
    ; not_ (raw_relop fp32 Le f g)
    ; raw_binop fp32 Add (raw_binop fp32 Add f (f32 1.)) (f32 1.)
    ; raw_binop fp32 Mul (raw_binop fp32 Mul f (f32 3.)) (f32 5.)
    ; raw_extract w ~high:0 ~low:0
    ]

  let check_rewritten e () =
    let e' = simplify_with_rules e in
    if equal e e' then Alcotest.fail "not rewritten";
    let module S = Solver.Batch (M.Fresh.Make ()) in
    match S.check (S.create ()) [ raw_relop Ty_bool Ne e e' ] with
    | `Unsat -> ()
    | `Sat | `Unknown -> Alcotest.failf "may differ from %a" pp e'

  let check_kept e () =
    let e' = simplify_with_rules e in
    if not (equal e e') then Alcotest.failf "rewritten to %a" pp e'

  let test =
    let case check e =
      Alcotest.test_case (Fmt.str "%a" pp e) `Quick (check e)
    in
    ( "test_simplifier_rules"
    , List.map (case check_rewritten) simplified
      @ List.map (case check_kept) not_simplified )
end

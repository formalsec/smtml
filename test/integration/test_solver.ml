(* SPDX-License-Identifier: MIT *)
(* Copyright (C) 2023-2026 formalsec *)
(* Written by the Smtml programmers *)

open Smtml

module Make (M : Mappings_intf.S_with_fresh) = struct
  open Smtml_test.Test_harness
  module Cached = Solver.Cached (M)

  let with_solver f () =
    let module Mappings : Mappings_intf.S = M.Fresh.Make () in
    let solver_module =
      (module Smtml.Solver.Incremental (Mappings) : Solver_intf.S)
    in
    f solver_module

  let test_default_params _solver_module =
    Alcotest.(check int)
      "default timeout"
      Int32.(to_int max_int)
      (Params.default_value Timeout);
    Alcotest.(check bool) "default model" true (Params.default_value Model);
    Alcotest.(check bool)
      "default unsat_core" false
      (Params.default_value Unsat_core);
    Alcotest.(check bool)
      "default ematching" true
      (Params.default_value Ematching);
    Alcotest.(check int)
      "default random_seed" 0
      (Params.default_value Random_seed)

  let test_solver_params solver_module =
    let module Solver = (val solver_module : Solver_intf.S) in
    let params =
      Params.(
        default () $ (Timeout, 900) $ (Model, false) $ (Unsat_core, true)
        $ (Ematching, false) $ (Parallel, true) $ (Num_threads, 1)
        $ (Debug, false) $ (Random_seed, 1227) )
    in
    Alcotest.(check bool)
      "params unsat_core" true
      (Params.get params Unsat_core);
    let _ : Solver.t = Solver.create ~params () in
    ()

  let test_params =
    ( "test_params"
    , [ Alcotest.test_case "test_default_params" `Quick
          (with_solver test_default_params)
      ; Alcotest.test_case "test_solver_params" `Quick
          (with_solver test_solver_params)
      ] )

  let test_cache_hits _solver_module =
    let solver = Cached.create ~logic:LIA () in
    let x = Infix.symbol "x" Ty_int in
    let c = Infix.(Int.(int 0 <= x)) in
    let get_stat key =
      let stats = Cached.get_statistics solver in
      let stat = Statistics.Map.find_opt key stats in
      match stat with
      | Some (`Int s) -> s
      | _ -> Alcotest.failf "%s should exist and be an int in stats" key
    in
    Alcotest.(check int) "cache hits initial" 0 (get_stat "cache hits");
    assert_sat (Cached.check_set solver @@ Expr.Set.singleton c);
    assert_sat (Cached.check_set solver @@ Expr.Set.singleton c);
    assert_sat (Cached.check_set solver @@ Expr.Set.singleton c);
    Alcotest.(check int) "cache misses" 1 (get_stat "cache misses");
    Alcotest.(check int) "cache hits" 2 (get_stat "cache hits")

  let test_cache_get_model _solver_module =
    let open Infix in
    let solver = Cached.create ~logic:LIA () in
    let x = symbol "x" Ty_int in
    let set = Expr.Set.of_list Int.[ int 0 <= x; x < int 10 ] in
    Alcotest.(check bool)
      "cache get model"
      ( match Cached.get_sat_model solver set with
      | `Model _ -> true
      | `Unsat | `Unknown -> false )
      true

  let test_cached =
    ( "test_cached"
    , [ Alcotest.test_case "test_cache_hits" `Quick
          (with_solver test_cache_hits)
      ; Alcotest.test_case "test_cache_get_model" `Quick
          (with_solver test_cache_get_model)
      ] )

  let test_lia_0 solver_module =
    let open Infix in
    let module Solver = (val solver_module : Solver_intf.S) in
    let solver = Solver.create ~logic:LIA () in
    let symbol_x = Symbol.("x" @: Ty_int) in
    let x = Expr.symbol symbol_x in
    assert_sat ~f:"test" (Solver.check solver []);

    Solver.push solver;
    Solver.add solver Int.[ int 0 <= x ];
    assert_sat (Solver.check solver []);
    check (Solver.get_value solver x) (int 0);
    Solver.pop solver 1;

    Solver.push solver;
    Solver.add solver [ x = int 3 ];
    assert_sat ~f:"test" (Solver.check solver []);
    check (Solver.get_value solver Int.(x * x)) (int 9);
    Solver.pop solver 1;

    Solver.push solver;
    Solver.add solver Int.[ int 0 <= x || x < int 0 ];
    assert_sat ~f:"test" (Solver.check solver []);
    let model = Solver.model ~symbols:[ symbol_x ] solver in
    let val_x = Option.bind model (fun m -> Model.evaluate m symbol_x) in
    Alcotest.(check bool) "x has value" true (Option.is_some val_x);
    Solver.pop solver 1;

    Solver.add solver [ x = int 5 ];
    assert_sat (Solver.check solver []);
    let model = Solver.model solver in
    let val_x = Option.bind model (fun m -> Model.evaluate m symbol_x) in
    Alcotest.(check bool)
      "x = 5"
      (match val_x with Some v -> Value.equal v (Int 5) | None -> false)
      true

  let test_distinct solver_module =
    let open Typed in
    let module Solver = (val solver_module : Solver_intf.S) in
    let solver = Solver.create ~logic:LIA () in
    let x = symbol Types.int "x" in
    let y = symbol Types.int "y" in
    let z = symbol Types.int "z" in
    Solver.add solver [ (Bool.distinct [ x; y; z ] :> Expr.t) ];
    Solver.add solver [ (Bool.eq x (Int.v 1) :> Expr.t) ];
    Solver.add solver [ (Bool.eq y (Int.v 1) :> Expr.t) ];
    assert_unsat ~f:"test_distinct_unsat" (Solver.check solver []);
    Solver.reset solver;
    let x = symbol Types.int "x" in
    let y = symbol Types.int "y" in
    let z = symbol Types.int "z" in
    Solver.add solver [ (Bool.distinct [ x; y; z ] :> Expr.t) ];
    Solver.add solver [ (Bool.eq x (Int.v 1) :> Expr.t) ];
    Solver.add solver [ (Bool.eq y (Int.v 2) :> Expr.t) ];
    Solver.add solver [ (Bool.eq z (Int.v 3) :> Expr.t) ];
    assert_sat ~f:"test_distinct_sat" (Solver.check solver [])

  let test_lia_1 solver_module =
    let open Infix in
    let module Solver = (val solver_module : Solver_intf.S) in
    let solver = Solver.create ~logic:QF_LIA () in
    let a = symbol "a" Ty_int in
    Solver.add solver Int.[ a + int 1 = int 2 => ((a * int 2) + int 2 = int 4) ];
    assert_sat ~f:"test_lia" (Solver.check solver [])

  let test_lia =
    ( "test_lia"
    , [ Alcotest.test_case "test_lia_0" `Quick (with_solver test_lia_0)
      ; Alcotest.test_case "test_lia_1" `Quick (with_solver test_lia_1)
      ; Alcotest.test_case "test_distinct" `Quick (with_solver test_distinct)
      ] )

  let test_lra =
    ( "test_lra"
    , [ Alcotest.test_case "test_lra" `Quick
          (with_solver (fun solver_module ->
             let module Solver = (val solver_module : Solver_intf.S) in
             let solver = Solver.create () in
             assert_sat ~f:"test_lra"
               (let x = Expr.symbol Symbol.("x" @: Ty_real) in
                let y = Expr.symbol Symbol.("y" @: Ty_real) in
                let c0 = Expr.relop Ty_bool Eq x y in
                let c1 =
                  Expr.relop Ty_bool Eq
                    (Expr.cvtop Ty_real ToString x)
                    (Expr.cvtop Ty_real ToString y)
                in
                Solver.check solver [ c0; c1 ] ) ) )
      ] )

  let test_bv_8 solver_module =
    let open Infix in
    let module Solver = (val solver_module : Solver_intf.S) in
    let solver = Solver.create ~params:(Params.default ()) ~logic:QF_BVFP () in
    let ty = Ty.Ty_bitv 8 in
    let x = symbol "h" ty in
    Solver.add solver
      [ Expr.relop ty Lt (int8 0) x; Expr.relop ty Lt x (int8 2) ];
    assert_sat ~f:"test_bv_8" (Solver.check solver []);
    check (Solver.get_value solver x) (int8 1)

  let test_bv_32 solver_module =
    let open Infix in
    let module Solver = (val solver_module : Solver_intf.S) in
    let solver = Solver.create ~params:(Params.default ()) ~logic:QF_BVFP () in
    let ty = Ty.Ty_bitv 32 in
    let x = symbol "x" ty in
    let y = symbol "y" ty in
    let z = symbol "z" ty in
    let w = symbol "w" ty in
    Solver.add solver
      [ Expr.relop ty Lt (int32 0l) x && Expr.relop ty Lt w (int32 5l)
      ; Expr.relop ty Lt x y && Expr.relop ty Lt y z && Expr.relop ty Lt z w
      ];
    assert_sat ~f:"test_bv_32" (Solver.check solver []);
    Alcotest.(check bool)
      "model exists"
      (match Solver.model solver with None -> false | Some _m -> true)
      true

  let test_arbitrary_bv solver_module =
    let open Infix in
    let module Solver = (val solver_module : Solver_intf.S) in
    let solver = Solver.create ~params:(Params.default ()) ~logic:QF_BVFP () in
    for i = 1 to 64 do
      let ty = Ty.Ty_bitv i in
      let x = symbol ("x" ^ string_of_int i) ty in
      Solver.add solver
        [ Expr.relop ty Eq x (Expr.value (Bitv (Bitvector.make (Z.of_int i) i)))
        ]
    done;
    assert_sat ~f:"test_arbitrary_bv" (Solver.check solver [])

  let test_bv_rotate solver_module =
    let open Typed in
    let module Solver = (val solver_module : Solver_intf.S) in
    let solver = Solver.create ~params:(Params.default ()) ~logic:QF_BVFP () in
    let x = symbol Types.bitv8 "rotate_x" in
    let input = Bitv8.of_int 0x36 in
    let rotated_left = Bitv8.rotate_left 3 x in
    let rotated_right = Bitv8.rotate_right 3 x in
    Solver.add solver
      [ (Bool.eq x input :> Expr.t)
      ; (Bool.eq rotated_left (Bitv8.of_int 0xB1) :> Expr.t)
      ; (Bool.eq rotated_right (Bitv8.of_int 0xC6) :> Expr.t)
      ];
    assert_sat ~f:"test_bv_rotate" (Solver.check solver []);
    Solver.add solver [ (Bool.eq rotated_left (Bitv8.of_int 0xB0) :> Expr.t) ];
    assert_unsat ~f:"test_bv_rotate_inconsistent" (Solver.check solver [])

  let test_bv_ext_rotate solver_module =
    let open Typed in
    let module Solver = (val solver_module : Solver_intf.S) in
    let solver = Solver.create ~params:(Params.default ()) ~logic:QF_BVFP () in
    let x = symbol Types.bitv8 "ext_rotate_x" in
    let shift = symbol Types.bitv8 "ext_rotate_shift" in
    let rotated_left = Bitv8.ext_rotate_left x shift in
    let rotated_right = Bitv8.ext_rotate_right x shift in
    Solver.add solver
      [ (Bool.eq x (Bitv8.of_int 0x36) :> Expr.t)
      ; (Bool.eq shift (Bitv8.of_int 3) :> Expr.t)
      ; (Bool.eq rotated_left (Bitv8.of_int 0xB1) :> Expr.t)
      ; (Bool.eq rotated_right (Bitv8.of_int 0xC6) :> Expr.t)
      ];
    assert_sat ~f:"test_bv_ext_rotate" (Solver.check solver []);
    Solver.add solver [ (Bool.eq rotated_right (Bitv8.of_int 0xC7) :> Expr.t) ];
    assert_unsat ~f:"test_bv_ext_rotate_inconsistent" (Solver.check solver [])

  let test_bv =
    ( "test_bv"
    , [ Alcotest.test_case "test_bv_8" `Quick (with_solver test_bv_8)
      ; Alcotest.test_case "test_bv_32" `Quick (with_solver test_bv_32)
      ; Alcotest.test_case "test_arbitrary_bv" `Quick
          (with_solver test_arbitrary_bv)
      ; Alcotest.test_case "test_bv_rotate" `Quick (with_solver test_bv_rotate)
      ] )

  let test_fp_get_value32 solver_module =
    let open Infix in
    let module Solver = (val solver_module : Solver_intf.S) in
    let solver = Solver.create ~logic:QF_BVFP () in
    let ty = Ty.Ty_fp 32 in
    let x = symbol "x" ty in
    let const = float32 50.0 in
    Solver.add solver [ Expr.relop ty Eq x const ];
    assert_sat ~f:"test_fp_get_value32" (Solver.check solver []);
    check (Solver.get_value solver x) const

  let test_fp_get_value64 solver_module =
    let open Infix in
    let module Solver = (val solver_module : Solver_intf.S) in
    let solver = Solver.create ~logic:QF_BVFP () in
    let ty = Ty.Ty_fp 64 in
    let x = symbol "x" ty in
    let const = float64 50.0 in
    Solver.add solver [ Expr.relop ty Eq x const ];
    assert_sat ~f:"test_fp_get_value64" (Solver.check solver []);
    check (Solver.get_value solver x) const

  let test_fp_sqrt solver_module =
    let open Infix in
    let module Solver = (val solver_module : Solver_intf.S) in
    let solver = Solver.create ~logic:QF_BVFP () in
    let ty = Ty.Ty_fp 32 in
    let x = symbol "x" ty in
    Solver.add solver [ Expr.relop ty Eq x (float32 4.0) ];
    Solver.add solver [ Expr.relop ty Eq (Expr.unop ty Sqrt x) (float32 2.0) ];
    assert_sat ~f:"test_fp_sqrt" (Solver.check solver []);
    check (Solver.get_value solver x) (float32 4.0)

  let test_fp_copysign32 solver_module =
    let open Infix in
    let module Solver = (val solver_module : Solver_intf.S) in
    let solver = Solver.create ~logic:QF_BVFP () in
    let ty = Ty.Ty_fp 32 in
    let x = symbol "x" ty in
    let y = symbol "y" ty in
    Solver.add solver
      [ Expr.relop ty Lt (float32 0.0) x && Expr.relop ty Lt y (float32 0.0)
      ; Expr.relop ty Lt (Expr.binop ty Copysign x y) (float32 0.0)
      ];
    assert_sat ~f:"test_copysign32" (Solver.check solver [])

  let test_fp_copysign64 solver_module =
    let open Infix in
    let module Solver = (val solver_module : Solver_intf.S) in
    let solver = Solver.create ~logic:QF_BVFP () in
    let ty = Ty.Ty_fp 64 in
    let x = symbol "x" ty in
    let y = symbol "y" ty in
    Solver.add solver
      [ Expr.relop ty Lt (float64 0.0) x && Expr.relop ty Lt y (float64 0.0)
      ; Expr.relop ty Lt (Expr.binop ty Copysign x y) (float64 0.0)
      ];
    assert_sat ~f:"test_copysign64" (Solver.check solver [])

  let test_to_ieee_bv solver_module =
    let open Infix in
    let module Solver = (val solver_module : Solver_intf.S) in
    let solver =
      Solver.create ~params:(Params.default ()) ~logic:Logic.QF_UFBV ()
    in
    let x = symbol "x" (Ty_fp 32) in
    let y = symbol "y" (Ty_bitv 32) in
    let converted = Expr.cvtop (Ty_bitv 32) Reinterpret_float x in
    Solver.add solver [ Expr.relop Ty_bool Eq converted y ];
    assert_sat ~f:"test_i32_of_f32" (Solver.check solver [])

  let test_fp =
    ( "test_fp"
    , [ Alcotest.test_case "test_fp_get_value32" `Quick
          (with_solver test_fp_get_value32)
      ; Alcotest.test_case "test_fp_get_value64" `Quick
          (with_solver test_fp_get_value64)
      ; Alcotest.test_case "test_fp_sqrt" `Quick (with_solver test_fp_sqrt)
      ; Alcotest.test_case "test_fp_copysign32" `Quick
          (with_solver test_fp_copysign32)
      ; Alcotest.test_case "test_fp_copysign64" `Quick
          (with_solver test_fp_copysign64)
      ; Alcotest.test_case "test_to_ieee_bv" `Quick
          (with_solver test_to_ieee_bv)
      ] )

  let test_regexp_allchar solver_module =
    let open Typed in
    let module Solver = (val solver_module : Solver_intf.S) in
    let solver = Solver.create () in
    let s = symbol Types.string "s" in
    let allchar = String.Re.allchar in
    Solver.add solver [ (String.in_re s allchar :> Expr.t) ];
    assert_sat ~f:"test_re_allchar" (Solver.check solver []);
    let model = Solver.model solver in
    let val_s =
      Option.bind model (fun m -> Model.evaluate m (Symbol.make Ty_str "s"))
    in
    Alcotest.(check bool)
      "allchar length 1"
      ( match val_s with
      | Some (Str s) -> Stdlib.String.length s = 1
      | _ -> false )
      true

  let test_regexp_diff solver_module =
    let open Typed in
    let module Solver = (val solver_module : Solver_intf.S) in
    let solver = Solver.create () in
    let s = symbol Types.string "s" in
    let re_az = String.(Re.range (v "a") (v "z")) in
    let re_a = String.(to_re (v "a")) in
    let re_not_a = String.Re.diff re_az re_a in
    Solver.add solver [ (String.in_re s re_not_a :> Expr.t) ];
    assert_sat ~f:"test_re_diff" (Solver.check solver []);
    let model = Solver.model solver in
    let val_s =
      Option.bind model (fun m -> Model.evaluate m (Symbol.make Ty_str "s"))
    in
    Alcotest.(check bool)
      "re_diff: not a"
      ( match val_s with
      | Some (Str s) -> Stdlib.String.length s = 1 && s <> "a"
      | _ -> false )
      true

  let test_regexp_diff_unsat solver_module =
    let open Typed in
    let module Solver = (val solver_module : Solver_intf.S) in
    let solver = Solver.create () in
    let s = symbol Types.string "s" in
    let re_a = String.(to_re (v "a")) in
    let re_diff_a_a = String.Re.diff re_a re_a in
    Solver.add solver [ (String.in_re s re_diff_a_a :> Expr.t) ];
    assert_unsat ~f:"test_re_diff_unsat" (Solver.check solver [])

  let test_regexp_concat solver_module =
    let open Typed in
    let module Solver = (val solver_module : Solver_intf.S) in
    let solver = Solver.create () in
    let s = symbol Types.string "s" in
    let re_a = String.(to_re (v "a")) in
    let re_b = String.(to_re (v "b")) in
    let re_ab = String.Re.concat [ re_a; re_b ] in
    Solver.add solver [ (String.in_re s re_ab :> Expr.t) ];
    assert_sat ~f:"test_re_concat" (Solver.check solver []);
    let model = Solver.model solver in
    let val_s =
      Option.bind model (fun m -> Model.evaluate m (Symbol.make Ty_str "s"))
    in
    Alcotest.(check bool)
      "re_concat: 'ab'"
      (match val_s with Some (Str "ab") -> true | _ -> false)
      true

  let test_regexp_union solver_module =
    let open Typed in
    let module Solver = (val solver_module : Solver_intf.S) in
    let solver = Solver.create () in
    let s = symbol Types.string "s" in
    let re_a = String.(to_re (v "a")) in
    let re_b = String.(to_re (v "b")) in
    let re_a_or_b = String.Re.union [ re_a; re_b ] in
    Solver.add solver [ (String.in_re s re_a_or_b :> Expr.t) ];
    assert_sat ~f:"test_re_union" (Solver.check solver []);
    let model = Solver.model solver in
    let val_s =
      Option.bind model (fun m -> Model.evaluate m (Symbol.make Ty_str "s"))
    in
    Alcotest.(check bool)
      "re_union: 'a' or 'b'"
      (match val_s with Some (Str "a") | Some (Str "b") -> true | _ -> false)
      true

  let test_regexp_star solver_module =
    let open Typed in
    let module Solver = (val solver_module : Solver_intf.S) in
    let solver = Solver.create () in
    let s = symbol Types.string "s" in
    let re_a = String.(to_re (v "a")) in
    let re_a_star = String.Re.star re_a in
    Solver.add solver [ (String.in_re s re_a_star :> Expr.t) ];
    Solver.add solver [ (Bool.eq (String.length s) (Int.v 3) :> Expr.t) ];
    assert_sat ~f:"test_re_star" (Solver.check solver []);
    let model = Solver.model solver in
    let val_s =
      Option.bind model (fun m -> Model.evaluate m (Symbol.make Ty_str "s"))
    in
    Alcotest.(check bool)
      "re_star: 'aaa'"
      (match val_s with Some (Str "aaa") -> true | _ -> false)
      true

  let test_regexp_complex solver_module =
    let open Typed in
    let module Solver = (val solver_module : Solver_intf.S) in
    let solver = Solver.create () in
    let s = symbol Types.string "s" in
    let re_a = String.(to_re (v "a")) in
    let re_b = String.(to_re (v "b")) in
    let re =
      String.(
        Re.concat
          [ Re.star (Re.union [ re_a; re_b ])
          ; to_re (v "a")
          ; to_re (v "b")
          ; to_re (v "b")
          ] )
    in
    Solver.add solver [ (String.in_re s re :> Expr.t) ];
    Solver.add solver [ (Bool.eq (String.length s) (Int.v 5) :> Expr.t) ];
    assert_sat ~f:"test_re_complex" (Solver.check solver []);
    let model = Solver.model solver in
    let val_s =
      Option.bind model (fun m -> Model.evaluate m (Symbol.make Ty_str "s"))
    in
    Alcotest.(check bool)
      "re_complex: has suffix abb"
      ( match val_s with
      | Some (Str s) ->
        Stdlib.String.length s = 5 && Stdlib.String.ends_with s ~suffix:"abb"
      | _ -> false )
      true

  let test_regexp_unsat solver_module =
    let open Typed in
    let module Solver = (val solver_module : Solver_intf.S) in
    let solver = Solver.create () in
    let s = symbol Types.string "s" in
    let re_a = String.(to_re (v "a")) in
    let re_b = String.(to_re (v "b")) in
    Solver.add solver [ (String.in_re s re_a :> Expr.t) ];
    Solver.add solver [ (String.in_re s re_b :> Expr.t) ];
    assert_unsat ~f:"test_re_unsat" (Solver.check solver [])

  let test_regexp_none solver_module =
    let open Typed in
    let module Solver = (val solver_module : Solver_intf.S) in
    let solver = Solver.create () in
    let s = symbol Types.string "s" in
    Solver.add solver [ (String.(in_re s Re.none) :> Expr.t) ];
    assert_unsat ~f:"test_re_none" (Solver.check solver [])

  let test_regexp =
    ( "test_regexp"
    , [ Alcotest.test_case "test_re_allchar" `Quick
          (with_solver test_regexp_allchar)
      ; Alcotest.test_case "test_re_diff" `Quick (with_solver test_regexp_diff)
      ; Alcotest.test_case "test_re_diff_unsat" `Quick
          (with_solver test_regexp_diff_unsat)
      ; Alcotest.test_case "test_re_concat" `Quick
          (with_solver test_regexp_concat)
      ; Alcotest.test_case "test_re_union" `Quick
          (with_solver test_regexp_union)
      ; Alcotest.test_case "test_re_star" `Quick (with_solver test_regexp_star)
      ; Alcotest.test_case "test_re_complex" `Quick
          (with_solver test_regexp_complex)
      ; Alcotest.test_case "test_re_unsat" `Quick
          (with_solver test_regexp_unsat)
      ; Alcotest.test_case "test_re_none" `Quick (with_solver test_regexp_none)
      ] )

  let test_uninterpreted =
    ( "test_uninterpreted_function"
    , [ Alcotest.test_case "test_int_bool_app" `Quick
          (with_solver (fun solver_module ->
             let module Solver = (val solver_module : Solver_intf.S) in
             let solver =
               Solver.create ~params:(Params.default ()) ~logic:Logic.QF_UFBV ()
             in
             let f = Symbol.(make Ty_int "f") in
             let app = Expr.app f [ Expr.value (Int 1); Expr.value True ] in
             Solver.add solver [ Expr.relop Ty_int Eq app (Expr.value (Int 2)) ];
             assert_sat ~f:"test_uninterpreted_function"
               (Solver.check solver []) ) )
      ] )

  let test_extract_bit_level solver_module =
    let open Infix in
    let module Solver = (val solver_module : Solver_intf.S) in
    let create_solver () =
      Solver.create ~params:(Params.default ()) ~logic:QF_BVFP ()
    in

    let solver1 = create_solver () in
    let x = int8 0xAF in
    let extracted = Expr.raw_extract x ~high:3 ~low:0 in
    Alcotest.(check (testable Ty.pp Ty.equal) "Result type should be 4 bits")
      (Ty.Ty_bitv 4) (Expr.ty extracted);
    Solver.add solver1
      [ Expr.raw_relop (Ty_bitv 4) Eq extracted
          (Expr.value (Bitv (Bitvector.make (Z.of_int 0xF) 4)))
      ];
    assert_sat ~f:"test_extract_low_bits" (Solver.check solver1 []);

    let solver2 = create_solver () in
    let extracted_high = Expr.raw_extract x ~high:7 ~low:4 in
    Alcotest.(check (testable Ty.pp Ty.equal) "Result type should be 4 bits")
      (Ty.Ty_bitv 4) (Expr.ty extracted_high);
    Solver.add solver2
      [ Expr.raw_relop (Ty_bitv 4) Eq extracted_high
          (Expr.value (Bitv (Bitvector.make (Z.of_int 0xA) 4)))
      ];
    assert_sat ~f:"test_extract_high_bits" (Solver.check solver2 []);

    let solver3 = create_solver () in
    let y = int8 0xAB in
    let extracted_mid = Expr.raw_extract y ~high:5 ~low:2 in
    Alcotest.(check (testable Ty.pp Ty.equal) "Result type should be 4 bits")
      (Ty.Ty_bitv 4) (Expr.ty extracted_mid);
    Solver.add solver3
      [ Expr.raw_relop (Ty_bitv 4) Eq extracted_mid
          (Expr.value (Bitv (Bitvector.make (Z.of_int 0xA) 4)))
      ];
    assert_sat ~f:"test_extract_non_aligned" (Solver.check solver3 []);

    let solver4 = create_solver () in
    let z = int32 0xFl in
    let single_bit = Expr.raw_extract z ~high:0 ~low:0 in
    Alcotest.(check (testable Ty.pp Ty.equal) "Result type should be 1 bit")
      (Ty.Ty_bitv 1) (Expr.ty single_bit);
    Solver.add solver4
      [ Expr.raw_relop (Ty_bitv 1) Eq single_bit
          (Expr.value (Bitv (Bitvector.make Z.one 1)))
      ];
    assert_sat ~f:"test_extract_single_bit" (Solver.check solver4 []);

    let solver5 = create_solver () in
    let w = int32 0xDEADBEEFl in
    let full_extract = Expr.raw_extract w ~high:31 ~low:0 in
    Alcotest.(check (testable Ty.pp Ty.equal) "Result type should be 32 bits")
      (Ty.Ty_bitv 32) (Expr.ty full_extract);
    Solver.add solver5
      [ Expr.raw_relop (Ty_bitv 32) Eq full_extract (int32 0xDEADBEEFl) ];
    assert_sat ~f:"test_extract_full_width" (Solver.check solver5 []);

    let solver6 = create_solver () in
    let sym_x = symbol "bv_x" (Ty_bitv 32) in
    Solver.add solver6 [ Expr.relop (Ty_bitv 32) Eq sym_x (int32 0x12345678l) ];
    let sym_extracted = Expr.extract sym_x ~high:15 ~low:8 in
    Solver.add solver6 [ Expr.relop (Ty_bitv 8) Eq sym_extracted (int8 0x56) ];
    assert_sat ~f:"test_extract_symbolic" (Solver.check solver6 [])

  let test_extract =
    ( "test_extract"
    , [ Alcotest.test_case "test_extract_bit_level" `Quick
          (with_solver test_extract_bit_level)
      ] )

  let test_bitv32_to_bytes solver_module =
    let open Typed in
    let module Solver = (val solver_module : Solver_intf.S) in
    let solver = Solver.create ~params:(Params.default ()) ~logic:QF_BVFP () in

    let bv_val = Bitvector.make (Z.of_int32 0xDEADBEEFl) 32 in
    let bv = Bitv32.v bv_val in

    match Bitv32.to_bytes bv with
    | [ b0; b1; b2; b3 ] ->
      let v8 i = Bitv8.v (Bitvector.make (Z.of_int i) 8) in
      Solver.add solver
        [ (Bool.eq b0 (v8 0xEF) :> Expr.t)
        ; (Bool.eq b1 (v8 0xBE) :> Expr.t)
        ; (Bool.eq b2 (v8 0xAD) :> Expr.t)
        ; (Bool.eq b3 (v8 0xDE) :> Expr.t)
        ];
      assert_sat ~f:"test_bitv32_to_bytes" (Solver.check solver [])
    | _ -> Alcotest.fail "Bitv32.to_bytes should return exactly 4 bytes"

  let test_bitv64_to_bytes solver_module =
    let open Typed in
    let module Solver = (val solver_module : Solver_intf.S) in
    let solver = Solver.create ~params:(Params.default ()) ~logic:QF_BVFP () in

    let bv_val = Bitvector.make (Z.of_int64 0x0123456789ABCDEFL) 64 in
    let bv = Bitv64.v bv_val in

    match Bitv64.to_bytes bv with
    | [ b0; b1; b2; b3; b4; b5; b6; b7 ] ->
      let v8 i = Bitv8.v (Bitvector.make (Z.of_int i) 8) in
      Solver.add solver
        [ (Bool.eq b0 (v8 0xEF) :> Expr.t)
        ; (Bool.eq b1 (v8 0xCD) :> Expr.t)
        ; (Bool.eq b2 (v8 0xAB) :> Expr.t)
        ; (Bool.eq b3 (v8 0x89) :> Expr.t)
        ; (Bool.eq b4 (v8 0x67) :> Expr.t)
        ; (Bool.eq b5 (v8 0x45) :> Expr.t)
        ; (Bool.eq b6 (v8 0x23) :> Expr.t)
        ; (Bool.eq b7 (v8 0x01) :> Expr.t)
        ];
      assert_sat ~f:"test_bitv64_to_bytes" (Solver.check solver [])
    | _ -> Alcotest.fail "Bitv64.to_bytes should return exactly 8 bytes"

  let test_typed_api_consistency =
    ( "test_typed_api_consistency"
    , [ Alcotest.test_case "test_bitv32_to_bytes" `Quick
          (with_solver test_bitv32_to_bytes)
      ; Alcotest.test_case "test_bitv64_to_bytes" `Quick
          (with_solver test_bitv64_to_bytes)
      ] )
end

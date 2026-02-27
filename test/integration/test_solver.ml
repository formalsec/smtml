open OUnit2
open Smtml

module Make (M : Mappings_intf.S_with_fresh) = struct
  open Smtml_test.Test_harness
  module Cached = Solver.Cached (M)

  let setup _test_ctxt =
    let module Mappings : Mappings_intf.S = M.Fresh.Make () in
    (module Smtml.Solver.Incremental (Mappings) : Solver_intf.S)

  let teardown _solver_module _test_ctxt = ()

  let with_solver f test_ctxt =
    let solver_module = bracket setup teardown test_ctxt in
    f solver_module

  let test_default_params _ =
    assert_equal (Params.default_value Timeout) Int32.(to_int max_int);
    assert_equal (Params.default_value Model) true;
    assert_equal (Params.default_value Unsat_core) false;
    assert_equal (Params.default_value Ematching) true;
    assert_equal (Params.default_value Random_seed) 0

  let test_solver_params solver_module =
    let module Solver = (val solver_module : Solver_intf.S) in
    let params =
      Params.(
        default () $ (Timeout, 900) $ (Model, false) $ (Unsat_core, true)
        $ (Ematching, false) $ (Parallel, true) $ (Num_threads, 1)
        $ (Debug, false) $ (Random_seed, 1227) )
    in
    assert (Params.get params Unsat_core);
    let _ : Solver.t = Solver.create ~params () in
    ()

  let test_params =
    "test_params"
    >::: [ "test_default_params" >:: with_solver test_default_params
         ; "test_solver_params" >:: with_solver test_solver_params
         ]

  let test_cache_hits _ =
    let solver = Cached.create ~logic:LIA () in
    let x = Infix.symbol "x" Ty_int in
    let c = Infix.(Int.(int 0 <= x)) in
    let get_stat key =
      let stats = Cached.get_statistics solver in
      let stat = Statistics.Map.find_opt key stats in
      match stat with
      (* we're using this for cache hitrate so it's always `Int *)
      | Some (`Int s) -> s
      | _ -> Fmt.failwith "%s should exist and be an int in stats" key
    in
    assert (get_stat "cache hits" = 0);
    assert_sat (Cached.check_set solver @@ Expr.Set.singleton c);
    assert_sat (Cached.check_set solver @@ Expr.Set.singleton c);
    assert_sat (Cached.check_set solver @@ Expr.Set.singleton c);
    assert (get_stat "cache misses" = 1);
    assert (get_stat "cache hits" = 2)

  let test_cache_get_model _ =
    let open Infix in
    let solver = Cached.create ~logic:LIA () in
    let x = symbol "x" Ty_int in
    let set = Expr.Set.of_list Int.[ int 0 <= x; x < int 10 ] in
    assert (
      match Cached.get_sat_model solver set with
      | `Model _ -> true
      | `Unsat | `Unknown -> false )

  let test_cached =
    "test_cached"
    >::: [ "test_cache_hits" >:: with_solver test_cache_hits
         ; "test_cache_get_model" >:: with_solver test_cache_get_model
         ]

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
    (* necessary, otherwise the solver doesn't know x and can't produce a model
       for it *)
    let model = Solver.model ~symbols:[ symbol_x ] solver in
    let val_x = Option.bind model (fun m -> Model.evaluate m symbol_x) in
    assert (Option.is_some val_x);
    Solver.pop solver 1;

    Solver.add solver [ x = int 5 ];
    assert_sat (Solver.check solver []);
    let model = Solver.model solver in
    let val_x = Option.bind model (fun m -> Model.evaluate m symbol_x) in
    assert (match val_x with Some v -> Value.equal v (Int 5) | None -> false)

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
    "test_lia"
    >::: [ "test_lia_0" >:: with_solver test_lia_0
         ; "test_lia_1" >:: with_solver test_lia_1
         ; "test_distinct" >:: with_solver test_distinct
         ]

  let test_lra =
    "test_lra"
    >:: with_solver @@ fun solver_module ->
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
           Solver.check solver [ c0; c1 ] )

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
    assert (match Solver.model solver with None -> false | Some _m -> true)

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

  let test_bv =
    "test_bv"
    >::: [ "test_bv_8" >:: with_solver test_bv_8
         ; "test_bv_32" >:: with_solver test_bv_32
         ; "test_arbitrary_bv" >:: with_solver test_arbitrary_bv
         ]

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
    "test_fp"
    >::: [ "test_fp_get_value32" >:: with_solver test_fp_get_value32
         ; "test_fp_get_value64" >:: with_solver test_fp_get_value64
         ; "test_fp_sqrt" >:: with_solver test_fp_sqrt
         ; "test_fp_copysign32" >:: with_solver test_fp_copysign32
         ; "test_fp_copysign64" >:: with_solver test_fp_copysign64
         ; "test_to_ieee_bv" >:: with_solver test_to_ieee_bv
         ]

  let test_regexp solver_module =
    let open Typed in
    let module Solver = (val solver_module : Solver_intf.S) in
    let solver = Solver.create () in
    let s = symbol Types.string "s" in
    let any_char = String.(Re.range (v "a") (v "z")) in
    Solver.add solver [ (String.in_re s any_char :> Expr.t) ];
    assert_sat ~f:"test_re_allchar" (Solver.check solver []);
    let model = Solver.model solver in
    let val_s =
      Option.bind model (fun m -> Model.evaluate m (Symbol.make Ty_str "s"))
    in
    assert (
      match val_s with Some (Str s) -> Stdlib.String.length s = 1 | _ -> false )

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
    assert (match val_s with Some (Str "ab") -> true | _ -> false)

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
    assert (
      match val_s with Some (Str "a") | Some (Str "b") -> true | _ -> false )

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
    assert (match val_s with Some (Str "aaa") -> true | _ -> false)

  let test_regexp_complex solver_module =
    let open Typed in
    let module Solver = (val solver_module : Solver_intf.S) in
    let solver = Solver.create () in
    let s = symbol Types.string "s" in
    let re_a = String.(to_re (v "a")) in
    let re_b = String.(to_re (v "b")) in
    (* (a|b)*abb *)
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
    assert (
      match val_s with
      | Some (Str s) ->
        Stdlib.String.length s = 5 && Stdlib.String.ends_with s ~suffix:"abb"
      | _ -> false )

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
    "test_regexp"
    >::: [ "test_re" >:: with_solver test_regexp
         ; "test_re_concat" >:: with_solver test_regexp_concat
         ; "test_re_union" >:: with_solver test_regexp_union
         ; "test_re_star" >:: with_solver test_regexp_star
         ; "test_re_complex" >:: with_solver test_regexp_complex
         ; "test_re_unsat" >:: with_solver test_regexp_unsat
         ; "test_re_none" >:: with_solver test_regexp_none
         ]

  let test_uninterpreted =
    "test_uninterpreted_function"
    >::: [ ( "test_int_bool_app"
           >:: with_solver @@ fun solver_module ->
               let module Solver = (val solver_module : Solver_intf.S) in
               let solver =
                 Solver.create ~params:(Params.default ()) ~logic:Logic.QF_UFBV
                   ()
               in
               let f = Symbol.(make Ty_int "f") in
               let app = Expr.app f [ Expr.value (Int 1); Expr.value True ] in
               Solver.add solver
                 [ Expr.relop Ty_int Eq app (Expr.value (Int 2)) ];
               assert_sat ~f:"test_uninterpreted_function"
                 (Solver.check solver []) )
         ]
end

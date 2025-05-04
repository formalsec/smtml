open OUnit2
open Smtml

module Make (M : Mappings_intf.S_with_fresh) = struct
  open Smtml_test.Test_harness
  module Cached = Solver.Cached (M)

  let setup _test_ctxt =
    let module Mappings : Mappings_intf.S = M.Fresh.Make () in
    (module Smtml.Solver.Incremental (Mappings) : Solver_intf.S)

  let teardown _solver_module _test_ctxt = ()

  let wrap f test_ctxt =
    let solver_module = bracket setup teardown test_ctxt in
    f solver_module

  let test_default_params _ =
    assert_equal (Params.default_value Timeout) Int32.(to_int max_int);
    assert_equal (Params.default_value Model) true;
    assert_equal (Params.default_value Unsat_core) false;

    assert_equal (Params.default_value Ematching) true

  let test_solver_params solver_module =
    let module Solver = (val solver_module : Solver_intf.S) in
    let params =
      Params.(
        default () $ (Timeout, 900) $ (Model, false) $ (Unsat_core, true)
        $ (Ematching, false) $ (Parallel, true) $ (Num_threads, 1)
        $ (Debug, false) )
    in
    assert (Params.get params Unsat_core);
    let _ : Solver.t = Solver.create ~params () in
    ()

  let test_params =
    "test_params"
    >::: [ "test_default_params" >:: wrap test_default_params
         ; "test_solver_params" >:: wrap test_solver_params
         ]

  let test_cache_hits _ =
    let solver = Cached.create ~logic:LIA () in
    let x = Infix.symbol "x" Ty_int in
    let c = Infix.(Int.(x >= int 0)) in
    assert (Cached.cache_hits () = 0);
    assert_sat (Cached.check_set solver @@ Expr.Set.singleton c);
    assert_sat (Cached.check_set solver @@ Expr.Set.singleton c);
    assert_sat (Cached.check_set solver @@ Expr.Set.singleton c);
    assert (Cached.cache_misses () = 1);
    assert (Cached.cache_hits () = 2)

  let test_cache_get_model _ =
    let open Infix in
    let solver = Cached.create ~logic:LIA () in
    let x = symbol "x" Ty_int in
    let set = Expr.Set.of_list Int.[ x >= int 0; x < int 10 ] in
    assert (
      match Cached.get_sat_model solver set with
      | `Model _ -> true
      | `Unsat | `Unknown -> false )

  let test_cached =
    "test_cached"
    >::: [ "test_cache_hits" >:: wrap test_cache_hits
         ; "test_cache_get_model" >:: wrap test_cache_get_model
         ]

  let test_lia_0 solver_module =
    let open Infix in
    let module Solver = (val solver_module : Solver_intf.S) in
    let solver = Solver.create ~logic:LIA () in
    let symbol_x = Symbol.("x" @: Ty_int) in
    let x = Expr.symbol symbol_x in
    assert_sat ~f:"test" (Solver.check solver []);

    Solver.push solver;
    Solver.add solver Int.[ x >= int 0 ];
    assert_sat (Solver.check solver []);
    check (Solver.get_value solver x) (int 0);
    Solver.pop solver 1;

    Solver.push solver;
    Solver.add solver [ x = int 3 ];
    assert_sat ~f:"test" (Solver.check solver []);
    check (Solver.get_value solver Int.(x * x)) (int 9);
    Solver.pop solver 1;

    Solver.push solver;
    Solver.add solver Int.[ x >= int 0 || x < int 0 ];
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

  let test_lia_1 solver_module =
    let open Infix in
    let module Solver = (val solver_module : Solver_intf.S) in
    let solver = Solver.create ~logic:QF_LIA () in
    let a = symbol "a" Ty_int in
    Solver.add solver Int.[ a + int 1 = int 2 => ((a * int 2) + int 2 = int 4) ];
    assert_sat ~f:"test_lia" (Solver.check solver [])

  let test_lia =
    "test_lia"
    >::: [ "test_lia_0" >:: wrap test_lia_0; "test_lia_1" >:: wrap test_lia_1 ]

  let test_lra =
    "test_lra"
    >:: wrap @@ fun solver_module ->
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
      [ Expr.relop ty Gt x (int8 0); Expr.relop ty Lt x (int8 2) ];
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
      [ Expr.relop ty Gt x (int32 0l) && Expr.relop ty Lt w (int32 5l)
      ; Expr.relop ty Lt x y && Expr.relop ty Lt y z && Expr.relop ty Lt z w
      ];
    assert_sat ~f:"test_bv_32" (Solver.check solver []);
    assert (match Solver.model solver with None -> false | Some _m -> true)

  let test_bv =
    "test_bv"
    >::: [ "test_bv_8" >:: wrap test_bv_8; "test_bv_32" >:: wrap test_bv_32 ]

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
      [ Expr.relop ty Gt x (float32 0.0) && Expr.relop ty Lt y (float32 0.0)
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
      [ Expr.relop ty Gt x (float64 0.0) && Expr.relop ty Lt y (float64 0.0)
      ; Expr.relop ty Lt (Expr.binop ty Copysign x y) (float64 0.0)
      ];
    assert_sat ~f:"test_copysign64" (Solver.check solver [])

  let test_fp =
    "test_fp"
    >::: [ "test_fp_get_value32" >:: wrap test_fp_get_value32
         ; "test_fp_get_value64" >:: wrap test_fp_get_value64
         ; "test_fp_sqrt" >:: wrap test_fp_sqrt
         ; "test_fp_copysign32" >:: wrap test_fp_copysign32
         ; "test_fp_copysign64" >:: wrap test_fp_copysign64
         ]
end

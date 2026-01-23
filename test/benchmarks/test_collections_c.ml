open OUnit2
module Solver = Smtml.Solver.Incremental (Smtml.Z3_mappings)
module Interpreter = Smtml.Interpret.Make (Solver)

let benchmarks =
  let dir = Fpath.(v "datasets" / "collections-c") in
  Bos.OS.Dir.fold_contents ~traverse:`Any ~elements:`Files
    (fun path acc -> if Fpath.has_ext ".smt2" path then path :: acc else acc)
    [] dir
  |> Result.map_error (fun (`Msg err) ->
    Fmt.epr "%s" err;
    `Msg err )
  |> Result.get_ok

let make_test path _ =
  let script = Smtml.Compile.until_rewrite path in
  let _ = Interpreter.start ~no_strict_status:true ~quiet:true script in
  ()

let tests =
  "collections-c"
  >::: List.map (fun path -> Fpath.to_string path >:: make_test path) benchmarks

let () = run_test_tt_main tests

open OUnit2
open Smtml
module Map = Statistics.Map

let test_merge _ =
  let s1 = Map.empty |> Map.add "time" (`Float 10.0) in
  let s2 = Map.empty |> Map.add "time" (`Float 20.0) in
  assert_equal
    (Statistics.merge s1 s2 |> Map.find_opt "time")
    (Some (`Float 30.0))

let () = run_test_tt_main ("test_merge" >:: test_merge)

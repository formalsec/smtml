open Smtml
module Map = Statistics.Map

let () =
  let s1 = Map.empty |> Map.add "time" (`Float 10.0) in
  let s2 = Map.empty |> Map.add "time" (`Float 20.0) in
  assert (Statistics.merge s1 s2 |> Map.find "time" = `Float 30.0)

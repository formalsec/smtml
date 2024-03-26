let run_and_time_call ~use f =
  let start = Sys.time () in
  let result = f () in
  use (Sys.time () -. start);
  result

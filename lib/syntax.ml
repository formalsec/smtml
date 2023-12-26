module Option = struct
  let ( let+ ) o f = Option.map f o
  let ( let* ) o f = Option.bind o f
end

module Result = struct
  let ( let+ ) o f = Result.map f o
  let ( let* ) o f = Result.bind o f

  let list_map f vs =
    let exception E of string in
    try
      Ok
        (List.map
           (fun v -> match f v with Error msg -> raise (E msg) | Ok v -> v)
           vs )
    with E msg -> Error msg
end

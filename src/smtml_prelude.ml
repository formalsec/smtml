include Prelude

module Option = struct
  include Option

  let ( let* ) v f = bind v f

  let ( let+ ) v f = map f v
end

module Result = struct
  include Result

  let ( let* ) v f = Result.bind v f

  let ( let+ ) v f = Result.map f v

  let rec list_iter f = function
    | [] -> Ok ()
    | hd :: tl ->
      let* () = f hd in
      list_iter f tl

  let list_map f v =
    let rec list_map_cps f v k =
      match v with
      | [] -> k (Ok [])
      | hd :: tl ->
        list_map_cps f tl (fun rest ->
            let* rest in
            let* hd' = f hd in
            k (Ok (hd' :: rest)) )
    in
    list_map_cps f v Fun.id

  let list_filter_map f v =
    let rec list_filter_map_cps f v k =
      match v with
      | [] -> k (Ok [])
      | hd :: tl ->
        list_filter_map_cps f tl (fun rest ->
            let* rest in
            let* v = f hd in
            k (Ok (match v with None -> rest | Some v -> v :: rest)) )
    in
    list_filter_map_cps f v Fun.id
end

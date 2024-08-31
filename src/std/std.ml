module Result = struct
  include Result

  let rec iter_list f = function
    | [] -> Ok ()
    | h :: tl -> (
      match f h with Error _ as e -> e | Ok () -> iter_list f tl )

  module Let_syntax = struct
    let ( let* ) v f = Result.bind v f

    let ( let+ ) v f = Result.map f v
  end
end

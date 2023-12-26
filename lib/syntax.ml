let ( let+ ) o f = Option.map f o
let ( let* ) o f = Result.bind o f

type t =
  { sort : Ty.t
  ; name : String.t
  }

let ( @: ) (name : string) (sort : Ty.t) = { name; sort }
let mk_symbol (sort : Ty.t) (name : string) = name @: sort

let equal (s1 : t) (s2 : t) : bool =
  s1.sort = s2.sort && String.equal s1.name s2.name

let compare t1 t2 =
  let compare_name = compare t1.name t2.name in
  if compare_name = 0 then
    compare t1.sort t2.sort
  else
    compare_name

let rename (s : t) (name : String.t) = { s with name }
let type_of (s : t) : Ty.t = s.sort
let pp fmt s = Format.pp_print_string fmt s.name
let to_string (s : t) : string = s.name

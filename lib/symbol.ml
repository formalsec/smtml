type t =
  { sort : Ty.t
  ; name : String.t
  }

let mk_symbol (sort : Ty.t) (name : string) = { sort; name }

let equal (s1 : t) (s2 : t) : bool =
  s1.sort = s2.sort && String.equal s1.name s2.name

let rename (s : t) (name : String.t) = { s with name }
let type_of (s : t) : Ty.t = s.sort
let pp fmt s = Format.pp_print_string fmt s.name
let to_string (s : t) : string = s.name

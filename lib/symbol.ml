type t =
  { ty : Ty.t
  ; name : string
  }

let ( @: ) (name : string) (ty : Ty.t) : t = { name; ty }

let make (ty : Ty.t) (name : string) : t = name @: ty

let mk_symbol (ty : Ty.t) (name : string) : t = name @: ty

let equal (s1 : t) (s2 : t) : bool =
  Ty.equal s1.ty s2.ty && String.equal s1.name s2.name

let compare (t1 : t) (t2 : t) : int =
  let compare_name = compare t1.name t2.name in
  if compare_name = 0 then compare t1.ty t2.ty else compare_name

let rename (symbol : t) (name : string) : t = { symbol with name }

let type_of ({ ty; _ } : t) : Ty.t = ty

let pp (fmt : Format.formatter) ({ name; _ } : t) : unit =
  Format.pp_print_string fmt name

let to_string ({ name; _ } : t) : string = name

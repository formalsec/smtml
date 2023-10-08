type binop =
  | Nth
  | Concat

type unop =
  | Len
  | Trim

type relop =
  | Eq
  | Ne

type triop = SubStr
type cvtop

let neg_relop (op : relop) : relop = match op with Eq -> Ne | Ne -> Eq

let string_of_binop (op : binop) : string =
  match op with Nth -> "nth" | Concat -> "++"

let string_of_unop (op : unop) : string =
  match op with Len -> "len" | Trim -> "trim"

let string_of_triop (op : triop) : string = match op with SubStr -> "sub"

let string_of_relop (op : relop) : string =
  match op with Eq -> "eq" | Ne -> "ne"

let string_of_cvtop (_ : cvtop) : string = assert false

open Base

type binop = Nth | Concat
type unop = Len
type relop = Eq | Ne
type triop = SubStr
type cvtop = ToFloat

let neg_relop (op : relop) : relop = match op with Eq -> Ne | Ne -> Eq

let string_of_binop (op : binop) : string =
  match op with Nth -> "nth" | Concat -> "concat"

let pp_string_of_binop (op : binop) : string = string_of_binop op
let string_of_unop (op : unop) : string = match op with Len -> "len"
let pp_string_of_unop (op : unop) : string = string_of_unop op
let string_of_triop (op : triop) : string = match op with SubStr -> "s_substr"
let pp_string_of_triop (op : triop) : string = string_of_triop op

let string_of_relop (op : relop) : string =
  match op with Eq -> "Eq" | Ne -> "Ne"

let pp_string_of_relop (op : relop) : string =
  match op with Eq -> "==" | Ne -> "!="

let string_of_cvtop (op : cvtop) : string = match op with ToFloat -> "ToFloat"
let pp_string_of_cvtop (op : cvtop) : string = string_of_cvtop op

type binop =
  | And
  | Or
  | Xor

type unop = Not

type relop =
  | Eq
  | Ne

type triop = ITE
type cvtop

let neg_relop (op : relop) : relop = match op with Eq -> Ne | Ne -> Eq
let string_of_unop (op : unop) : string = match op with Not -> "not"

let string_of_binop (op : binop) : string =
  match op with And -> "and" | Or -> "or" | Xor -> "xor"

let string_of_relop (op : relop) : string =
  match op with Eq -> "eq" | Ne -> "ne"

let string_of_cvtop (_ : cvtop) : string = assert false
let string_of_triop (op : triop) : string = match op with ITE -> "ite"

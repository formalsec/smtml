open Base

type binop = And | Or | Xor
type unop = Not
type relop = Eq | Ne
type triop
type cvtop

let neg_relop (op : relop) : relop = match op with Eq -> Ne | Ne -> Eq

let string_of_binop (op : binop) : string =
  match op with And -> "And" | Or -> "Or" | Xor -> "Xor"

let pp_string_of_binop (op : binop) : string =
  match op with And -> "&&" | Or -> "||" | Xor -> "^"

let string_of_unop (op : unop) : string = match op with Not -> "Not"
let pp_string_of_unop (op : unop) : string = match op with Not -> "!"

let string_of_relop (op : relop) : string =
  match op with Eq -> "Eq" | Ne -> "Ne"

let pp_string_of_relop (op : relop) : string =
  match op with Eq -> "==" | Ne -> "!="

let string_of_cvtop (_ : cvtop) : string = assert false
let pp_string_of_cvtop (_ : cvtop) : string = assert false
let string_of_triop (_ : triop) : string = assert false
let pp_string_of_triop (_ : triop) : string = assert false

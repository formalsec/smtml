open Base

type binop = Add | Mul | Div | Rem | And | Sub | Shl | ShrA | ShrL | Or | Xor
type unop = Neg
type relop = Eq | Lt | Le | Ne | Gt | Ge
type triop
type cvtop = ToString | OfString

let neg_relop (op : relop) : relop =
  match op with
  | Eq -> Ne
  | Ne -> Eq
  | Lt -> Ge
  | Gt -> Le
  | Le -> Gt
  | Ge -> Lt

(*  String representation of an i32 binary operation  *)
let string_of_binop (op : binop) : string =
  match op with
  | Add -> "Add"
  | And -> "And"
  | Or -> "Or"
  | Sub -> "Sub"
  | Div -> "Div"
  | Xor -> "Xor"
  | Mul -> "Mul"
  | Shl -> "Shl"
  | ShrA -> "ShrA"
  | ShrL -> "ShrU"
  | Rem -> "Rem"

let pp_string_of_binop (op : binop) : string =
  match op with
  | Add -> "+"
  | And -> "&"
  | Or -> "|"
  | Sub -> "-"
  | Div -> "/"
  | Xor -> "^"
  | Mul -> "*"
  | Shl -> "<<"
  | ShrA -> ">>a"
  | ShrL -> ">>l"
  | Rem -> "%"

(*  String representation of an i32 unary operation  *)
let string_of_unop (op : unop) : string = match op with Neg -> "Neg"
let pp_string_of_unop (op : unop) : string = match op with Neg -> "-"

(*  String representation of an i32 relative operation  *)
let string_of_relop (op : relop) : string =
  match op with
  | Eq -> "Eq"
  | Ne -> "Ne"
  | Lt -> "Lt"
  | Gt -> "Gt"
  | Le -> "Le"
  | Ge -> "Ge"

let pp_string_of_relop (op : relop) : string =
  match op with
  | Eq -> "=="
  | Ne -> "!="
  | Lt -> "<"
  | Gt -> ">"
  | Le -> "<="
  | Ge -> ">="

let string_of_cvtop (op : cvtop) : string =
  match op with ToString -> "ToString" | OfString -> "OfString"

let pp_string_of_cvtop (op : cvtop) : string = string_of_cvtop op
let string_of_triop (_ : triop) : string = assert false
let pp_string_of_triop (_ : triop) : string = assert false

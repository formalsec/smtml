open Base

type binop = Add | Sub | Mul | Div | Min | Max (*  Falta: | CopySign *)
type unop = Neg | Abs | Sqrt | Nearest (*  Falta: | Ceil | Floor | Trunc *)
type relop = Eq | Ne | Lt | Le | Gt | Ge
type triop

type cvtop =
  | DemoteF64
  | ConvertSI32
  | ConvertUI32
  | ConvertSI64
  | ConvertUI64
  | ReinterpretInt
  | PromoteF32

let neg_relop (op : relop) : relop =
  match op with
  | Eq -> Ne
  | Ne -> Eq
  | Lt -> Ge
  | Gt -> Le
  | Le -> Gt
  | Ge -> Lt

(*  String representation of an f32 binary operation  *)
let string_of_binop (op : binop) : string =
  match op with
  | Add -> "Add"
  | Sub -> "Sub"
  | Mul -> "Mul"
  | Div -> "Div"
  | Min -> "Min"
  | Max -> "Max"

let pp_string_of_binop (op : binop) : string =
  match op with
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Min -> "Min"
  | Max -> "Max"

(*  String representation of an f32 unary operation  *)
let string_of_unop (op : unop) : string =
  match op with
  | Neg -> "Neg"
  | Abs -> "Abs"
  | Sqrt -> "Sqrt"
  | Nearest -> "Nearest"

let pp_string_of_unop (op : unop) : string =
  match op with
  | Neg -> "-"
  | Abs -> "Abs"
  | Sqrt -> "Sqrt"
  | Nearest -> "Nearest"

(*  String representation of an f32 relative operation  *)
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
  match op with
  | DemoteF64 -> "DemoteF64"
  | ConvertSI32 -> "ConvertSI32"
  | ConvertUI32 -> "ConvertUI32"
  | ConvertSI64 -> "ConvertSI64"
  | ConvertUI64 -> "ConvertUI64"
  | ReinterpretInt -> "ReinterpretInt"
  | PromoteF32 -> "PromoteF32"

let pp_string_of_cvtop (op : cvtop) : string = string_of_cvtop op
let string_of_triop (_ : triop) : string = assert false
let pp_string_of_triop (_ : triop) : string = assert false

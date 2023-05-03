type binop =
  | Add
  | Mul
  | DivU
  | RemU
  | ShrU
  | And
  | Sub
  | Shl
  | DivS
  | RemS
  | ShrS
  | Or
  | Xor

type unop = Not | Clz (*  Falta:  Ctz | Popcnt *)
type relop = Eq | LtU | LtS | LeU | LeS | Ne | GtU | GtS | GeU | GeS
type triop

type cvtop =
  | TruncSF32
  | TruncUF32
  | TruncSF64
  | TruncUF64
  | ReinterpretFloat
  | WrapI64
  | ExtendSI32
  | ExtendUI32

let neg_relop (op : relop) : relop =
  match op with
  | Eq -> Ne
  | Ne -> Eq
  | LtU -> GeU
  | LtS -> GeS
  | GtU -> LeU
  | GtS -> LeS
  | LeU -> GtU
  | LeS -> GtS
  | GeU -> LtU
  | GeS -> LtS

(*  String representation of an i32 binary operation  *)
let string_of_binop (op : binop) : string =
  match op with
  | Add -> "Add"
  | And -> "And"
  | Or -> "Or"
  | Sub -> "Sub"
  | DivS -> "DivS"
  | DivU -> "DivU"
  | Xor -> "Xor"
  | Mul -> "Mul"
  | Shl -> "Shl"
  | ShrS -> "ShrS"
  | ShrU -> "ShrU"
  | RemS -> "RemS"
  | RemU -> "RemU"

let pp_string_of_binop (op : binop) : string =
  match op with
  | Add -> "+"
  | And -> "&"
  | Or -> "|"
  | Sub -> "-"
  | DivS -> "/"
  | DivU -> "/u"
  | Xor -> "^"
  | Mul -> "*"
  | Shl -> "<<"
  | ShrS -> ">>"
  | ShrU -> ">>u"
  | RemS -> "%"
  | RemU -> "%u"

(*  String representation of an i32 unary operation  *)
let string_of_unop (op : unop) : string =
  match op with Clz -> "Clz" | Not -> "Not"

let pp_string_of_unop (op : unop) : string =
  match op with Clz -> "Clz" | Not -> "Not"

(*  String representation of an i32 relative operation  *)
let string_of_relop (op : relop) : string =
  match op with
  | Eq -> "Eq"
  | Ne -> "Ne"
  | LtU -> "LtU"
  | LtS -> "LtS"
  | GtU -> "GtU"
  | GtS -> "GtS"
  | LeU -> "LeU"
  | LeS -> "LeS"
  | GeU -> "GeU"
  | GeS -> "GeS"

let pp_string_of_relop (op : relop) : string =
  match op with
  | Eq -> "=="
  | Ne -> "!="
  | LtU -> "<"
  | LtS -> "<"
  | GtU -> ">"
  | GtS -> ">"
  | LeU -> "<="
  | LeS -> "<="
  | GeU -> ">="
  | GeS -> ">="

let string_of_cvtop (op : cvtop) : string =
  match op with
  | WrapI64 -> "WrapI64"
  | TruncSF32 -> "TruncSF32"
  | TruncUF32 -> "TruncUF32"
  | TruncSF64 -> "TruncSF64"
  | TruncUF64 -> "TruncUF64"
  | ReinterpretFloat -> "ReinterpretFloat"
  | ExtendSI32 -> "ExtendSI32"
  | ExtendUI32 -> "ExtendUI32"

let pp_string_of_cvtop (op : cvtop) : string = string_of_cvtop op
let string_of_triop (_ : triop) : string = assert false
let pp_string_of_triop (_ : triop) : string = assert false

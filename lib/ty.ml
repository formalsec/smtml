type _ cast =
  | C32 : int32 cast
  | C64 : int64 cast

type sz =
  | S32
  | S64

type t =
  | Ty_int
  | Ty_real
  | Ty_bool
  | Ty_str
  | Ty_bitv of sz
  | Ty_fp of sz

type unop =
  | Neg
  | Not
  | Clz
  | Abs
  | Sqrt
  | Nearest
  | Is_nan
  | Ceil
  | Floor
  (* To remove *)
  | Len
  | Trim

type binop =
  | Add
  | Sub
  | Mul
  | Div
  | DivU
  | Rem
  | RemU
  | Shl
  | ShrA
  | ShrL
  | And
  | Or
  | Xor
  | Pow
  | Min
  | Max
  | Rotl
  | Rotr
  | Ext
  | ExtU
  (* To remove *)
  | Nth
  | Concat

type relop =
  | Eq
  | Ne
  | Lt
  | LtU
  | Gt
  | GtU
  | Le
  | LeU
  | Ge
  | GeU

type triop =
  | Ite
  | Substr

type cvtop =
  | ToString
  | OfString
  | ToBool
  | OfBool
  | Reinterpret_int
  | Reinterpret_float
  | DemoteF64
  | PromoteF32
  | ConvertSI32
  | ConvertUI32
  | ConvertSI64
  | ConvertUI64
  | TruncSF32
  | TruncUF32
  | TruncSF64
  | TruncUF64
  | WrapI64
  | ExtendSI32
  | ExtendUI32

let pp_unop fmt (op : unop) =
  match op with
  | Neg -> Format.pp_print_string fmt "neg"
  | Not -> Format.pp_print_string fmt "not"
  | Clz -> Format.pp_print_string fmt "clz"
  | Abs -> Format.pp_print_string fmt "abs"
  | Sqrt -> Format.pp_print_string fmt "sqrt"
  | Nearest -> Format.pp_print_string fmt "nearest"
  | Is_nan -> Format.pp_print_string fmt "is_nan"
  | Ceil -> Format.pp_print_string fmt "ceil"
  | Floor -> Format.pp_print_string fmt "floor"
  | Len -> Format.pp_print_string fmt "len"
  | Trim -> Format.pp_print_string fmt "trim"

let pp_binop fmt (op : binop) =
  match op with
  | Add -> Format.pp_print_string fmt "add"
  | Sub -> Format.pp_print_string fmt "sub"
  | Mul -> Format.pp_print_string fmt "mul"
  | Div -> Format.pp_print_string fmt "div"
  | DivU -> Format.pp_print_string fmt "div_u"
  | Rem -> Format.pp_print_string fmt "rem"
  | RemU -> Format.pp_print_string fmt "rem_u"
  | Shl -> Format.pp_print_string fmt "shl"
  | ShrA -> Format.pp_print_string fmt "shr"
  | ShrL -> Format.pp_print_string fmt "shr_u"
  | And -> Format.pp_print_string fmt "and"
  | Or -> Format.pp_print_string fmt "or"
  | Xor -> Format.pp_print_string fmt "xor"
  | Pow -> Format.pp_print_string fmt "pow"
  | Min -> Format.pp_print_string fmt "min"
  | Max -> Format.pp_print_string fmt "max"
  | Rotl -> Format.pp_print_string fmt "rotl"
  | Rotr -> Format.pp_print_string fmt "rotr"
  | Ext -> Format.pp_print_string fmt "ext"
  | ExtU -> Format.pp_print_string fmt "ext_u"
  | Nth -> Format.pp_print_string fmt "nth"
  | Concat -> Format.pp_print_string fmt "++"

let pp_triop fmt (op : triop) =
  match op with
  | Ite -> Format.pp_print_string fmt "ite"
  | Substr -> Format.pp_print_string fmt "sub"

let pp_relop fmt (op : relop) =
  match op with
  | Eq -> Format.pp_print_string fmt "eq"
  | Ne -> Format.pp_print_string fmt "ne"
  | Lt -> Format.pp_print_string fmt "lt"
  | LtU -> Format.pp_print_string fmt "lt_u"
  | Gt -> Format.pp_print_string fmt "gt"
  | GtU -> Format.pp_print_string fmt "gt_u"
  | Le -> Format.pp_print_string fmt "le"
  | LeU -> Format.pp_print_string fmt "le_u"
  | Ge -> Format.pp_print_string fmt "ge"
  | GeU -> Format.pp_print_string fmt "ge_u"

let pp_cvtop fmt (op : cvtop) =
  match op with
  | ToString -> Format.pp_print_string fmt "to_string"
  | OfString -> Format.pp_print_string fmt "of_string"
  | ToBool -> Format.pp_print_string fmt "to_bool"
  | OfBool -> Format.pp_print_string fmt "of_bool"
  | Reinterpret_int -> Format.pp_print_string fmt "reinterpret_int"
  | Reinterpret_float -> Format.pp_print_string fmt "reinterpret_float"
  | DemoteF64 -> Format.pp_print_string fmt "demote_f64"
  | PromoteF32 -> Format.pp_print_string fmt "promote_f32"
  | ConvertSI32 -> Format.pp_print_string fmt "convert_i32_s"
  | ConvertUI32 -> Format.pp_print_string fmt "convert_i32_u"
  | ConvertSI64 -> Format.pp_print_string fmt "convert_i64_s"
  | ConvertUI64 -> Format.pp_print_string fmt "convert_i64_u"
  | TruncSF32 -> Format.pp_print_string fmt "trunc_f32_s"
  | TruncUF32 -> Format.pp_print_string fmt "trunc_f32_u"
  | TruncSF64 -> Format.pp_print_string fmt "trunc_f64_s"
  | TruncUF64 -> Format.pp_print_string fmt "trunc_f64_u"
  | WrapI64 -> Format.pp_print_string fmt "wrap_i64"
  | ExtendSI32 -> Format.pp_print_string fmt "extend_i32_s"
  | ExtendUI32 -> Format.pp_print_string fmt "extend_i32_u"

let pp fmt = function
  | Ty_int -> Format.pp_print_string fmt "int"
  | Ty_real -> Format.pp_print_string fmt "real"
  | Ty_bool -> Format.pp_print_string fmt "bool"
  | Ty_str -> Format.pp_print_string fmt "str"
  | Ty_bitv S32 -> Format.pp_print_string fmt "i32"
  | Ty_bitv S64 -> Format.pp_print_string fmt "i64"
  | Ty_fp S32 -> Format.pp_print_string fmt "f32"
  | Ty_fp S64 -> Format.pp_print_string fmt "f64"

let string_of_type (ty : t) : string = Format.asprintf "%a" pp ty

let size (ty : t) : int =
  match ty with
  | Ty_bitv S32 | Ty_fp S32 -> 4
  | Ty_bitv S64 | Ty_fp S64 -> 8
  | _ -> assert false

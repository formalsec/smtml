let pp_string = Format.pp_print_string
let fprintf = Format.fprintf

type _ cast =
  | C8 : int cast
  | C32 : int32 cast
  | C64 : int64 cast

type bitwidth =
  | S8
  | S32
  | S64

type t =
  | Ty_int
  | Ty_real
  | Ty_bool
  | Ty_str
  | Ty_bitv of bitwidth
  | Ty_fp of bitwidth
  | Ty_var of { id : int }

type unop =
  | Abs
  | Ceil
  | Clz
  | Ctz
  | Floor
  | Is_nan
  | Len
  | Neg
  | Nearest
  | Not
  | Sqrt
  | Trim
  | Trunc

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
  | ExtS of int
  | ExtU of int
  | String_to_code
  | String_from_code

type logic =
  | AUFLIA
  | AUFLIRA
  | AUFNIRA
  | LIA
  | LRA
  | QF_ABV
  | QF_AUFBV
  | QF_AUFLIA
  | QF_AX
  | QF_BV
  | QF_BVFP
  | QF_IDL
  | QF_LIA
  | QF_LRA
  | QF_NIA
  | QF_NRA
  | QF_RDL
  | QF_UF
  | QF_UFBV
  | QF_UFIDL
  | QF_UFLIA
  | QF_UFLRA
  | QF_UFNRA
  | UFLRA
  | UFNIA

let pp_unop fmt (op : unop) =
  match op with
  | Neg -> pp_string fmt "neg"
  | Not -> pp_string fmt "not"
  | Clz -> pp_string fmt "clz"
  | Ctz -> pp_string fmt "ctz"
  | Abs -> pp_string fmt "abs"
  | Sqrt -> pp_string fmt "sqrt"
  | Is_nan -> pp_string fmt "is_nan"
  | Ceil -> pp_string fmt "ceil"
  | Floor -> pp_string fmt "floor"
  | Trunc -> pp_string fmt "trunc"
  | Nearest -> pp_string fmt "nearest"
  | Len -> pp_string fmt "len"
  | Trim -> pp_string fmt "trim"

let pp_binop fmt (op : binop) =
  match op with
  | Add -> pp_string fmt "add"
  | Sub -> pp_string fmt "sub"
  | Mul -> pp_string fmt "mul"
  | Div -> pp_string fmt "div"
  | DivU -> pp_string fmt "div_u"
  | Rem -> pp_string fmt "rem"
  | RemU -> pp_string fmt "rem_u"
  | Shl -> pp_string fmt "shl"
  | ShrA -> pp_string fmt "shr"
  | ShrL -> pp_string fmt "shr_u"
  | And -> pp_string fmt "and"
  | Or -> pp_string fmt "or"
  | Xor -> pp_string fmt "xor"
  | Pow -> pp_string fmt "pow"
  | Min -> pp_string fmt "min"
  | Max -> pp_string fmt "max"
  | Rotl -> pp_string fmt "rotl"
  | Rotr -> pp_string fmt "rotr"
  | Nth -> pp_string fmt "nth"
  | Concat -> pp_string fmt "++"

let pp_triop fmt (op : triop) =
  match op with Ite -> pp_string fmt "ite" | Substr -> pp_string fmt "sub"

let pp_relop fmt (op : relop) =
  match op with
  | Eq -> pp_string fmt "eq"
  | Ne -> pp_string fmt "ne"
  | Lt -> pp_string fmt "lt"
  | LtU -> pp_string fmt "lt_u"
  | Gt -> pp_string fmt "gt"
  | GtU -> pp_string fmt "gt_u"
  | Le -> pp_string fmt "le"
  | LeU -> pp_string fmt "le_u"
  | Ge -> pp_string fmt "ge"
  | GeU -> pp_string fmt "ge_u"

let pp_cvtop fmt (op : cvtop) =
  match op with
  | ToString -> pp_string fmt "to_string"
  | OfString -> pp_string fmt "of_string"
  | ToBool -> pp_string fmt "to_bool"
  | OfBool -> pp_string fmt "of_bool"
  | Reinterpret_int -> pp_string fmt "reinterpret_int"
  | Reinterpret_float -> pp_string fmt "reinterpret_float"
  | DemoteF64 -> pp_string fmt "demote_f64"
  | PromoteF32 -> pp_string fmt "promote_f32"
  | ConvertSI32 -> pp_string fmt "convert_i32_s"
  | ConvertUI32 -> pp_string fmt "convert_i32_u"
  | ConvertSI64 -> pp_string fmt "convert_i64_s"
  | ConvertUI64 -> pp_string fmt "convert_i64_u"
  | TruncSF32 -> pp_string fmt "trunc_f32_s"
  | TruncUF32 -> pp_string fmt "trunc_f32_u"
  | TruncSF64 -> pp_string fmt "trunc_f64_s"
  | TruncUF64 -> pp_string fmt "trunc_f64_u"
  | WrapI64 -> pp_string fmt "wrap_i64"
  | ExtS sz -> fprintf fmt "extend_i%d_s" sz
  | ExtU sz -> fprintf fmt "extend_i%d_u" sz
  | String_to_code -> pp_string fmt "string_to_code"
  | String_from_code -> pp_string fmt "string_from_code"

let pp fmt = function
  | Ty_int -> pp_string fmt "int"
  | Ty_real -> pp_string fmt "real"
  | Ty_bool -> pp_string fmt "bool"
  | Ty_str -> pp_string fmt "str"
  | Ty_bitv S8 -> pp_string fmt "i8"
  | Ty_bitv S32 -> pp_string fmt "i32"
  | Ty_bitv S64 -> pp_string fmt "i64"
  | Ty_fp S8 -> pp_string fmt "f8"
  | Ty_fp S32 -> pp_string fmt "f32"
  | Ty_fp S64 -> pp_string fmt "f64"
  | Ty_var { id } -> fprintf fmt "'a%d" id

let pp_logic fmt : logic -> unit = function
  | AUFLIA -> pp_string fmt "AUFLIA"
  | AUFLIRA -> pp_string fmt "AUFLIRA"
  | AUFNIRA -> pp_string fmt "AUFNIRA"
  | LIA -> pp_string fmt "LIA"
  | LRA -> pp_string fmt "LRA"
  | QF_ABV -> pp_string fmt "QF_ABV"
  | QF_AUFBV -> pp_string fmt "QF_AUFBV"
  | QF_AUFLIA -> pp_string fmt "QF_AUFLIA"
  | QF_AX -> pp_string fmt "QF_AX"
  | QF_BV -> pp_string fmt "QF_BV"
  | QF_BVFP -> pp_string fmt "QF_BVFP"
  | QF_IDL -> pp_string fmt "QF_IDL"
  | QF_LIA -> pp_string fmt "QF_LIA"
  | QF_LRA -> pp_string fmt "QF_LRA"
  | QF_NIA -> pp_string fmt "QF_NIA"
  | QF_NRA -> pp_string fmt "QF_NRA"
  | QF_RDL -> pp_string fmt "QF_RDL"
  | QF_UF -> pp_string fmt "QF_UF"
  | QF_UFBV -> pp_string fmt "QF_UFBV"
  | QF_UFIDL -> pp_string fmt "QF_UFIDL"
  | QF_UFLIA -> pp_string fmt "QF_UFLIA"
  | QF_UFLRA -> pp_string fmt "QF_UFLRA"
  | QF_UFNRA -> pp_string fmt "QF_UFNRA"
  | UFLRA -> pp_string fmt "UFLRA"
  | UFNIA -> pp_string fmt "UFNIA"

let string_of_type (ty : t) : string = Format.asprintf "%a" pp ty

let size (ty : t) : int =
  match ty with
  | Ty_bitv S32 | Ty_fp S32 -> 4
  | Ty_bitv S64 | Ty_fp S64 -> 8
  | Ty_bitv S8 | Ty_fp S8 -> 1
  | Ty_int | Ty_bool -> 4
  | Ty_real | Ty_str | Ty_var _ -> assert false

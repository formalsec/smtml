{
open Ty
open Lexing
open Parser

exception SyntaxError of string

let keywords =
  let tbl = Hashtbl.create 256 in
  Array.iter
    (fun (k, v) -> Hashtbl.add tbl k v)
    [| ("int" , TYPE (Ty_int))
     ; ("real", TYPE (Ty_real))
     ; ("bool", TYPE (Ty_bool))
     ; ("str" , TYPE (Ty_str))
     ; ("i32" , TYPE (Ty_bitv 32))
     ; ("i64" , TYPE (Ty_bitv 64))
     ; ("f32" , TYPE (Ty_fp 32))
     ; ("f64" , TYPE (Ty_fp 64))
     ; ("int.neg", UNARY (Ty_int, Neg))
     ; ("int.add", BINARY (Ty_int, Add))
     ; ("int.sub", BINARY (Ty_int, Sub))
     ; ("int.div", BINARY (Ty_int, Div))
     ; ("int.mul", BINARY (Ty_int, Mul))
     ; ("int.rem", BINARY (Ty_int, Rem))
     ; ("int.pow", BINARY (Ty_int, Pow))
     ; ("int.eq", RELOP (Ty_int, Eq))
     ; ("int.ne", RELOP (Ty_int, Ne))
     ; ("int.lt", RELOP (Ty_int, Lt))
     ; ("int.le", RELOP (Ty_int, Le))
     ; ("int.gt", RELOP (Ty_int, Gt))
     ; ("int.ge", RELOP (Ty_int, Ge))
     ; ("int.to_string", CVTOP (Ty_int, ToString))
     ; ("int.of_string", CVTOP (Ty_int, OfString))
     ; ("int.reinterpret_real", CVTOP (Ty_int, Reinterpret_float))
     ; ("real.neg", UNARY (Ty_real, Neg))
     ; ("real.abs", UNARY (Ty_real, Abs))
     ; ("real.sqrt", UNARY (Ty_real, Sqrt))
     ; ("real.nearest", UNARY (Ty_real, Nearest))
     ; ("real.is_nan", UNARY (Ty_real, Is_nan))
     ; ("real.add", BINARY (Ty_real, Add))
     ; ("real.sub", BINARY (Ty_real, Sub))
     ; ("real.div", BINARY (Ty_real, Div))
     ; ("real.mul", BINARY (Ty_real, Mul))
     ; ("real.rem", BINARY (Ty_real, Rem))
     ; ("real.min", BINARY (Ty_real, Min))
     ; ("real.max", BINARY (Ty_real, Max))
     ; ("real.eq", RELOP (Ty_real, Eq))
     ; ("real.ne", RELOP (Ty_real, Ne))
     ; ("real.lt", RELOP (Ty_real, Lt))
     ; ("real.le", RELOP (Ty_real, Le))
     ; ("real.gt", RELOP (Ty_real, Gt))
     ; ("real.ge", RELOP (Ty_real, Ge))
     ; ("real.reinterpret_int", CVTOP (Ty_real, Reinterpret_int))
     ; ("real.to_string", CVTOP (Ty_real, ToString))
     ; ("real.of_string", CVTOP (Ty_real, OfString))
     ; ("bool.not", UNARY (Ty_bool, Not))
     ; ("bool.and", BINARY (Ty_bool, And))
     ; ("bool.or", BINARY (Ty_bool, Or))
     ; ("bool.xor", BINARY (Ty_bool, Xor))
     ; ("=", RELOP (Ty_bool, Eq))
     ; ("bool.eq", RELOP (Ty_bool, Ne))
     ; ("bool.ne", RELOP (Ty_bool, Ne))
     ; ("bool.ite", TERNARY (Ty_bool, Ite))
     ; ("str.len", UNARY (Ty_str, Len))
     ; ("str.nth", BINARY (Ty_str, Nth))
     ; ("str.++", BINARY (Ty_str, Concat))
     ; ("str.sub", TERNARY (Ty_str, Substr))
     ; ("str.eq", RELOP (Ty_str, Eq))
     ; ("str.ne", RELOP (Ty_str, Ne))
     ; ("str.to_code", CVTOP (Ty_str, String_to_code))
     ; ("str.from_code", CVTOP (Ty_str, String_from_code))
     ; ("i32.neg", UNARY (Ty_bitv 32, Neg))
     ; ("i32.clz", UNARY (Ty_bitv 32, Clz))
     ; ("i32.not", UNARY (Ty_bitv 32, Not))
     ; ("i32.add", BINARY (Ty_bitv 32, Add))
     ; ("i32.sub", BINARY (Ty_bitv 32, Sub))
     ; ("i32.div", BINARY (Ty_bitv 32, Div))
     ; ("i32.div_u", BINARY (Ty_bitv 32, DivU))
     ; ("i32.and", BINARY (Ty_bitv 32, And))
     ; ("i32.or", BINARY (Ty_bitv 32, Or))
     ; ("i32.xor", BINARY (Ty_bitv 32, Xor))
     ; ("i32.mul", BINARY (Ty_bitv 32, Mul))
     ; ("i32.shl", BINARY (Ty_bitv 32, Shl))
     ; ("i32.shr", BINARY (Ty_bitv 32, ShrA))
     ; ("i32.shr_u", BINARY (Ty_bitv 32, ShrL))
     ; ("i32.rem", BINARY (Ty_bitv 32, Rem))
     ; ("i32.rem_u", BINARY (Ty_bitv 32, RemU))
     ; ("i32.eq", RELOP (Ty_bitv 32, Eq))
     ; ("i32.ne", RELOP (Ty_bitv 32, Ne))
     ; ("i32.lt_u", RELOP (Ty_bitv 32, LtU))
     ; ("i32.lt", RELOP (Ty_bitv 32, Lt))
     ; ("i32.le_u", RELOP (Ty_bitv 32, LeU))
     ; ("i32.le", RELOP (Ty_bitv 32, Le))
     ; ("i32.gt_u", RELOP (Ty_bitv 32, GtU))
     ; ("i32.gt", RELOP (Ty_bitv 32, Gt))
     ; ("i32.ge_u", RELOP (Ty_bitv 32, GeU))
     ; ("i32.ge", RELOP (Ty_bitv 32, Ge))
     ; ("i32.to_bool", CVTOP (Ty_bitv 32, ToBool))
     ; ("i32.of_bool", CVTOP (Ty_bitv 32, OfBool))
     ; ("i32.trunc_f32_s",  CVTOP (Ty_bitv 32, TruncSF32))
     ; ("i32.trunc_f32_u", CVTOP (Ty_bitv 32, TruncUF32))
     ; ("i32.trunc_f64_s", CVTOP (Ty_bitv 32, TruncSF64))
     ; ("i32.trunc_f64_u", CVTOP (Ty_bitv 32, TruncUF64))
     ; ("i32.reinterpret_float", CVTOP (Ty_bitv 32, Reinterpret_float))
     ; ("i32.wrap_i64", CVTOP (Ty_bitv 32, WrapI64))
     ; ("i32.extend_i16_s", CVTOP (Ty_bitv 32, ExtS 16))
     ; ("i32.extend_i16_u", CVTOP (Ty_bitv 32, ExtU 16))
     ; ("i32.extend_i24_s", CVTOP (Ty_bitv 32, ExtS 24))
     ; ("i32.extend_i24_u", CVTOP (Ty_bitv 32, ExtU 24))
     ; ("i64.neg", UNARY (Ty_bitv 64, Neg))
     ; ("i64.clz", UNARY (Ty_bitv 64, Clz))
     ; ("i64.not", UNARY (Ty_bitv 64, Not))
     ; ("i64.add", BINARY (Ty_bitv 64, Add))
     ; ("i64.sub", BINARY (Ty_bitv 64, Sub))
     ; ("i64.div", BINARY (Ty_bitv 64, Div))
     ; ("i64.div_u", BINARY (Ty_bitv 64, DivU))
     ; ("i64.and", BINARY (Ty_bitv 64, And))
     ; ("i64.or", BINARY (Ty_bitv 64, Or))
     ; ("i64.xor", BINARY (Ty_bitv 64, Xor))
     ; ("i64.mul", BINARY (Ty_bitv 64, Mul))
     ; ("i64.shl", BINARY (Ty_bitv 64, Shl))
     ; ("i64.shr", BINARY (Ty_bitv 64, ShrA))
     ; ("i64.shr_u", BINARY (Ty_bitv 64, ShrL))
     ; ("i64.rem", BINARY (Ty_bitv 64, Rem))
     ; ("i64.rem_u", BINARY (Ty_bitv 64, RemU))
     ; ("i64.eq", RELOP (Ty_bitv 64, Eq))
     ; ("i64.ne", RELOP (Ty_bitv 64, Ne))
     ; ("i64.lt_u", RELOP (Ty_bitv 64, LtU))
     ; ("i64.lt", RELOP (Ty_bitv 64, Lt))
     ; ("i64.le_u", RELOP (Ty_bitv 64, LeU))
     ; ("i64.le", RELOP (Ty_bitv 64, Le))
     ; ("i64.gt_u", RELOP (Ty_bitv 64, GtU))
     ; ("i64.gt", RELOP (Ty_bitv 64, Gt))
     ; ("i64.ge_u", RELOP (Ty_bitv 64, GeU))
     ; ("i64.ge", RELOP (Ty_bitv 64, Ge))
     ; ("i64.trunc_f32_s",  CVTOP (Ty_bitv 64, TruncSF32))
     ; ("i64.trunc_f32_u", CVTOP (Ty_bitv 64, TruncUF32))
     ; ("i64.trunc_f64_s", CVTOP (Ty_bitv 64, TruncSF64))
     ; ("i64.trunc_f64_u", CVTOP (Ty_bitv 64, TruncUF64))
     ; ("i64.reinterpret_float", CVTOP (Ty_bitv 64, Reinterpret_float))
     ; ("i64.extend_i32_s", CVTOP (Ty_bitv 64, ExtS 32))
     ; ("i64.extend_i32_u", CVTOP (Ty_bitv 64, ExtU 32))
     ; ("f32.neg", UNARY (Ty_fp 32, Neg))
     ; ("f32.abs", UNARY (Ty_fp 32, Abs))
     ; ("f32.sqrt", UNARY (Ty_fp 32, Sqrt))
     ; ("f32.nearest",UNARY (Ty_fp 32, Nearest) )
     ; ("f32.is_nan", UNARY (Ty_fp 32, Is_nan))
     ; ("f32.ceil", UNARY (Ty_fp 32, Ceil))
     ; ("f32.floor", UNARY (Ty_fp 32, Floor))
     ; ("f32.trunc", UNARY (Ty_fp 32, Trunc))
     ; ("f32.add", BINARY (Ty_fp 32, Add))
     ; ("f32.sub", BINARY (Ty_fp 32, Sub))
     ; ("f32.mul", BINARY (Ty_fp 32, Mul))
     ; ("f32.div", BINARY (Ty_fp 32, Div))
     ; ("f32.min", BINARY (Ty_fp 32, Min))
     ; ("f32.max", BINARY (Ty_fp 32, Max))
     ; ("f32.rem", BINARY (Ty_fp 32, Rem))
     ; ("f32.eq", RELOP (Ty_fp 32, Eq))
     ; ("f32.ne", RELOP (Ty_fp 32, Ne))
     ; ("f32.lt", RELOP (Ty_fp 32, Lt))
     ; ("f32.le", RELOP (Ty_fp 32, Le))
     ; ("f32.gt", RELOP (Ty_fp 32, Gt))
     ; ("f32.ge", RELOP (Ty_fp 32, Ge))
     ; ("f32.convert_i32_s", CVTOP (Ty_fp 32, ConvertSI32))
     ; ("f32.convert_i32_u", CVTOP (Ty_fp 32, ConvertUI32))
     ; ("f32.convert_i64_s", CVTOP (Ty_fp 32, ConvertSI32))
     ; ("f32.demote_f64", CVTOP (Ty_fp 32, DemoteF64))
     ; ("f32.reinterpret_int", CVTOP (Ty_fp 32, Reinterpret_int))
     ; ("f64.neg", UNARY (Ty_fp 64, Neg))
     ; ("f64.abs", UNARY (Ty_fp 64, Abs))
     ; ("f64.sqrt", UNARY (Ty_fp 64, Sqrt))
     ; ("f64.nearest",UNARY (Ty_fp 64, Nearest) )
     ; ("f64.is_nan", UNARY (Ty_fp 64, Is_nan))
     ; ("f64.ceil", UNARY (Ty_fp 32, Ceil))
     ; ("f64.floor", UNARY (Ty_fp 32, Floor))
     ; ("f64.trunc", UNARY (Ty_fp 32, Trunc))
     ; ("f64.add", BINARY (Ty_fp 64, Add))
     ; ("f64.sub", BINARY (Ty_fp 64, Sub))
     ; ("f64.mul", BINARY (Ty_fp 64, Mul))
     ; ("f64.div", BINARY (Ty_fp 64, Div))
     ; ("f64.min", BINARY (Ty_fp 64, Min))
     ; ("f64.max", BINARY (Ty_fp 64, Max))
     ; ("f64.rem", BINARY (Ty_fp 64, Rem))
     ; ("f64.eq", RELOP (Ty_fp 64, Eq))
     ; ("f64.ne", RELOP (Ty_fp 64, Ne))
     ; ("f64.lt", RELOP (Ty_fp 64, Lt))
     ; ("f64.le", RELOP (Ty_fp 64, Le))
     ; ("f64.gt", RELOP (Ty_fp 64, Gt))
     ; ("f64.ge", RELOP (Ty_fp 64, Ge))
     ; ("f64.convert_i32_s", CVTOP (Ty_fp 64, ConvertSI32))
     ; ("f64.convert_i32_u", CVTOP (Ty_fp 64, ConvertUI32))
     ; ("f64.convert_i64_s", CVTOP (Ty_fp 64, ConvertSI32))
     ; ("f64.promote_f32", CVTOP (Ty_fp 64, PromoteF32))
     ; ("f64.reinterpret_int", CVTOP (Ty_fp 64, Reinterpret_int))
     ; ("extract", EXTRACT)
     ; ("++", CONCAT)
     ; ("Ptr", PTR)
     ; ("assert", ASSERT)
     ; ("check-sat", CHECK_SAT)
     ; ("push", PUSH)
     ; ("pop", POP)
     ; ("let-const", LET_CONST)
     ; ("get-model", GET_MODEL)
     ; ("set-logic", SET_LOGIC)
     ; ("QF_BVFP", LOGIC QF_BVFP)
    |];
  tbl

let error msg = raise (SyntaxError msg)
}

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

let digit = ['0'-'9']
let character = ['a'-'z' 'A'-'Z']
let digits = '0' | [ '1'-'9' ] digit*
let numeral = '-'? digits
let decimal = '-'? digits '.' digit*
let hexadec = "#x" (['a'-'f' 'A'-'F'] | digit)+
let binary = "#b" ('0' | '1')+
let bool = "true" | "false"

let symbols = ['~''!''@''$''%''^''&''*''_''-''+''=''<''>''.''?''/']
let symbol = (character | symbols) (character | digit | symbols)*
(* TODO: Quoted symbols: |symbol| *)

rule token = parse
  | '(' { LPAREN }
  | ')' { RPAREN }

  | "nan" { DEC Float.nan }
  | numeral as s { NUM (int_of_string s) }
  | decimal as s { DEC (Float.of_string s) }
  | bool as s { BOOL (s = "true") }
  | hexadec { failwith "TODO: Lexer(hexadec)" }
  | binary { failwith "TODO: Lexer(binary)" }
  | '"' { string (Buffer.create 17) lexbuf }

  | symbol as x { try Hashtbl.find keywords x with Not_found -> SYMBOL x }

  | ';' { comment lexbuf }
  | white { token lexbuf }
  | newline { new_line lexbuf; token lexbuf }
  | eof { EOF }

  | _ { error ("Unexpected char: " ^ Lexing.lexeme lexbuf) }

and comment = parse
  | newline { new_line lexbuf; token lexbuf }
  | _ { comment lexbuf }

and string buf = parse
  | '"' { STR (Buffer.contents buf) }
  | '"' '"' { Buffer.add_char buf '"'; string buf lexbuf }
  | [^ '"']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf); string buf lexbuf }
  | eof { error "nonterminated string" }
  | _ { error ("illegal string char: " ^ Lexing.lexeme lexbuf) }

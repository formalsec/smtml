{
open Types
open Lexing
open Parser

exception SyntaxError of string

let keywords =
  let tbl = Hashtbl.create 256 in
  Array.iter
    (fun (k, v) -> Hashtbl.add tbl k v)
    [| ("int" , TYPE (`IntType))
     ; ("real", TYPE (`RealType))
     ; ("bool", TYPE (`BoolType))
     ; ("str" , TYPE (`StrType))
     ; ("i32" , TYPE (`I32Type))
     ; ("i64" , TYPE (`I64Type))
     ; ("f32" , TYPE (`F32Type))
     ; ("f64" , TYPE (`F64Type))
     ; ("int.neg", UNARY (Int I.Neg))
     ; ("int.add", BINARY (Int I.Add))
     ; ("int.sub", BINARY (Int I.Sub))
     ; ("int.div", BINARY (Int I.Div))
     ; ("int.mul", BINARY (Int I.Mul))
     ; ("int.shl", BINARY (Int I.Shl))
     ; ("int.shr_a", BINARY (Int I.ShrA))
     ; ("int.shr_u", BINARY (Int I.ShrL))
     ; ("int.pow", BINARY (Int I.Pow))
     ; ("int.and", BINARY (Int I.And))
     ; ("int.or", BINARY (Int I.Or))
     ; ("int.xor", BINARY (Int I.Xor))
     ; ("int.eq", RELOP (Int I.Eq))
     ; ("int.ne", RELOP (Int I.Ne))
     ; ("int.lt", RELOP (Int I.Lt))
     ; ("int.le", RELOP (Int I.Le))
     ; ("int.gt", RELOP (Int I.Gt))
     ; ("int.ge", RELOP (Int I.Ge))
     ; ("int.to_string", CVTOP (Int I.ToString))
     ; ("int.of_string", CVTOP (Int I.OfString))
     ; ("int.reinterpret_real", CVTOP (Int I.ReinterpretReal))
     ; ("real.neg", UNARY (Real R.Neg))
     ; ("real.abs", UNARY (Real R.Abs))
     ; ("real.sqrt", UNARY (Real R.Sqrt))
     ; ("real.nearest", UNARY (Real R.Nearest))
     ; ("real.is_nan", UNARY (Real R.IsNan))
     ; ("real.add", BINARY (Real R.Add))
     ; ("real.sub", BINARY (Real R.Sub))
     ; ("real.div", BINARY (Real R.Div))
     ; ("real.mul", BINARY (Real R.Mul))
     ; ("real.rem", BINARY (Real R.Rem))
     ; ("real.min", BINARY (Real R.Min))
     ; ("real.max", BINARY (Real R.Max))
     ; ("real.eq", RELOP (Real R.Eq))
     ; ("real.ne", RELOP (Real R.Ne))
     ; ("real.lt", RELOP (Real R.Lt))
     ; ("real.le", RELOP (Real R.Le))
     ; ("real.gt", RELOP (Real R.Gt))
     ; ("real.ge", RELOP (Real R.Ge))
     ; ("real.reinterpret_int", CVTOP (Real R.ReinterpretInt))
     ; ("real.to_string", CVTOP (Real R.ToString))
     ; ("real.of_string", CVTOP (Real R.OfString))
     ; ("bool.not", UNARY (Bool B.Not))
     ; ("bool.and", BINARY (Bool B.And))
     ; ("bool.or", BINARY (Bool B.Or))
     ; ("bool.xor", BINARY (Bool B.Xor))
     ; ("bool.eq", RELOP (Bool B.Eq))
     ; ("bool.ne", RELOP (Bool B.Ne))
     ; ("bool.ite", TERNARY (Bool B.ITE))
     ; ("str.len", UNARY (Str S.Len))
     ; ("str.nth", BINARY (Str S.Nth))
     ; ("str.++", BINARY (Str S.Concat))
     ; ("str.sub", TERNARY (Str S.SubStr))
     ; ("str.eq", RELOP (Str S.Eq))
     ; ("str.ne", RELOP (Str S.Ne))
     ; ("i32.clz", UNARY (I32 I32.Clz))
     ; ("i32.not", UNARY (I32 I32.Not))
     ; ("i32.add", BINARY (I32 I32.Add))
     ; ("i32.sub", BINARY (I32 I32.Sub))
     ; ("i32.div_s", BINARY (I32 I32.DivS))
     ; ("i32.div_u", BINARY (I32 I32.DivU))
     ; ("i32.and", BINARY (I32 I32.And))
     ; ("i32.or", BINARY (I32 I32.Or))
     ; ("i32.xor", BINARY (I32 I32.Xor))
     ; ("i32.mul", BINARY (I32 I32.Mul))
     ; ("i32.shl", BINARY (I32 I32.Shl))
     ; ("i32.shr_s", BINARY (I32 I32.ShrS))
     ; ("i32.shr_u", BINARY (I32 I32.ShrU))
     ; ("i32.rem_s", BINARY (I32 I32.RemS))
     ; ("i32.rem_u", BINARY (I32 I32.RemU))
     ; ("i32.eq", RELOP (I32 I32.Eq))
     ; ("i32.ne", RELOP (I32 I32.Ne))
     ; ("i32.lt_u", RELOP (I32 I32.LtU))
     ; ("i32.lt_s", RELOP (I32 I32.LtS))
     ; ("i32.le_u", RELOP (I32 I32.LeU))
     ; ("i32.le_s", RELOP (I32 I32.LeS))
     ; ("i32.gt_u", RELOP (I32 I32.GtU))
     ; ("i32.gt_s", RELOP (I32 I32.GtS))
     ; ("i32.ge_u", RELOP (I32 I32.GeU))
     ; ("i32.ge_s", RELOP (I32 I32.GeS))
     ; ("i32.ge_s", RELOP (I32 I32.GeS))
     ; ("i32.to_bool", CVTOP (I32 I32.ToBool))
     ; ("i32.of_bool", CVTOP (I32 I32.OfBool))
     ; ("i32.trunc_f32_s",  CVTOP (I32 I32.TruncSF32))
     ; ("i32.trunc_f32_u", CVTOP (I32 I32.TruncUF32))
     ; ("i32.trunc_f64_s", CVTOP (I32 I32.TruncSF64))
     ; ("i32.trunc_f64_u", CVTOP (I32 I32.TruncUF64))
     ; ("i32.reinterpret_f32", CVTOP (I32 I32.ReinterpretFloat))
     ; ("i32.wrap_i64", CVTOP (I32 I32.WrapI64))
     ; ("i64.clz", UNARY (I64 I64.Clz))
     ; ("i64.not", UNARY (I64 I64.Not))
     ; ("i64.add", BINARY (I64 I64.Add))
     ; ("i64.sub", BINARY (I64 I64.Sub))
     ; ("i64.div_s", BINARY (I64 I64.DivS))
     ; ("i64.div_u", BINARY (I64 I64.DivU))
     ; ("i64.and", BINARY (I64 I64.And))
     ; ("i64.or", BINARY (I64 I64.Or))
     ; ("i64.xor", BINARY (I64 I64.Xor))
     ; ("i64.mul", BINARY (I64 I64.Mul))
     ; ("i64.shl", BINARY (I64 I64.Shl))
     ; ("i64.shr_s", BINARY (I64 I64.ShrS))
     ; ("i64.shr_u", BINARY (I64 I64.ShrU))
     ; ("i64.rem_s", BINARY (I64 I64.RemS))
     ; ("i64.rem_u", BINARY (I64 I64.RemU))
     ; ("i64.eq", RELOP (I64 I64.Eq))
     ; ("i64.ne", RELOP (I64 I64.Ne))
     ; ("i64.lt_u", RELOP (I64 I64.LtU))
     ; ("i64.lt_s", RELOP (I64 I64.LtS))
     ; ("i64.le_u", RELOP (I64 I64.LeU))
     ; ("i64.le_s", RELOP (I64 I64.LeS))
     ; ("i64.gt_u", RELOP (I64 I64.GtU))
     ; ("i64.gt_s", RELOP (I64 I64.GtS))
     ; ("i64.ge_u", RELOP (I64 I64.GeU))
     ; ("i64.ge_s", RELOP (I64 I64.GeS))
     ; ("i64.ge_s", RELOP (I64 I64.GeS))
     ; ("i64.trunc_f32_s",  CVTOP (I64 I64.TruncSF32))
     ; ("i64.trunc_f32_u", CVTOP (I64 I64.TruncUF32))
     ; ("i64.trunc_f64_s", CVTOP (I64 I64.TruncSF64))
     ; ("i64.trunc_f64_u", CVTOP (I64 I64.TruncUF64))
     ; ("i64.reinterpret_f64", CVTOP (I64 I64.ReinterpretFloat))
     ; ("i64.extend_i32_s", CVTOP (I64 I64.ExtendSI32))
     ; ("i64.extend_i32_u", CVTOP (I64 I64.ExtendUI32))
     ; ("f32.neg", UNARY (F32 F32.Neg))
     ; ("f32.abs", UNARY (F32 F32.Abs))
     ; ("f32.sqrt", UNARY (F32 F32.Sqrt))
     ; ("f32.nearest",UNARY (F32 F32.Nearest) )
     ; ("f32.is_nan", UNARY (F32 F32.IsNan))
     ; ("f32.add", BINARY (F32 F32.Add))
     ; ("f32.sub", BINARY (F32 F32.Sub))
     ; ("f32.mul", BINARY (F32 F32.Mul))
     ; ("f32.div", BINARY (F32 F32.Div))
     ; ("f32.min", BINARY (F32 F32.Min))
     ; ("f32.max", BINARY (F32 F32.Max))
     ; ("f32.rem", BINARY (F32 F32.Rem))
     ; ("f32.eq", RELOP (F32 F32.Eq))
     ; ("f32.ne", RELOP (F32 F32.Ne))
     ; ("f32.lt", RELOP (F32 F32.Lt))
     ; ("f32.le", RELOP (F32 F32.Le))
     ; ("f32.gt", RELOP (F32 F32.Gt))
     ; ("f32.ge", RELOP (F32 F32.Ge))
     ; ("f32.convert_i32_s", CVTOP (F32 F32.ConvertSI32))
     ; ("f32.convert_i32_u", CVTOP (F32 F32.ConvertUI32))
     ; ("f32.convert_i64_s", CVTOP (F32 F32.ConvertSI32))
     ; ("f32.demote_f64", CVTOP (F32 F32.DemoteF64))
     ; ("f32.reinterpret_i32", CVTOP (F32 F32.ReinterpretInt))
     ; ("f64.neg", UNARY (F64 F64.Neg))
     ; ("f64.abs", UNARY (F64 F64.Abs))
     ; ("f64.sqrt", UNARY (F64 F64.Sqrt))
     ; ("f64.nearest",UNARY (F64 F64.Nearest) )
     ; ("f64.is_nan", UNARY (F64 F64.IsNan))
     ; ("f64.add", BINARY (F64 F64.Add))
     ; ("f64.sub", BINARY (F64 F64.Sub))
     ; ("f64.mul", BINARY (F64 F64.Mul))
     ; ("f64.div", BINARY (F64 F64.Div))
     ; ("f64.min", BINARY (F64 F64.Min))
     ; ("f64.max", BINARY (F64 F64.Max))
     ; ("f64.rem", BINARY (F64 F64.Rem))
     ; ("f64.eq", RELOP (F64 F64.Eq))
     ; ("f64.ne", RELOP (F64 F64.Ne))
     ; ("f64.lt", RELOP (F64 F64.Lt))
     ; ("f64.le", RELOP (F64 F64.Le))
     ; ("f64.gt", RELOP (F64 F64.Gt))
     ; ("f64.ge", RELOP (F64 F64.Ge))
     ; ("f64.convert_i32_s", CVTOP (F64 F64.ConvertSI32))
     ; ("f64.convert_i32_u", CVTOP (F64 F64.ConvertUI32))
     ; ("f64.convert_i64_s", CVTOP (F64 F64.ConvertSI32))
     ; ("f64.promote_f32", CVTOP (F64 F64.PromoteF32))
     ; ("f64.reinterpret_i64", CVTOP (F64 F64.ReinterpretInt))
     ; ("assert", ASSERT)
     ; ("check-sat", CHECK_SAT)
     ; ("declare-fun", DECLARE_FUN)
     ; ("get-model", GET_MODEL)
    |];
  tbl

let error msg = raise (SyntaxError msg)
}

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

let digit = ['0'-'9']
let character = ['a'-'z' 'A'-'Z']
let numeral = '0' | '-'? [ '1'-'9' ] digit*
let decimal = numeral '.' '0'* numeral
let hexadec = "#x" (['a'-'f' 'A'-'F'] | digit)+
let binary = "#b" ('0' | '1')+
let bool = "true" | "false"

let symbols = ['~''!''@''$''%''^''&''*''_''-''+''=''<''>''.''?''/']
let symbol = (character | symbols) (character | digit | symbols)*
(* TODO: Quoted symbols: |symbol| *)

rule token = parse
  | '(' { LPAREN }
  | ')' { RPAREN }

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
